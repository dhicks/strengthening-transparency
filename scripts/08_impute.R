library(tidyverse)

library(readxl)
library(here)
library(tictoc)
library(assertthat)
library(arrow)
library(glue)

library(tidymodels)
library(textrecipes)
library(themis)

data_dir = here('data')

## Load data ----
## Form letters
form_letters = read_rds(here(data_dir, '06_form_letters.Rds'))

## Manual coding results
coded_df = read_excel(here(data_dir, '06_docs_2024-01-16_DJH.xlsx')) |> 
    filter(!is.na(support)) |> 
    mutate(support = case_when(
        support == 'n' ~ 'oppose', 
        support == 'y' ~ 'support', 
        support == 'NA' ~ NA_character_,
        TRUE ~ NA_character_), 
        support = as_factor(support))

coded_df |> 
    count(support)

# coded_df |> 
#     pull(support) |> 
#     is.na() |> 
#     any() |> 
#     magrittr::not() |> 
#     assert_that(msg = 'Unexpected values in `coded_df`')

comments = coded_df |> 
    pull(comment_id)

identical(n_distinct(comments), length(comments)) |> 
    assert_that()

message(glue('{length(comments)} manually coded documents'))

# ## Document-term matrix on full corpus
# ## NB already tokenized and lemmatized
# dtm_corpus = open_dataset(here(data_dir, '03_annotations')) |> 
#     filter(pos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) |> 
#     count(comment_id, lemma)
# 
# dtm_sample = dtm_corpus |> 
#     filter(comment_id %in% c(comments, form_letters)) |> 
#     left_join(coded_df, by = 'comment_id') |> 
#     mutate(support = if_else(comment_id %in% form_letters, 
#                              'oppose', 
#                              support)) |> 
#     select(comment_id, support, lemma, n) |> 
#     collect()
# 
# ## sample: 9688 docs
# n_distinct(dtm_sample$comment_id)

## Document text
get_text = function(...) {
    open_dataset(here(data_dir, '03_text.parquet')) |> 
        filter(...) |> 
        collect() |> 
        group_by(comment_id) |> 
        summarize(text = str_flatten(text, collapse = '\\n')) |> 
        ungroup()
}
# get_text()
# get_text(comment_id %in% comments)

text_sample = get_text(comment_id %in% c(comments, form_letters)) |> 
    left_join(coded_df, by = 'comment_id') |> 
    mutate(support = if_else(comment_id %in% form_letters,
                             'oppose',
                             support)) |>
    filter(!is.na(support)) |> 
    mutate(support = as_factor(support)) |> 
    select(comment_id, support, text = text.x)

assert_that(identical(
    n_distinct(text_sample$comment_id), 
    nrow(text_sample)))

## 9,650 docs in sample
nrow(text_sample)

## 98.4% oppose rule
count(text_sample, support) |> 
    mutate(share = n / sum(n))


## Train and test sets ----
## 9650 -> 7237 + 2413
set.seed(2024-01-26)
splits = initial_split(text_sample, prop = 3/4, strata = support)

train = training(splits)
test = testing(splits)

## Check sizes and overlap
train |> 
    pull(comment_id) |> 
    n_distinct()
test |> 
    pull(comment_id) |> 
    n_distinct()

train |> 
    count(comment_id, support) |> 
    count(support) |> 
    mutate(share = n / sum(n))
test |> 
    count(comment_id, support) |> 
    count(support) |> 
    mutate(share = n / sum(n))

intersect(train$comment_id, test$comment_id) |> 
    length() |> 
    identical(0L) |> 
    assert_that(msg = 'Comment overlap in train and test sets')


## Resampling ----
## 7237 -> 5789/90 + 1448/47
set.seed(2024-01-12)
folds = vfold_cv(train, strata = support, v = 5)


## Preprocessing ----
binary_hash <- function(x) {
    x <- ifelse(x < 0, -1, x)
    x <- ifelse(x > 0,  1, x)
    x
}

preprocessing = recipe(support ~ comment_id + text, data = train) |> 
    ## comment_id is ID rather than predictor
    update_role(comment_id, new_role = 'ID') |> 
    ## Trim long comments to first ~1k words
    step_mutate(text = str_trunc(as.character(text), 
                                 6*500, ellipsis = '')) |>
    ## Tokenize
    step_tokenize(text) |>
    step_stopwords(text) |>
    step_stem(text) |>
    ## Construct hashed columns
    ## More hashed columns improve predictions, esp. specificity (support)
    ## But fitting is significantly slower
    ## Be sure verbose = TRUE in train_grid() to monitor progress
    step_texthash(text, signed = TRUE, num_terms = 2^10) |>
    step_rename_at(starts_with('texthash'),
                   fn = ~ str_replace(., 'texthash_text_', 'hash_')) |>
    step_mutate_at(starts_with('hash'), fn = binary_hash) |>
    ## Trim zero variance predictors
    step_zv(all_predictors()) |>
    ## Upsampling (since there's very little support)
    step_upsample(support, seed = 2024-02-06, skip = TRUE)

## Check timing for preprocessing
# {
#     tic()
#     precipe = prep(preprocessing, train)
#     # bake(precipe, new_data = NULL)
#     bake(precipe, new_data = train)
#     toc()
#     }


## Define models and assemble workflows ----
rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
    set_engine('ranger', num.threads = future::availableCores() - 1) |> 
    set_mode('classification')

rf_workflow = workflow() |> 
    add_model(rf_model) |> 
    add_recipe(preprocessing)


lasso_model = logistic_reg(penalty = tune(), mixture = 1) |> 
    set_mode('classification') |> 
    set_engine('glmnet')

## Based off <https://smltar.com/mlclassification#casestudysparseencoding>
## But isn't any faster
# sparse_blueprint = hardhat::default_recipe_blueprint(composition = "dgCMatrix")

lasso_workflow = workflow() |> 
    add_model(lasso_model) |> 
    # add_recipe(preprocessing, blueprint = sparse_blueprint)
    add_recipe(preprocessing)


## Tuning ----
## n_folds x grid_size x time_per_model
## eg, 5 x 10 x 90 sec -> 75 min
{
    tic()
    set.seed(2024-02-09)
    ## RF fits VERY slowly, even w/ parallelization
    # rf_tuning = rf_workflow |> 
    #     tune_grid(resamples = folds, 
    #               grid = 10, 
    #               control = control_grid(verbose = TRUE, 
    #                                      save_pred = TRUE),
    #               metrics = metric_set(bal_accuracy,
    #                                    accuracy, 
    #                                    sensitivity, 
    #                                    specificity))
    ## lasso also gets significantly better specificity
    lasso_tuning = lasso_workflow |> 
        tune_grid(resamples = folds, 
                  grid = grid_regular(penalty(), levels = 30), 
                  control = control_grid(verbose = TRUE, 
                                         save_pred = TRUE, 
                                         save_workflow = TRUE), 
                  metrics = metric_set(bal_accuracy, 
                                       accuracy, 
                                       sensitivity, 
                                       specificity))
    toc()
}

## Tuning results for lasso look really nice
autoplot(lasso_tuning)

collect_notes(lasso_tuning)

collect_metrics(lasso_tuning)

## Best for balanced accuracy should also be best for specificity
show_best(lasso_tuning, metric = 'bal_accuracy')
show_best(lasso_tuning, metric = 'specificity')

select_best(lasso_tuning, metric = 'bal_accuracy')


## Finalize ----
## - finalize the workflow w/ best hyperparameters
# lasso_final = finalize_workflow(lasso_workflow, 
#                                select_best(lasso_tuning, 
#                                            metric = 'bal_accuracy'))

## - fit finalized workflow on full train data
lasso_model = fit_best(lasso_tuning, 
                       metric = 'bal_accuracy', 
                       verbose = TRUE)

## - assess on test data
augment(lasso_model, new_data = test) |> 
    bal_accuracy(support, .pred_class)
augment(lasso_model, new_data = test) |> 
    specificity(support, .pred_class)
# augment(lasso_model, new_data = test) |> 
#     filter(support == 'support') |> 
#     view()

## - next script: generate predictions for full corpus
