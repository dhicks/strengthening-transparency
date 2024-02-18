## Principal components / latent semantic analysis of science and health bigrams
library(tidyverse)
theme_set(theme_minimal())
library(openxlsx2)

library(tidytext)
library(broom) # PCA tidiers
library(irlba)

library(arrow)
library(assertthat)
library(here)
library(tictoc)
library(xfun) # for strip_html()

source(here('R', 'build_url.R'))

data_dir = here('data')
output_dir = here('out')

sample_n = 200 # how many docs to draw from top / bottom
set.seed(2024-01-12)

## Load data ----
text = open_dataset(here(data_dir, '03_text.parquet'))
dtm = open_dataset(here(data_dir, '05_bigrams.parquet'))

## Exclude form letters ----
## 2,427 comments are nearly identical, and seem to be based on a form letter campaign by the Sierra Club
## See <https://angeles.sierraclub.org/news/blog/2018/05/stop_pruitts_quest_help_industrial_polluters>
## We leave these in for fitting the model, but exclude them from manual coding to save time (they're negative)
sierra_club = here(data_dir, '03_text.parquet') |> 
    open_dataset() |> 
    filter(str_detect(text, 
                      fixed('Medical records and health data are largely kept confidential to protect patients', 
                            ignore_case = TRUE)) |
               str_detect(text, 
                          fixed('Medical science has repeatedly proven that smog', 
                                ignore_case = TRUE))) |> 
    pull(comment_id, as_vector = TRUE)

## And another 4,523 nearly identical comments
form2 = here(data_dir, '03_text.parquet') |> 
    open_dataset() |> 
    filter(str_detect(text, 
                      fixed('As someone who believes that our air, water, and food should be protected', 
                            ignore_case = TRUE))) |> 
    pull(comment_id, as_vector = TRUE)

## 1,925
form3 = here(data_dir, '03_text.parquet') |> 
    open_dataset() |> 
    filter(str_detect(text, 
                      fixed('I am writing to strongly oppose the Environmental Protection Agency', 
                            ignore_case = TRUE))) |> 
    pull(comment_id, as_vector = TRUE)

## Total 8,875 form comments to exclude
form_letters = c(sierra_club, form2, form3)

write_rds(form_letters, here(data_dir, '06_form_letters.Rds'))

## Pre-processing ----
## How many terms? 
## 332 adjective-noun bigrams w/ noun == health or science
dtm |> 
    pull(bigram) |> 
    n_distinct()

## How many comments? 
## 15,846, out of: 
##   17,809 (89%) that use "science" or "health" as a noun 
##   22,390 (71%) in the whole corpus
dtm |> 
    pull(comment_id) |> 
    n_distinct()


## Principal components ----
## We're trying to cluster *comments*, not terms, 
## so comments are columns, rather than rows
## ~4 sec
tic()
pc = dtm |> 
    collect() |> 
    cast_sparse(bigram, comment_id, n) |> 
    prcomp_irlba(n = 5, center = TRUE, scale. = TRUE)
toc()
## prcomp_irlba loses the row names (bigrams)
vocab = dtm |> 
    collect() |> 
    cast_sparse(bigram, comment_id, n) |> 
    rownames()


## Write out PC model ----
write_rds(pc, here(data_dir, '06_pc.Rds'))


## Scree plot ----
## 1 PC explains almost 60% of the variance
## 5 explain about 80%
scree_cum = tidy(pc, matrix = 'd') |> 
    ggplot(aes(PC, cumulative)) +
    geom_point() +
    geom_line(group = 1L) +
    ylim(0, 1) +
    labs(y = 'cumulative variance explained')
scree_cum
ggsave(here(output_dir, '06_scree_cum.png'), scree_cum)

scree = tidy(pc, matrix = 'd') |> 
    ggplot(aes(PC, std.dev^2)) +
    geom_point() +
    geom_line(group = 1L)
scree
ggsave(here(output_dir, '06_scree.png'), scree)


## Crosswalks ---
## Rows are bigrams
row_xwalk = tibble(bigram = vocab) |> 
    mutate(row = row_number())
## Columns are comments
col_xwalk = tibble(comment_id = names(pc$center)) |> 
    mutate(column = row_number())


## Biplots, PC 1 and 2 ----
## Very large values make this difficult to interpret
pc |>
    tidy() |>
    filter(PC %in% c(1L, 2L)) |>
    pivot_wider(names_from = PC, names_prefix = 'pc',
                id_cols = row,
                values_from = value) |>
    left_join(row_xwalk) |> 
    mutate(noun = {bigram |>
            str_extract('_.*') |>
            str_remove('^_')}) |>
    ggplot(aes(pc1, pc2, 
               color = noun)) +
    geom_point(aes(label = bigram)) +
    scale_color_brewer(palette = 'Set1')
# biplot
# plotly::ggplotly()


## Top and bottom terms for PC 1 ----
top_and_bottom = function(dataf, value_var, n) {
    top = top_n(dataf, n, wt = {{ value_var }}) |> 
        mutate(side = 'top')
    bottom = top_n(dataf, -n, wt = {{ value_var }}) |> 
        mutate(side = 'bottom')
    
    bind_rows(top, bottom) |>
        arrange(desc({{ value_var}} ), .by_group = TRUE)
}

## Validate top_and_bottom()
assert_that(
    {
        all.equal(
            ## Directly pull out top 20 values
            {pc |> 
                    tidy() |> 
                    filter(PC == 1L) |> 
                    arrange(desc(value)) |> 
                    head(50) |> 
                    pull(value)
            }, 
            ## Output of top_and_bottom
            {pc |> 
                    tidy() |> 
                    filter(PC == 1L) |> 
                    top_and_bottom(value, 50) |> 
                    head(50) |> 
                    pull(value)
            }
        )
    }
)

bigram_tbl = pc |> 
    tidy() |> 
    filter(PC == 1L) |> 
    top_and_bottom(value, 20) |> 
    left_join(row_xwalk) |> 
    select(side, bigram, score = value) |> 
    mutate(bigram = str_replace(bigram, '_', ' '))
# view(bigram_tbl)

bigram_tbl |> 
    knitr::kable(format = 'markdown', 
                 digits = 2, 
                 caption = 'Top and bottom 20 bigrams, first principal component.') |> 
    write_lines(here(output_dir, '06_top_bottom.md'))

tb_plot = pc |> 
    tidy() |> 
    filter(PC == 1L) |> 
    top_and_bottom(value, 20) |> 
    left_join(row_xwalk) |> 
    mutate(bigram = str_replace(bigram, '_', ' '),
           term = fct_rev(fct_inorder(bigram)), 
           value_trans = sqrt(abs(value)) * sign(value), 
           noun = case_when(str_detect(bigram, ' science') ~ 'science', 
                            str_detect(bigram, ' health') ~ 'health', 
                            TRUE ~ NA_character_)) |> 
    ggplot(aes(term, value_trans, color = noun)) +
    # geom_point(aes(size = df)) +
    geom_segment(aes(xend = term, yend = 0)) +
    scale_color_brewer(palette = 'Set1') +
    scale_size(trans = 'log10') +
    coord_flip() +
    facet_grid(rows = vars(side), scales = 'free_y', 
               as.table = FALSE) +
    theme_bw() +
    labs(x = 'bigram', 
         y = 'score (square-root scale)', 
         size = 'document frequency', 
         title = 'Top and bottom terms', 
         subtitle = 'First principal component')
tb_plot
ggsave(here(output_dir, '06_top_bottom.png'), 
       height = 6, width = 6, scale = 1.5)
# plotly::ggplotly()


## Extreme documents along the first PC ----
## Top and bottom 200 documents 
## (actually returns quite a few more on one side due to ties)
docs_of_interest_tb = pc |>
    tidy(matrix = 'v') |>
    filter(PC == 1L) |> 
    left_join(col_xwalk) |> 
    filter(!comment_id %in% form_letters) |>
    top_and_bottom(value, sample_n)

count(docs_of_interest_tb, side)

set.seed(2024-01-11)
docs_of_interest_ran = pc |> 
    tidy(matrix = 'v') |> 
    filter(PC == 1L) |> 
    left_join(col_xwalk) |> 
    filter(!comment_id %in% form_letters, 
           !comment_id %in% docs_of_interest_tb$comment_id) |> 
    slice_sample(n = 2*sample_n) |> 
    mutate(side = 'random')

docs_of_interest = bind_rows(docs_of_interest_tb, 
                             docs_of_interest_ran) |> 
    distinct(comment_id, .keep_all = TRUE)

count(docs_of_interest, side)

pc |> 
    tidy(matrix = 'v') |> 
    filter(PC == 1L) |> 
    left_join(col_xwalk) |> 
    mutate(form_letters = comment_id %in% form_letters) |> 
    ggplot(aes(value)) +
    geom_density() +
    geom_point(data = ~ filter(.x, form_letters), 
               aes(color = 'form letters'),
               y = 0,
               shape = '|', size = 2, alpha = .5) +
    geom_point(data = filter(docs_of_interest, 
                             side != 'random'), 
               aes(color = side),
               y = 10, 
               shape = '|', size = 2, alpha = .5) +
    geom_point(data = filter(docs_of_interest, 
                             side == 'random'), 
               aes(color = side), 
               y = 20,
               shape = '|', size = 2, alpha = .5) +
    geom_point(y = -10, color = 'black', alpha = .01,
               shape = '|', size = 2) +
    scale_color_brewer(palette = 'Set1', 
                       guide = guide_legend(reverse = TRUE)) +
    labs(color = 'document set', 
         x = 'loading')

ggsave(here(output_dir, '06_loading_distribution.png'), 
       height = 4, width = 8, bg = 'white')


## Excel sheets for coding ----
set.seed(2024-01-12)
out_df = docs_of_interest |> 
    select(comment_id) |> 
    ## Add ~150 words of text from each document
    left_join(text, by = 'comment_id', copy = TRUE) |> 
    select(comment_id, doc_id, text) |> 
    mutate(text = str_trunc(text, 150*5)) |> 
    group_by(comment_id) |> 
    summarize(text = str_c(text, collapse = '\n\n')) |> 
    ungroup() |> 
    ## Shuffle rows
    sample_frac() |> 
    mutate(url = build_url(comment_id)) |> 
    select(comment_id, url, text) |> 
    ## Space for coding
    mutate(support = '', 
           notes = '')

## openxlsx is rather clunky but we can do all the formatting here
wb_workbook() |> 
    wb_add_worksheet() |> 
    wb_add_data_table(x = {out_df |> 
            mutate(url = magrittr::set_class(url, 'hyperlink'), 
                   text = strip_html(text)) |> 
            as.data.frame()}) |> 
    wb_set_sheetview(zoom_scale = 180) |>
    wb_set_col_widths(cols = 1:5, widths = c(25, 10, 70, 10, 25)) |>
    wb_add_cell_style(dims = glue('A1:E{nrow(out_df)}'), 
                      vertical = 'top', wrap_text = TRUE) |> 
    wb_save(file = here(output_dir, glue('06_docs_{today()}.xlsx')), 
            overwrite = FALSE)

