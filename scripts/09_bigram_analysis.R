library(tidyverse)
theme_set(theme_minimal())
library(ggforce)
library(patchwork)
library(here)
library(arrow)

extract_noun = function(df, in_col = bigram, out_col = noun) {
    mutate(df, {{ out_col }} := str_split_i({{ in_col }}, '_', -1))
}

## TODO: write out plots throughout

## Load data ----
data_dir = here('data')
source(here('R', 'load_coding.R'))

coded = list(manual = load_manual_coding(), 
             filtered = load_imputed_fltd(), 
             imputed = load_imputed_full())

map(coded, nrow)

## Number of docs by support
## Here we only want number of docs *w/ bigrams*
# count_docs = function(coded_df) {
#     count(coded_df, support, name = 'n_docs')
# }
# # n_docs = count_docs(coding)
# n_docs = map(coded, count_docs)
# 
# mapped(n_docs, ~ mutate(.x, share = n_docs / sum(n_docs)))


## Load bigrams ----
load_bigrams = function(coded_df) {
    open_dataset(here(data_dir, 
                      '05_bigrams.parquet')) |> 
        inner_join(coded_df, by = 'comment_id') |> 
        filter(support %in% c('support', 'oppose')) |>
        collect() |>
        extract_noun()
}

bigrams = map(coded, load_bigrams)

bigrams |> 
    mapped(~ {.x |> 
            pull(comment_id) |> 
            n_distinct()})

n_docs = map(bigrams, ~ {.x |> 
        distinct(comment_id, support) |> 
        count(support, name = 'n_docs')})

## Noun analysis ----
## Pr(bigram is "science" | doc support)
## TODO: medians
noun_plot = function(bigrams_df, title) {
    bigrams_df |> 
        count(support, comment_id, noun) |> 
        complete(support, comment_id, noun, fill = list(n = 0L)) |> 
        group_by(comment_id) |> 
        mutate(share = n / sum(n)) |> 
        ungroup() |> 
        filter(noun == 'health') |>
        ggplot(aes(support, share, color = support)) +
        geom_violin(scale = 'width') +
        geom_sina(scale = 'width', alpha = .3) +
        scale_y_continuous(name = 'share of bigrams that are health',
                           labels = scales::percent_format()) +
        ggtitle(title)
}
# bigrams[['manual']] |>
#     noun_plot('test')

imap(bigrams, noun_plot) |> 
    wrap_plots(nrow = 1, guides = 'collect')

## Pr(doc contains health/sci bigram | support)
noun_occurrence = function(bigram_df, doc_count_df) {
    bigram_df |> 
        distinct(support, comment_id, noun) |> 
        count(support, noun) |> 
        left_join(doc_count_df, by = 'support') |> 
        mutate(p = n / n_docs)
}
# debugonce(noun_occurrence)
# noun_occurrence(bigrams$manual, n_docs$manual)

mapped2(bigrams, n_docs, noun_occurrence) |> 
    ggplot(aes(support, p, color = noun)) +
    geom_point(aes(shape = coding), 
               size = 2, 
               position = position_dodge(width = .2)) +
    # facet_wrap(vars(coding)) +
    # scale_x_discrete(position = 'top') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'occurrence of bigrams by noun')

## Bigram analysis ----
## Pr(doc contains bigram | support)
bigram_occurrence = function(bigram_df, doc_count_df) {
    bigram_df |> 
        ## NB one-hot encoding / doc frequency
        distinct(support, comment_id, bigram) |> 
        count(support, bigram, name = 'df') |> 
        left_join(doc_count_df, by = 'support') |> 
        mutate(p = df / n_docs)
}
# bigram_occurrence(bigrams$manual, n_docs$manual)
p_df = map2(bigrams, n_docs, bigram_occurrence)

bigram_top_n = function(pr_bigram, n = 10) {
    pr_bigram |> 
        group_by(support) |> 
        top_n(n, wt = p) |> 
        arrange(support, desc(p)) |> 
        ungroup() |>
        print(n = 2 * n)
}
## TODO: readable table
# bigram_top_n(p_df, 10)
mapped(p_df, bigram_top_n)

## TODO: fix this? 
# ggplot(p_df, aes(fct_reorder(bigram, p, .desc = FALSE),
#                  p, color = support)) +
#     geom_point() +
#     coord_flip() +
#     facet_wrap(vars(coding))

## Likelihood ratio ----
log1kp = function(x, k = 5) {
    log10(x + 10^-k)
}

source(here('R', 'top_and_bottom.R'))

llr = function(pr_bigram) {
    pr_bigram |> 
        pivot_wider(id_cols = bigram, 
                    names_from = support,
                    values_from = p, 
                    values_fill = 0) |> 
        mutate(llr = log1kp(oppose) - log1kp(support))
}
p_df |> 
    map(llr) |> 
    map(~ top_and_bottom(.x, llr, 10))

## Top and bottom plot
llr_plot = function(llr_df, title, n = 15) {
    llr_df |> 
        top_and_bottom(llr, n) |> 
        mutate(bigram = fct_reorder(bigram, llr), 
               side = fct_inorder(side)) |> 
        extract_noun() |> 
        ggplot(aes(bigram, ymax = llr, color = noun)) +
        geom_linerange(ymin = 0) +
        coord_flip() +
        facet_wrap(vars(side), ncol = 1, scales = 'free_y') +
        scale_color_brewer(palette = 'Set1') +
        ggtitle(title)
}
p_df |> 
    map(llr) |> 
    imap(llr_plot) |> 
    wrap_plots(nrow = 1, guides = 'collect')

# opts = options(pillar.sigfig = 10)
## For manual, medians are identical bc the median bigram appears in 1 opposing doc and no supporting docs
p_df |> 
    map(llr) |> 
    mapped(~ {.x |> 
            extract_noun() |> 
            group_by(noun) |> 
            summarize(llr = median(llr))})
p_df$manual |> 
    complete(support, bigram, fill = list(df = 0)) |> 
    group_by(support) |> 
    summarize(df = median(df))
log1kp(1/622) - log1kp(0)
# options(opts)

llr_by_noun_plot = function(llr_df, title) {
    llr_df |> 
        extract_noun() |> 
        mutate(bigram = fct_reorder(bigram, llr)) |> 
        ggplot(aes(noun, y = llr, color = noun)) +
        geom_violin(scale = 'width') +
        geom_sina(aes(label = bigram), scale = 'width') +
        # stat_summary(fun = median,
        #              geom = 'crossbar', 
        #              color = 'black') +
        geom_hline(yintercept = 0) +
        scale_color_brewer(palette = 'Set1', 
                           guide = 'none') +
        labs(y = 'log likelihood ratio\n←support                                 oppose→') +
        ggtitle(title)
}

p_df |> 
    map(llr) |> 
    imap(llr_by_noun_plot) |> 
    wrap_plots(nrow = 1, guides = 'collect') +
    plot_layout(axis_titles = 'collect')

## TODO: feed in the combined plot
plotly::ggplotly()


## KWIC ----
source(here('R', 'kwic.R'))
text_ar = open_dataset(here('data', '03_text.parquet'))

## "very science" comes from "attacking the very science on which its regulations are"
kwic(text_ar, 'very science', window = 30)

## "less science" shows up where comments are concerned about reduced use of science in regulation
kwic(text_ar, 'less science')

## "well science" doesn't appear in corpus? 
kwic(text_ar, 'well science')

## "your own health," "my own health," "their own health"
kwic(text_ar, ' own health')

