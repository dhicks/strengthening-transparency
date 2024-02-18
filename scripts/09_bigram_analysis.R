library(tidyverse)
theme_set(theme_minimal())
library(ggforce)
library(here)
library(arrow)

extract_noun = function(df, in_col = bigram, out_col = noun) {
    mutate(df, {{ out_col }} := str_split_i({{ in_col }}, '_', -1))
}

## Load data ----
data_dir = here('data')

coding = read_rds(here(data_dir, '07_coding.Rds')) |> 
    select(-starts_with('support'), 
           -starts_with('notes')) |> 
    mutate(support = case_when(
        mode == 'y' ~ 'support', 
        mode == 'n' ~ 'oppose', 
        TRUE ~ NA_character_
    )) |> 
    select(-mode)

nrow(coding)

## Number of docs by support
count_docs = function(coded_df) {
    count(coded_df, support, name = 'n_docs')
}
n_docs = count_docs(coding)
mutate(n_docs, share = n_docs / sum(n_docs))

## Load bigrams ----
load_bigrams = function(coded_df) {
    open_dataset(here(data_dir, 
                      '05_bigrams.parquet')) |> 
        inner_join(coded_df, by = 'comment_id') |> 
        filter(support %in% c('support', 'oppose')) |>
        collect() |> 
        extract_noun()
}

bigrams = load_bigrams(coding)

bigrams |> 
    pull(comment_id) |> 
    n_distinct()

## Noun analysis ----
## Pr(bigram is "science" | doc support)
noun_plot = function(bigrams_df) {
    bigrams_df |> 
        group_by(support, comment_id) |> 
        count(support, comment_id, noun) |> 
        mutate(share = n / sum(n)) |> 
        ungroup() |> 
        filter(noun == 'science') |> 
        ggplot(aes(support, share, color = support)) +
        geom_violin() +
        geom_sina() +
        scale_y_continuous(name = 'share of science bigrams', 
                           labels = scales::percent_format())
}

noun_plot(bigrams)

## Pr(doc contains health/sci bigram | support)
noun_occurrence = function(bigram_df, doc_count_df) {
    bigram_df |> 
        distinct(support, comment_id, noun) |> 
        count(support, noun) |> 
        left_join(doc_count_df, by = 'support') |> 
        mutate(p = n / n_docs)
}

noun_occurrence(bigrams, n_docs)

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
p_df = bigram_occurrence(bigrams, n_docs)

bigram_top_n = function(pr_bigram, n = 10) {
    pr_bigram |> 
        group_by(support) |> 
        top_n(n, wt = p) |> 
        arrange(support, desc(p)) |> 
        ungroup() |>
        print(n = 2 * n)
}
bigram_top_n(p_df, 10)

# ggplot(p_df, aes(fct_reorder(bigram, p, .desc = FALSE), 
#                  p, color = support)) +
#     geom_point() +
#     coord_flip()

## Likelihood ratio ----
log1kp = function(x, k = 5) {
    log10(x + 10^-k)
}

source(here('R', 'top_and_bottom.R'))

llr = function(pr_bigram) {
    pr_bigram |> 
        pivot_wider(id_cols = bigram, names_from = support,
                    values_from = p, 
                    values_fill = 0) |> 
        mutate(llr = log1kp(oppose) - log1kp(support))
}
p_df |> 
    llr() |> 
    top_and_bottom(llr, 10)
    
## Top and bottom plot
llr_plot = function(llr_df, n = 15) {
    llr_df |> 
        top_and_bottom(llr, n) |> 
        mutate(bigram = fct_reorder(bigram, llr), 
               side = fct_inorder(side)) |> 
        extract_noun() |> 
        ggplot(aes(bigram, ymax = llr, color = noun)) +
        geom_linerange(ymin = 0) +
        coord_flip() +
        facet_wrap(vars(side), ncol = 1, scales = 'free_y') +
        scale_color_brewer(palette = 'Set1')
}
p_df |> 
    llr() |> 
    llr_plot()

llr_by_noun_plot = function(llr_df) {
    llr_df |> 
        extract_noun() |> 
        mutate(bigram = fct_reorder(bigram, llr)) |> 
        ggplot(aes(noun, y = llr, color = noun)) +
        geom_violin() +
        geom_sina(aes(label = bigram)) +
        geom_hline(yintercept = 0) +
        scale_color_brewer(palette = 'Set1') +
        labs(y = 'log likelihood ratio\n←opposition                                 support→')
}

p_df |> 
    llr() |> 
    llr_by_noun_plot()
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

