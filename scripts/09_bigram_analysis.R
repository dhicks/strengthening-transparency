library(tidyverse)
library(here)
library(arrow)

## Load data ----
data_dir = here('data')

coding = read_csv(here(data_dir, '07_coding.csv')) |> 
    mutate(support = case_when(
        support.x == 'y' ~ 'support', 
        support.x == 'n' ~ 'oppose', 
        TRUE ~ NA_character_
    ))

# TODO: coded docs that aren't in bigrams anymore? 
nrow(coding)


bigrams = open_dataset(here(data_dir, '05_bigrams.parquet')) |> 
    inner_join(coding, by = 'comment_id') |> 
    filter(support %in% c('support', 'oppose')) |>
    collect() |> 
    mutate(noun = str_split_i(bigram, '_', -1))

bigrams |> 
    pull(comment_id) |> 
    n_distinct()

## EDA ----
## Number of docs by support
n_docs = count(coding, support, name = 'n_docs')
mutate(n_docs, share = n_docs / sum(n_docs))

## What fraction of bigrams are "science," by support category? 
## Pr(bigram is "science" | support)
bigrams |> 
    group_by(support, comment_id) |> 
    count(support, comment_id, noun) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    filter(noun == 'science') |> 
    ggplot(aes(share, color = support)) +
    geom_density()

## Pr(doc contains bigram | support)
p_df = bigrams |> 
    distinct(support, comment_id, bigram) |> 
    count(support, bigram) |> 
    left_join(n_docs, by = 'support') |> 
    mutate(p = n / n_docs)

p_df |> 
    group_by(support) |> 
    top_n(10, wt = p) |> 
    arrange(support, desc(p)) |> 
    print(n = 20)

# ggplot(p_df, aes(fct_reorder(bigram, p, .desc = FALSE), 
#                  p, color = support)) +
#     geom_point() +
#     coord_flip()

## Log ratio
TODO: top-and-bottom
TODO: HTML in bigrams

log1kp = function(x, k = 5) {
    log10(x + 10^-k)
}
p_df |> 
    pivot_wider(id_cols = bigram, names_from = support, values_from = p, 
                values_fill = 0) |> 
    mutate(lr = log1kp(oppose) - log1kp(support)) |> 
    arrange(desc(lr)) |>
    # arrange(lr) |>
    print(n = 20) |> view()
