library(tidyverse)
theme_set(theme_bw())
library(here)
library(arrow)
library(assertthat)
library(tmfast)
library(tictoc)

source(here('R', 'H.R'))

data_dir = here('data')

## Load data ----
tokens = open_dataset(here(data_dir, '03_annotations'))


## Construct doc-term matrices and calculate ndH ----
## Verbs
## ~2 sec
## Top results look okay; some garbage, difficult to attribute
# tic()
# verbs = filter(tokens, pos == 'VERB') |> 
#     count(comment_id, lemma)
# calculate_H(verbs, comment_id, lemma, n) |> 
#     arrange(desc(ndH))
# toc()

## Nouns
## ~4 sec
## Better than verbs, but still some garbage
# tic()
# nouns = filter(tokens, pos == 'NOUN') |> 
#     count(comment_id, lemma)
# calculate_H(nouns, comment_id, lemma, n) |> 
#     arrange(desc(ndH))
# toc()

## Adjective-noun bigrams
## Where the nouns are 'science' and 'health'
## These look great
nouns = tokens |> 
    filter(token %in% c('science', 'health'), 
           pos == 'NOUN') |> 
    select(comment_id, doc_id, sentence_id, token_id, lemma)
adjectives = tokens |> 
    filter(pos == 'ADJ', lemma != '-') |> 
    select(comment_id, doc_id, sentence_id, head_token_id, lemma) |> 
    mutate(head_token_id = as.integer(head_token_id))

adjectives |> 
    filter(is.na(head_token_id)) |> 
    collect() |> 
    nrow() |> 
    are_equal(0L) |> 
    assert_that(msg = 'Non-integer `head_token_id` in adjectives')

bigrams = inner_join(adjectives, nouns, 
           by = join_by(comment_id, doc_id, sentence_id, 
                        head_token_id == token_id)) |> 
    mutate(bigram = str_c(lemma.x, '_', lemma.y)) |> 
    count(comment_id, bigram)

## 13 sec
tic()
bigrams_h = bigrams |> 
    collect() |> 
    ndH(comment_id, bigram, n)
toc()


## EDA ----
## 630 science-health bigrams
## 332 with H > 0
nrow(bigrams_h)
bigrams_h |> 
    filter(H > 0) |> 
    nrow()

bigrams_h |> 
    filter(H > 0) |> 
    ggplot(aes(n, dH, color = ndH)) +
    geom_point(aes(label = bigram), alpha = .5) +
    scale_x_log10() +
    scale_color_viridis_c()
plotly::ggplotly()


## Vocabulary selection ----
## 332 adjective-noun science or health bigrams
vocab = bigrams_h |> 
    filter(H > 0) |>
    pull(bigram)

bigrams_fltd = bigrams |> 
    filter(bigram %in% vocab) |> 
    collect()

## 44,326 comment-bigram pairs
nrow(bigrams_fltd)
## 15,846 comments
n_distinct(bigrams_fltd$comment_id)

## 75% of comments have 6 or fewer bigrams; 
## max has 1.5k
bigrams_fltd |> 
    group_by(comment_id) |> 
    summarize(n = sum(n)) |> 
    pull(n) |> 
    summary()

## Write output ----
write_rds(bigrams_h, here(data_dir, '05_bigrams_h.Rds'))
write_parquet(bigrams_fltd, here(data_dir, '05_bigrams.parquet'))
