library(tidyverse)
theme_set(theme_bw())

source('../R/H.R')

library(tictoc)

data_folder = '../data/'
prefix = '04_'

## Load data ----
## ~20 sec
message('Loading tokens; ~20 sec')
tic()
tokens = read_rds(str_c(data_folder, '03_tokens.Rds'))
toc()

# tokens_sm = head(tokens, 1e4)


## Construct doc-term matrices and calculate ndH ----
## Verbs
## ~2 sec
## Top results look okay; some garbage, difficult to attribute
# tic()
# verbs = filter(tokens, upos == 'VERB') %>% 
#     count(comment_id, lemma)
# calculate_H(verbs, comment_id, lemma, n) %>% 
#     arrange(desc(ndH))
# toc()

## Nouns
## ~4 sec
## Better than verbs, but still some garbage
# tic()
# nouns = filter(tokens, upos == 'NOUN') %>% 
#     count(comment_id, lemma)
# calculate_H(nouns, comment_id, lemma, n) %>% 
#     arrange(desc(ndH))
# toc()

## Adjective-noun bigrams
## ~9 sec
## These look great
tic()
adj_bigrams = filter(tokens, upos == 'ADJ') %>% 
    select(comment_id, doc_id, sid, tid, lemma, tid_source) %>% 
    inner_join(filter(tokens, upos == 'NOUN'),
              by = c('comment_id', 'doc_id', 'sid', 
                            'tid_source' = 'tid')) %>% 
    mutate(bigram = str_c(lemma.x, '_', lemma.y)) %>% 
    count(comment_id, bigram)
adj_h = calculate_H(adj_bigrams, comment_id, bigram, n) %>% 
    arrange(desc(ndH))
toc()

## Bigrams
## ~90 sec
## Lots of garbage at the top
# tic()
# bigrams = tokens %>% 
#     filter(!upos %in% c('PUNCT', 'SPACE', 'NUM')) %>% 
#     group_by(comment_id, doc_id, sid) %>% 
#     select(token, lemma) %>% 
#     mutate(lemma_lead = lead(lemma, 1L), 
#            bigram = str_c(lemma, '_', lemma_lead)) %>% 
#     ungroup() %>% 
#     count(comment_id, bigram)
# calculate_H(bigrams, comment_id, bigram, n) %>% 
#     arrange(desc(ndH))
# toc()


## EDA ----
## 105k bigrams
nrow(adj_h)

# ggplot(adj_h, aes(n, delta_H, color = ndH)) +
#     geom_point(alpha = .5) +
#     scale_x_log10() +
#     scale_color_viridis_c()
# 
# ggplot(adj_h, aes(ndH)) +
#     geom_density(color = 'blue') +
#     stat_ecdf(color = 'red') +
#     geom_hline(yintercept = .99)


## Vocabulary selection ----
vocab_5h = adj_h %>% 
    arrange(desc(ndH)) %>% 
    head(5e2) %>% 
    pull(bigram)
vocab_1k = adj_h %>% 
    arrange(desc(ndH)) %>% 
    head(1e3) %>% 
    pull(bigram)
vocab_5k = adj_h %>% 
    arrange(desc(ndH)) %>% 
    head(5e3) %>% 
    pull(bigram)


## Write output ----
write_rds(adj_bigrams, str_c(data_folder, prefix, 'adj_bigrams.Rds'))
write_rds(adj_h, str_c(data_folder, prefix, 'adj_h.Rds'))

list('vocab_5h' = vocab_5h, 
     'vocab_1k' = vocab_1k, 
     'vocab_5k' = vocab_5k) %>% 
    iwalk(~ write_rds(.x, path = str_c(data_folder, prefix, 
                                      .y, '.Rds')))
