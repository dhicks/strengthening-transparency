library(tidyverse)
theme_set(theme_bw())

source('../R/H.R')

library(tictoc)

data_folder = '../data/'
prefix = '05_'

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
## Where the nouns are 'science' and 'health'
## ~9 sec
## These look great
tic()
adj_bigrams = tokens %>% 
    filter(upos %in% c('ADJ')) %>% 
    select(comment_id, doc_id, sid, tid, lemma, tid_source) %>% 
    inner_join(filter(tokens, 
                      upos == 'NOUN', 
                      token %in% c('science', 'health')),
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
## 555 science-health bigrams
## 265 with H > 0
nrow(adj_h)
adj_h %>% 
    filter(H > 0) %>% 
    nrow()

# ggplot(adj_h, aes(n, delta_H, color = ndH)) +
#     geom_point(aes(label = bigram), alpha = .5) +
#     scale_x_log10() +
#     scale_color_viridis_c()
# plotly::ggplotly()
# 
# ggplot(adj_h, aes(ndH)) +
#     geom_density(color = 'blue') +
#     stat_ecdf(color = 'red') +
#     geom_hline(yintercept = .99)


## Bigram your_science shows up twice, in 2 almost-identical comments
## The comment contains "PLEASE ENTER YOUR COMMENT HERE" followed 
## immediately (w/out space) by the content of the comment: 
## "end secret science now! Regards, Juliet Winkour"
## The POS tagger evidently choked on this, interpreting YOUR as an 
## adjective modifying "science." 
## Because this document also gets the secret_science bigram, your_science appears on the top side. 
# adj_bigrams %>% 
#     filter(bigram == 'your_science') %>% 
#     inner_join(tokens, by = 'comment_id') %>% 
#     view()


## Vocabulary selection ----
vocab_sh = adj_h %>% 
    filter(H > 0) %>%
    pull(bigram)

# vocab_5h = adj_h %>% 
#     arrange(desc(ndH)) %>% 
#     head(5e2) %>% 
#     pull(bigram)
# vocab_1k = adj_h %>% 
#     arrange(desc(ndH)) %>% 
#     head(1e3) %>% 
#     pull(bigram)
# vocab_5k = adj_h %>% 
#     arrange(desc(ndH)) %>% 
#     head(5e3) %>% 
#     pull(bigram)


## Write output ----
## Total word counts
tokens %>% 
    filter(!upos %in% c('PUNCT', 'SPACE', 'NUM')) %>% 
    count(comment_id) %>% 
    write_rds(str_c(data_folder, prefix, 'word_counts.Rds'))

write_rds(adj_bigrams, str_c(data_folder, prefix, 'adj_bigrams.Rds'))
write_rds(adj_h, str_c(data_folder, prefix, 'adj_h.Rds'))

lst(# vocab_5h, 
    # vocab_1k, 
    # vocab_5k,
    vocab_sh
    ) %>% 
    iwalk(~ write_rds(.x, path = str_c(data_folder, prefix, 
                                      .y, '.Rds')))
