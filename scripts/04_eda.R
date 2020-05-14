## EDA of parsed comments
library(tidyverse)
theme_set(theme_minimal())

data_folder = '../data/'
words_filter = quos(! upos %in% c('PUNCT', 'SPACE', 'SYM', 'X', 'NUM'))

## Read data ----
text = read_rds(file.path(data_folder, '03_text.Rds'))
tokens = read_rds(str_c(data_folder, '03_tokens.Rds'))


## How many comments? ----
## 9292
count(tokens, comment_id) %>% 
    nrow()

## Distinct terms? 
## 211,390
count(tokens, lemma) %>% 
    nrow()

## Parts of speech
tokens %>% 
    count(upos) %>% 
    arrange(desc(n))

## X is unclassifiable URLs, etc.
filter(tokens, upos == 'X')
## SYM is dashes, dollars signs, etc. 
filter(tokens, upos == 'SYM') %>% 
    count(token) %>% 
    arrange(desc(n))

## Distribution of comment length ----
## In tokens, dropping punctuation, spaces, symbols, unknown
dtm = tokens %>% 
    filter(!!!words_filter) %>% 
    count(comment_id, lemma)

dtm %>% 
    group_by(comment_id) %>% 
    summarize(n = sum(n)) %>% 
    ggplot(aes(n)) +
    stat_ecdf() +
    geom_rug() +
    scale_x_log10()

## Longstanding known bug: <https://github.com/ropensci/plotly/issues/1377>
# plotly::ggplotly()

## 5% of docs are shorter than 12 words
## 95% are shorter than 676 words
## Max runs into tens of thousands of words
dtm %>% 
    group_by(comment_id) %>% 
    summarize(n = sum(n)) %>% 
    pull(n) %>% 
    quantile(c(.05, .25, .5, .75, .95))

## Very short comments
## Mostly one-sentence statements of support or opposition
dtm %>% 
    group_by(comment_id) %>% 
    summarize(n = sum(n)) %>% 
    filter(n <= 12) %>% 
    arrange(n) %>% 
    left_join(text) %>% 
    view()

## Mean IDF for detecting OCR errors ----
idf = tokens %>% 
    filter(!!!words_filter) %>% 
    count(lemma, name = 'df') %>% 
    mutate(idf = 1/df)

mean_idf = dtm %>% 
    inner_join(idf, by = 'lemma') %>% 
    group_by(comment_id) %>% 
    summarize(mean_idf = weighted.mean(idf, w = n)) %>% 
    arrange(desc(mean_idf))

ggplot(mean_idf, aes(mean_idf)) +
    stat_ecdf() +
    geom_rug()

## The top 7 are all handwritten letters
## 8 just had a string of numbers followed by a signature
## 9-10 are short born-digital comments with misspellings
slice(mean_idf, 1:10)
## 11-20 all fall into these same categories
slice(mean_idf, 11:20)


## Science and health ----
## 706 cognate terms based on "scien"
## Most common is "science" as a noun
tokens %>% 
    filter(str_detect(lemma, 'scien')) %>% 
    count(lemma, upos) %>% 
    arrange(desc(n))

## 6808 comments use "science" as a noun
tokens %>% 
    filter(str_detect(lemma, 'science'), upos == 'NOUN') %>% 
    count(comment_id) %>% 
    nrow()

## 251 cognate terms based on "health"
## Most commen is "health" as a noun
tokens %>% 
    filter(str_detect(lemma, 'health')) %>% 
    count(lemma, upos) %>% 
    arrange(desc(n))

## 5580 comments use "health" as a noun
tokens %>% 
    filter(str_detect(lemma, 'health'), upos == 'NOUN') %>% 
    count(comment_id) %>% 
    nrow()

