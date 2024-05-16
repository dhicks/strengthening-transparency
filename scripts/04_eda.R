## EDA of parsed comments
library(tidyverse)
theme_set(theme_minimal())
library(patchwork)
library(here)
library(arrow)

source(here('R', 'build_url.R'))

data_dir = here('data')
words_filter = quos(! pos %in% c('PUNCT', 'SPACE', 'SYM', 'X', 'NUM'))

## Read data ----
meta = read_rds(here(data_dir, '01_comments.Rds')) |> 
    mutate(date = date(receiveDate))

text = open_dataset(here(data_dir, '03_text.parquet'))
tokens = open_dataset(here(data_dir, '03_annotations'))


## How many comments? ----
## 22,390 attachments
nrow(meta)
## 20,787 comments
text |> 
    pull(comment_id, as_vector = TRUE) |> 
    n_distinct()

text |>
    count(comment_id) |>
    pull(n) |> 
    summary()

## Distinct terms? 
## 272,422
count(tokens, lemma) |> 
    pull(lemma) |> 
    n_distinct()

## Parts of speech
## 20% nouns; 13% punctuation; 13% proper noun; 9% verbs
tokens |> 
    count(pos) |> 
    arrange(desc(n)) |> 
    collect() |> 
    mutate(share = n/sum(n) * 100)

tokens |> 
    filter(pos == 'PROPN') |> 
    count(lemma) |> 
    head(20) |> 
    collect()

## X is unclassifiable URLs, etc.
filter(tokens, pos == 'X') |> 
    count(lemma) |> 
    collect()

## SYM is dashes, dollars signs, etc. 
filter(tokens, pos == 'SYM') |> 
    count(token) |> 
    arrange(desc(n)) |> 
    collect()


## Comment posting date ----
## Two waves: (1) May-August 2018, (2) March-May 2020
date_gg = meta |> 
    mutate(month = round_date(date, unit = 'month'), 
           week = round_date(date, unit = 'week')) |> 
    collect() |> 
    ggplot(aes(week)) +
    geom_bar(fill = 'blue', color = 'transparent') +
    geom_vline(xintercept = ym('2018-01',
                               '2019-01',
                               '2020-01'), 
               alpha = .4) +
    scale_x_date(date_breaks = '4 months', 
                 date_minor_breaks = '2 months',
                 date_labels = '%Y-%m') +
    scale_y_sqrt() +
    labs(x = 'date', 
         y = 'submission count\n(weekly total)')
date_gg

## first date was 22 May 2017; final date was 8 Dec 2020
summary(meta$date)

## Only 1 comment prior to February 2018
## attachment URL is https://downloads.regulations.gov/EPA-HQ-OA-2018-0259-0731/attachment_1.pdf
## Email metadata makes it clear this was mis-dated when it was archived
meta |> 
    filter(receiveDate < '2018-02-01') |> 
    inner_join(collect(text), by = c('id' = 'comment_id')) |> 
    pull(text) |> 
    str_trunc(500)


## Distribution of comment length ----
## In tokens; dropping punctuation, spaces, symbols, unknown
dtm = tokens |> 
    filter(!!!words_filter) |> 
    count(comment_id, lemma)

ecdf_gg = dtm |> 
    group_by(comment_id) |> 
    summarize(n = sum(n)) |> 
    collect() |> 
    ggplot(aes(n)) +
    stat_ecdf() +
    geom_rug(alpha = 1) +
    scale_x_log10(labels = scales::label_log()) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(x = 'comment length', 
         y = 'cumulative frequency')
ecdf_gg

# plotly::ggplotly()

date_gg + ecdf_gg +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'a')

ggsave(here('out', '04_date_ecdf.png'), 
       height = 4, width = 5, bg = 'white', scale = 1.5)

## 5% of docs are shorter than 23 words
## 95% are shorter than 495 words
## Max runs 468k words
## median 236 words
dtm |> 
    group_by(comment_id) |> 
    summarize(n = sum(n)) |> 
    pull(n) |> 
    quantile(c(.05, .25, .5, .75, .95, 1))

## Very short comments
## Mostly one-sentence statements of support or opposition
dtm |> 
    group_by(comment_id) |> 
    summarize(n = sum(n)) |> 
    filter(n <= 12) |> 
    arrange(n) |> 
    left_join(text) |> 
    view()


## Mean IDF for detecting OCR errors ----
idf = tokens |> 
    filter(!!!words_filter) |> 
    count(lemma, name = 'df') |> 
    mutate(idf = 1/df)

mean_idf = dtm |> 
    inner_join(idf, by = 'lemma') |> 
    collect() |> 
    group_by(comment_id) |> 
    summarize(mean_idf = weighted.mean(idf, w = n)) |> 
    arrange(desc(mean_idf)) |> 
    mutate(url = build_url(comment_id))

ggplot(mean_idf, aes(mean_idf)) +
    stat_ecdf() +
    geom_rug()

## The top 9 are all handwritten letters or postcards
## 10 is an email comprising just some button labels in Yahoo Mail
slice(mean_idf, 1:10)
## 12 and 13 are short, born-digital; otherwise 11-20 are more postcards
slice(mean_idf, 11:20)


## Science and health ----
## 995 terms based on "scien"
## Most common is "science" as a noun, followed by "scientific" as adjective and "scientist" as noun
tokens |> 
    filter(str_detect(lemma, 'scien')) |> 
    count(lemma, pos) |> 
    arrange(desc(n)) |> 
    collect() #|> view()

## 16,980 comments use "science" as a noun
tokens |> 
    filter(str_detect(lemma, 'science'), pos == 'NOUN') |> 
    count(comment_id) |> 
    collect() |> 
    nrow()

## 352 terms based on "health"
## Most common by far is "health" as a noun (76k instances), followed distantly by healthy (929 instances)
tokens |> 
    filter(str_detect(lemma, 'health')) |> 
    count(lemma, pos) |> 
    arrange(desc(n)) |> 
    collect()

## 15,419 comments use "health" as a noun
tokens |> 
    filter(str_detect(lemma, 'health'), pos == 'NOUN') |> 
    count(comment_id) |> 
    collect() |> 
    nrow()

## 17,809 use either "health" or "science" as a noun
tokens |> 
    filter(str_detect(lemma, 'science|health'), pos == 'NOUN') |> 
    count(comment_id) |> 
    collect() |> 
    nrow()

