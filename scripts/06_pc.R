## Principal components / latent semantic analysis of science and health bigrams
library(tidyverse)
theme_set(theme_minimal())

library(tidytext)
# library(widyr)
# library(apcluster)

library(tictoc)
library(assertthat)


data_folder = file.path('..', 'data')
prefix = '06_'

## Load data ----
vocab = read_rds(file.path(data_folder, '05_vocab_sh.Rds'))

dtm = read_rds(file.path(data_folder, '05_adj_bigrams.Rds')) %>% 
    filter(bigram %in% vocab)

dtm %>% 
    count(comment_id, bigram) %>% 
    nrow() %>% 
    all_equal(nrow(dtm)) %>% 
    assert_that(msg = "dtm doesn't have 1 row per comment-bigram")

## Document frequency and H
df = dtm %>% 
    group_by(bigram) %>% 
    summarize(df = n()) %>% 
    ungroup()
h = read_rds(file.path(data_folder, '05_adj_h.Rds'))


## Roughly 2426 comments are nearly identical, and seem to be based on a form letter campaign by the Sierra Club
## See <https://angeles.sierraclub.org/news/blog/2018/05/stop_pruitts_quest_help_industrial_polluters>
## We leave these in for fitting the model, but exclude them when identifying documents at the top and bottom of the first PC
text_df = read_rds(file.path(data_folder, '03_text.Rds'))

sierra_club = text_df %>% 
    filter(str_detect(text, 'Medical records and health data are largely kept confidential to protect patients')|
               str_detect(text, 'Medical science has repeatedly proven that smog')) %>% 
    pull(comment_id)


## How many terms? 
## 239 adjective-noun bigrams w/ noun == health or science
dtm %>% 
    pull(bigram) %>% 
    unique() %>% 
    length()

## How many comments? 
## 6125 (out of 6145 in dtm)
dtm %>% 
    pull(comment_id) %>% 
    unique() %>% 
    length()

## Principal components ----
## ~2 sec
tic()
pc = dtm %>% 
    cast_sparse(bigram, comment_id, n) %>% 
    prcomp(center = TRUE, scale. = TRUE)
toc()

## Scree plot ----
## 1 PC explains almost half of the variance!  5 explain about 80%
tidy(pc, matrix = 'd') %>% 
    ggplot(aes(PC, cumulative)) +
    geom_point() +
    geom_line(group = 1L) +
    ylim(0, 1)

last_plot() +
    xlim(0, 5)

tidy(pc, matrix = 'd') %>% 
    ggplot(aes(PC, std.dev^2)) +
    geom_point() +
    geom_line(group = 1L)

last_plot() +
    xlim(0, 5)


## All bigrams, PC 1 and 2 ----
# pc %>% 
#     tidy() %>% 
#     filter(PC %in% c(1L, 2L)) %>% 
#     mutate(value_trans = sqrt(abs(value)) * if_else(value < 0, -1, 1)) %>% 
#     select(-value) %>% 
#     pivot_wider(names_from = PC, names_prefix = 'pc', 
#                 values_from = value_trans) %>% 
#     left_join(h, by = c('row' = 'bigram')) %>% 
#     mutate(noun = {row %>% 
#             str_extract('_.*') %>% 
#             str_remove('^_')}) %>% 
#     ggplot(aes(pc1, pc2, color = noun)) +
#     geom_point(aes(size = log10(n), 
#                    label = row)) +
#     scale_color_brewer(palette = 'Set1')
# plotly::ggplotly()


## Top and bottom terms for PC 1 ----
top_and_bottom = function(dataf, term_var, value_var, n) {
    term_var = enquo(term_var)
    value_var = enquo(value_var)
    
    top = top_n(dataf, n, wt = !!value_var) %>% 
        mutate(side = 'top')
    bottom = top_n(dataf, -n, wt = !!value_var) %>% 
        mutate(side = 'bottom')
    
    bind_rows(top, bottom) %>%
        arrange(desc(!!value_var), .by_group = TRUE) %>%
        return()
}

pc %>% 
    tidy() %>% 
    filter(PC == 1L) %>% 
    top_and_bottom(row, value, 20) %>% 
    # left_join(h, by = c('row' = 'bigram')) %>% 
    left_join(df, by = c('row' = 'bigram')) %>% 
    mutate(row = str_replace(row, '_', ' '),
           term = fct_rev(fct_inorder(row)), 
           value_trans = sqrt(abs(value)) * sign(value)) %>% 
    ggplot(aes(term, value_trans, color = side)) +
    geom_point(aes(size = df)) +
    geom_segment(aes(xend = term, yend = 0)) +
    scale_color_brewer(palette = 'Set1') +
    scale_size(trans = 'log10') +
    coord_flip() +
    labs(x = 'bigram', 
         y = 'loading (square-root scale)', 
         size = 'document frequency')
# plotly::ggplotly()

## Extreme documents along the first PC ----
# pc %>%
#     tidy(matrix = 'v') %>%
#     filter(PC == 1L, 
#            !column %in% sierra_club) %>%
#     top_and_bottom(column, value, 25)

docs_of_interest = pc %>%
    tidy(matrix = 'v') %>%
    filter(PC == 1L,
           !column %in% sierra_club) %>%
    top_and_bottom(column, value, 150)

pc %>% 
    tidy(matrix = 'v') %>% 
    filter(PC == 1L) %>% 
    mutate(sierra_club = column %in% sierra_club) %>% 
    ggplot(aes(value)) +
    geom_density() +
    geom_rug(data = ~filter(.x, sierra_club),
             outside = TRUE,
             alpha = 1) +
    geom_rug(data = docs_of_interest, 
             aes(color = side)) +
    scale_color_brewer(palette = 'Set1') +
    coord_cartesian(clip = 'off')

docs_of_interest %>% 
    select(value, side, comment = column) %>% 
    write_csv(file.path(data_folder, 
                                      str_c(prefix, 'docs.csv')))

## Simple random sample of 100 docs
set.seed(2020-05-09)
dtm %>% 
    count(comment_id) %>% 
    sample_n(100) %>% 
    select(comment_id) %>% 
    write_csv(file.path(data_folder, 
                        str_c(prefix, 'simple_sample.csv')))
