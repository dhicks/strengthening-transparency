library(tidyverse)

library(tictoc)

library(reticulate)
library(cleanNLP)

## Set the conda environment before loading the spaCy model
which_conda = '~/opt/anaconda3/bin/conda'

## CoreNLP - quite slow
# # conda_create('coreNLP', conda = which_conda)
# # conda_install('coreNLP', 'cleannlp', conda = which_conda, pip = TRUE)
# use_condaenv('coreNLP', conda = which_conda, required = TRUE)
# # py_config()
# # cnlp_download_corenlp(lang = 'en')
# cnlp_init_corenlp()

## spaCy - chokes on strings larger than 1M characters
use_condaenv('spacy', conda = which_conda)
# py_config()
cnlp_init_spacy('en')

prefix = '03_'
data_folder = '../data/'
annotations_folder = str_c(data_folder, prefix, 'annotations/')
if (!dir.exists(annotations_folder)) {
    dir.create(annotations_folder)
}


## Load data ----
comments_df = read_rds(str_c(data_folder, '01_comments.Rds')) %>% 
    select(-attachment_urls) %>% 
    rename(text = comment_text)
attachments_df = read_rds(str_c(data_folder, '02_attachments.Rds'))

comb_df = bind_rows(comments_df, attachments_df) %>% 
    select(comment_id, text) %>% 
    arrange(comment_id) %>% 
    group_by(comment_id) %>% 
    mutate(doc_id = str_c(comment_id, '-', row_number())) %>% 
    ungroup() %>% 
    ## EDA indicates long texts are either thousands of identical comments
    ## or regulatory documents
    mutate(len = str_length(text)) %>% 
    filter(len < 1e6, len > 0) %>% 
    mutate(row_id = row_number())


## Parse with spaCy ----
## ~8 sec / 100 -> ~1100 sec = ~18 min
## Actually 1378 sec
# tic()
# annotated = comb_df %>% 
#     # head(100) %>%
#     cnlp_annotate(verbose = TRUE)
# toc()

annotate = function(doc_id, text, 
                    target_folder = annotations_folder, 
                    verbose = TRUE,
                    force = FALSE) {
    len = str_length(text)
    if (len == 0L) {
        stop(str_c('Length-0 string in ', doc_id))
    }
    
    target_file = str_c(target_folder, doc_id, '.Rds')
    if (!file.exists(target_file) | force) {
        if (verbose) message(doc_id)
        annotated = cnlp_annotate(text, verbose = verbose)$token
        write_rds(annotated, target_file)
    } else {
        annotated = read_rds(target_file)
    }
    annotated %>% 
        select(-doc_id) %>% 
        return()
}
# annotate(comb_df$doc_id[[1]], comb_df$text[[1]])


## Maybe ~2k sec if everything needs to be annotated from scratch
## ~50 sec to load all the Rds files
tic()
tokens_df = comb_df %>% 
    # head(100) %>% 
    select(doc_id, text) %>% 
    pmap_dfr(annotate, .id = 'row_id') %>% 
    mutate(row_id = as.integer(row_id)) %>% 
    inner_join(comb_df, ., by = 'row_id') %>% 
    select(comment_id, doc_id, sid:relation)
toc()


## Write output ----
write_rds(comb_df, file.path(data_folder, str_c(prefix, 'text.Rds')))
write_rds(tokens_df, str_c(data_folder, prefix, 'tokens.Rds'))

