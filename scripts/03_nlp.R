library(tidyverse)
library(spacyr)
library(furrr)

library(assertthat)
library(tictoc)

warning('This script uses a Python library that requires installation')
# spacyr::find_spacy(ask = TRUE)
# spacyr::spacy_install(version = "latest_v1")

prefix = '03_'
data_folder = '../data/'
annotations_folder = str_c(data_folder, prefix, 'annotations/')
if (!dir.exists(annotations_folder)) {
    dir.create(annotations_folder)
}

clean_phrases = function(phrase) {
    phrase %>% 
        tolower() %>% 
        str_replace_all('[^[:word:]]', ' ') %>% 
        str_replace_all('^(the|a|this) ', '') %>% 
        str_squish() %>% 
        str_replace_all(' ', '_')
}


## Load data ----
comments_df = read_rds(str_c(data_folder, '01_comments.Rds')) %>% 
    select(comment_id = id, text = comment) %>% 
    mutate(docOrder = 0L)
attachments_df = read_rds(str_c(data_folder, '02_attachments.Rds')) %>% 
    select(comment_id, docOrder, text)
    

comb_df = bind_rows(comments_df, attachments_df) %>% 
    mutate(doc_id = str_c(comment_id, '-', docOrder)) %>% 
    ## EDA indicates long texts are either thousands of identical comments
    ## or regulatory documents
    mutate(len = str_length(text)) %>% 
    filter(len < 1e6, len > 100) %>% 
    mutate(row_id = row_number()) %>% 
    select(doc_id, text, row_id, everything())


## Parse with spaCy ----
## 4.731 / 500 * 21732 -> ~200 sec
tic()
comb_df %>% 
    slice(1) %>% 
    spacy_extract_nounphrases(multithread = TRUE) %>% 
    as_tibble() %>% 
    mutate(clean_text = clean_phrases(text))
toc()
spacy_finalize()


annotate = function(doc_id, text, 
                    target_folder = annotations_folder, 
                    verbose = TRUE,
                    force = FALSE) {
    len = str_length(text)
    if (len == 0L) {
        stop(str_c('Length-0 string in ', doc_id))
    }
    
    target_file = str_c(target_folder, doc_id, '.Rds')
    if (!file.exists(target_file) || force) {
        if (verbose) message(doc_id)
        annotated = spacy_extract_nounphrases(text, multithread = TRUE) 
        
        if (!is.null(annotated)) {
            annotated = annotated %>% 
                as_tibble() %>% 
                select(-doc_id) %>% 
                mutate(doc_id = doc_id, 
                       clean_text = clean_phrases(text)) %>% 
                select(doc_id, everything())
        } else {
            annotated = tibble(doc_id)
        }
        
        write_rds(annotated, target_file)
    } else {
        annotated = read_rds(target_file)
    }
    return(annotated)
}
# annotate(comb_df$doc_id[[1]], comb_df$text[[1]], force = TRUE)

# debugonce(annotate)
# comb_df %>% 
#     filter(doc_id == 'EPA-HQ-OA-2018-0259-6305-0') %>% 
#     pull(text) %>% 
#     {annotate('EPA-HQ-OA-2018-0259-6305-0', .)}

## Maybe 20 minutes if everything needs to be annotated
spacy_initialize()
plan(multisession, workers = 6L)
tic()
phrases_df = comb_df %>% 
    # head(10000) %>% 
    select(doc_id, text) %>% 
    future_pmap_dfr(annotate, 
                    force = FALSE, verbose = FALSE, 
                    .options = furrr_options(seed = NULL),
                    .progress = TRUE)
toc()
plan(sequential)
spacy_finalize()

assert_that(are_equal(n_distinct(comb_df$doc_id), 
                      n_distinct(phrases_df$doc_id)))

## Write output ----
# write_rds(comb_df, file.path(data_folder, str_c(prefix, 'text.Rds')))
write_rds(phrases_df, file.path(data_folder, str_c(prefix, 'phrases.Rds')))

