library(tidyverse)
library(spacyr)
library(furrr)

library(assertthat)
library(tictoc)
library(here)
library(glue)
library(arrow)

warning('This script uses a Python library that requires installation')
# spacyr::find_spacy(ask = TRUE)
# spacyr::spacy_install(version = "3.7.2")

data_dir = here('data')
ann_dir = here(data_dir, '03_annotations')
if (!dir.exists(ann_dir)) {
    dir.create(ann_dir)
}

force = TRUE
if (force) warning('This script will re-annotate all documents')

## Load data ----
comments_df = read_rds(here(data_dir, '01_comments.Rds')) |> 
    select(comment_id = id, text = comment) |> 
    mutate(docOrder = 0L)
attachments_df = read_rds(here(data_dir, '02_attachments.Rds')) |> 
    select(comment_id, docOrder, text)

## Combined df w/ text of all comments and attachments
text_pq = here(data_dir, '03_text.parquet')
if (!file.exists(text_pq)) {
    comb_df = bind_rows(comments_df, attachments_df) |> 
        mutate(doc_id = str_c(comment_id, '-', docOrder)) |> 
        ## EDA indicates long texts are either thousands of identical comments
        ## or regulatory documents
        mutate(len = str_length(text)) |> 
        filter(len < 1e6, len > 100) |> 
        mutate(row_id = row_number()) |> 
        select(comment_id, doc_id, text, row_id, everything())
    
    write_parquet(comb_df, text_pq)
} else {
    comb_df = read_parquet(text_pq)
}

## 21,732 documents to annotate
nrow(comb_df)


## Parse with spaCy ----
## 8.6 / 500 * 21732 -> ~370 sec = 6 min? 
# tic()
# foo = comb_df |>  
#     slice(1:500) |> 
#     spacy_parse(entity = FALSE, 
#                 nounphrase = FALSE, 
#                 multithread = TRUE)
# toc()
# spacy_finalize()


annotate = function(comment_id, 
                    doc_id, 
                    text, 
                    target_folder = ann_dir, 
                    verbose = TRUE,
                    force = FALSE) {
    len = str_length(text)
    if (len == 0L) {
        stop(str_c('Length-0 string in ', doc_id))
    }
    
    doc_pq = here(target_folder, str_c(doc_id, '.parquet'))
    if (file.exists(doc_pq) && !force) {
        if (verbose) message(glue('{doc_id} already parsed'))
        return(TRUE)
    } else {
        if (verbose) message(doc_id)
        annotated = spacy_parse(text, 
                                entity = FALSE, 
                                dependency = TRUE,
                                nounphrase = FALSE, 
                                multithread = TRUE)
        
        if (!is.null(annotated)) {
            annotated = annotated |> 
                as_tibble() |> 
                select(-doc_id) |> 
                mutate(comment_id = comment_id, 
                       doc_id = doc_id) |> 
                select(comment_id, doc_id, everything())
        } else {
            annotated = tibble(doc_id)
        }
        
        write_parquet(annotated, doc_pq)
        return(TRUE)
    }
}
# debugonce(annotate)
# annotate(comb_df$comment_id[[2]],
#              comb_df$doc_id[[2]],
#                comb_df$text[[2]], force = FALSE)

## Maybe 30 minutes if everything needs to be annotated
spacy_initialize()
plan(multisession, workers = 6L)
tic()
comb_df |> 
    # head(100) |>
    select(comment_id, doc_id, text) |> 
    future_pmap_lgl(annotate, 
                    force = force, verbose = FALSE, 
                    .options = furrr_options(seed = NULL),
                    .progress = TRUE) |> 
    length()
toc()
plan(sequential)
spacy_finalize()

open_dataset(ann_dir) |> 
    collect() |> 
    pull(doc_id) |> 
    n_distinct() |> 
    are_equal(n_distinct(comb_df$doc_id)) |> 
    assert_that(msg = "annotated docs don't match combined doc list")

