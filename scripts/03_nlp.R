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


## Load data ----
comments_df = read_rds(str_c(data_folder, '01_comments.Rds')) %>% 
    select(-attachment_urls) %>% 
    rename(text = comment_text)
attachments_df = read_rds(str_c(data_folder, '02_attachments.Rds'))

comb_df = bind_rows(comments_df, attachments_df) %>% 
    select(comment_id, text) %>% 
    arrange(comment_id) %>% 
    mutate(doc_id = row_number(), 
           len = str_length(text)) %>% 
    ## EDA indicates long texts are either thousands of identical comments
    ## or regulatory documents
    filter(len < 1e6)


## Parse with spaCy ----
## ~8 sec / 100 -> ~1100 sec = ~18 min
## Actually 1378 sec
tic()
annotated = comb_df %>% 
    # head(100) %>%
    cnlp_annotate(verbose = TRUE)
toc()

tokens_df = inner_join(annotated$document, annotated$token, 
                    by = 'doc_id') %>% 
    nest(tokens = sid:relation)


## Write output ----
write_rds(tokens_df, str_c(data_folder, prefix, 'tokens.Rds'))

