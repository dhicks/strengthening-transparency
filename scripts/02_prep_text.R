## This script wrangles the text of the attachments
library(tidyverse)
library(pdftools)
library(officer)

library(furrr)
plan(multisession, workers = 3)

library(tictoc)
library(assertthat)

source('../R/extract_text.R')

prefix = '02_'
data_folder = '../data/'
# attachment_folder = str_c(data_folder, '01_attachments/')
attachment_text_folder = str_c(data_folder, prefix, 'attachment_text/')

## Load data ----
attachments_df = read_rds(str_c(data_folder, '01_attachments.Rds')) %>% 
    rename(dl_path = path) %>% 
    mutate(text_path = str_c(attachment_text_folder, filename, '.txt'))


## Extract text ----
## ~137 sec if everything needs to be extracted
tic()
attach_text = attachments_df %>% 
    # head(100) %>%
    mutate(text = future_pmap(list(dl_path, text_path, ext), 
                              extract_text, force = FALSE, 
                              .progress = TRUE), 
           text = map_chr(text, str_flatten, collapse = '\n'), 
           text = str_squish(text)) %>% 
    select(-matches('path'))
toc()


## Some EDA/error checking ----
## Very short documents -> some scanned docs need OCR
## Two remaining:  
## one that's totally blank; one that's a handwritten postcard
message('Checking short docs')
attach_text %>%
    mutate(len = str_length(text)) %>%
    filter(len < 100) %>%
    select(filename, ext, len)

# attach_text %>% 
#     mutate(len = str_length(text)) %>% 
#     ggplot(aes(len)) +
#     geom_histogram(binwidth = .25) +
#     scale_x_log10()


## Write output ----
write_rds(attach_text, str_c(data_folder, prefix, 'attachments.Rds'))
