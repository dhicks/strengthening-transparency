## This script wrangles the text of the attachments
library(tidyverse)
# devtools::install('../reg.gov.api')
library(reg.gov.api)

library(furrr)

library(tictoc)
library(assertthat)


prefix = '02_'
data_folder = file.path('..', 'data')
# attachment_folder = str_c(data_folder, '01_attachments')
attachment_text_folder = file.path(data_folder, 
                                   str_c(prefix, 
                                         'attachment_text'))

## Load data ----
attachments_df = read_rds(file.path(data_folder, 
                                    '01_attachments.Rds')) %>% 
    rename(dl_path = path) %>% 
    mutate(text_path = file.path(attachment_text_folder, 
                                 str_c(file, '.txt')))

count(attachments_df, format)

## Extract text ----
## ~137 sec if everything needs to be extracted
plan(multisession, workers = 6)
tic()
attach_text = attachments_df %>% 
    # slice(4000:5000) %>%
    mutate(text = future_map2(dl_path, text_path, 
                              extract_text, 
                              ocr_threshold = 100L,
                              force = FALSE, 
                              verbose = FALSE,
                              .progress = TRUE), 
           text = map_chr(text, str_flatten, collapse = '\n'), 
           text = str_squish(text)) %>% 
    select(-matches('path'))
toc()
plan(sequential)


## Some EDA/error checking ----
# attach_text %>%
#     mutate(len = str_length(text)) %>%
#     ggplot(aes(len)) +
#     geom_histogram(binwidth = .25) +
#     geom_vline(xintercept = 1e6) +
#     scale_x_log10()

## Very short documents
## EPA-HQ-OA-2018-0259-0495-1: blank pages?
## EPA-HQ-OA-2018-0259-3073-1: hand-written postcard
## EPA-HQ-OA-2018-0259-6138-1: camera test card? 
## EPA-HQ-OA-2018-0259-11189-1: photograph
# attach_text %>%
#     mutate(len = str_length(text)) %>%
#     filter(len < 100) %>%
#     select(file, format, len)


## Very long docs (> 1e6)
# message('Checking long docs')
# ## 43 attachments longer than 1M characters
# attach_text %>%
#     mutate(len = str_length(text)) %>%
#     filter(len > 1e6) %>%
#     arrange(desc(len)) %>%
#     select(-text) %>% view()

# ## From just 9 comments
# ## EPA-HQ-OA-2018-0259-6453 has 17 docs w/ thousands of identical comments
# ## EPA-HQ-OA-2018-0259-9233 includes 2 EPA docs w/ hundreds of pages
# ## EPA-HQ-OA-2018-0259-9298 also includes 2 regulatory docs w/ hundreds of pages
# attach_text %>%
#     mutate(len = str_length(text)) %>%
#     filter(len > 1e6) %>%
#     count(comment_id) %>%
#     arrange(desc(n))


## Write output ----
write_rds(attach_text, file.path(data_folder, 
                                 str_c(prefix, 'attachments.Rds')))
