#' This script retrieves all comments and selected attachments for a given Regulations.gov docket.  Beyond the dependencies in the first code block, it also uses two external command-line tools, pdftotext <http://www.foolabs.com/xpdf/download.html> and pandoc <http://pandoc.org/>.  
#' 
#' Note that this script assumes that a Regulations.gov API key is declared in `api_key.R`, as `api_key = '123foo'`.

library(tidyverse)
library(lubridate)
# library(stringr)
library(xml2)

# library(foreach)
# library(doSNOW)
library(furrr)
plan(multisession, workers = 3)

library(tictoc)

source('../R/scrape.R')

prefix = '01_'
rpp = 1000 ## number of results per page in API search

## Top-level parameters ----
## Docket of interest
## https://www.regulations.gov/docket?D=EPA-HQ-OA-2018-0259
docket_id = 'EPA-HQ-OA-2018-0259'

## Folders
files_folder = '../data/'
if (!file.exists(files_folder)) {
    dir.create(files_folder)
}

doc_ids_folder = str_c(files_folder, prefix, 'doc_id/')
if (!file.exists(doc_ids_folder)) {
    dir.create(doc_ids_folder)
}
docs_folder = str_c(files_folder, prefix, 'docs/')
if (!file.exists(docs_folder)) {
    dir.create(docs_folder)
}
attachments_folder = str_c(files_folder, prefix, 'attachments/')
if (!file.exists(attachments_folder)) {
    dir.create(attachments_folder)
}

## API key
## http://regulationsgov.github.io/developers/key/
source('api_key.R')


## Retrieve IDs ----
## for the public submissions in the docket
base_query = str_c('https://api.data.gov/regulations/v3/documents.xml', 
                   '?', 
                   'api_key=', api_key, '&', 
                   'dct=PS', '&', 
                   'dktid=', docket_id)

total_docs_query = str_c(base_query, '&',
                         'countsOnly=1' ## how many results?
)
total_docs_resp = read_xml(total_docs_query)
total_docs = total_docs_resp %>% 
    xml_find_first('//recordCount') %>%
    xml_text() %>% 
    as.numeric()

## Paging to get doc IDs
num_pages = ceiling(total_docs / rpp)
offsets = (1:num_pages - 1)*rpp
names(offsets) = str_pad(1:num_pages, 3, pad = '0')

doc_id_queries = str_c(base_query, '&',
                       'rpp=', rpp, '&', ## get up to 1k docs
                       'po=', offsets) %>% 
    set_names(names(offsets))
doc_id_files = str_c(doc_ids_folder, 
                     names(offsets), 
                     '.xml')

# scrape(doc_id_queries[[1]], doc_id_files[[1]])
future_map2_lgl(doc_id_queries, doc_id_files, scrape, 
                .progress = TRUE) %>% 
    all()

parse_doc_ids = function(doc_ids_file) {
    doc_ids_file %>% 
        read_xml() %>% 
        xml_find_all('//documentId') %>% 
        xml_text()
}
doc_ids = future_map(doc_id_files, parse_doc_ids, 
                     .progress = TRUE) %>% 
    simplify() %>% 
    as_vector(.type = 'chr')


## Scrape each document ---- 
# doc_id = doc_ids[155]
doc_files = str_c(docs_folder, doc_ids, '.xml')
doc_queries = str_c('https://api.data.gov/regulations/v3/document.xml', 
                    '?', 
                    'api_key=', api_key, '&', 
                    'documentId=', doc_ids)

## NB Running this in parallel will trigger rate-limiting
# plan(sequential)
plan(multisession, workers = 1)
future_map2_lgl(doc_queries, doc_files, scrape, 
                delay = 3600/1000,
                .progress = TRUE
) %>% 
    all()

parse_comments = function(doc_file) {
    response = read_xml(doc_file)
    
    doc_id = response %>% 
        xml_find_first('./documentId') %>% 
        xml_text()
    
    comment_text = response %>%
        xml_find_first('//comment') %>%
        xml_text()
    date = response %>%
        xml_find_first('//receivedDate') %>%
        xml_text %>%
        ymd_hms %>%
        round_date(unit = 'day')
    
    attachments = response %>%
        xml_find_all('//attachment')
    attachment_urls = attachments %>%
        xml_find_first('fileFormats') %>%
        xml_text()
    if (length(attachment_urls) == 0) {
        attachments = NA
        attachment_urls = as.character(NA)
    }
    ## Don't get rate-limited
    # Sys.sleep(1)
    
    regs_gov_url = str_c(
        'https://www.regulations.gov/document?D=',
        doc_id
    )
    return(tibble(comment_id = doc_id,
           url = regs_gov_url,
           comment_text,
           date,
           attachment_urls = list(attachment_urls)))
}

# parse_comments(doc_files[[1]]) %>% view()

plan(multisession, workers = 3)
## ~20 sec
tic()
comments = future_map_dfr(doc_files, parse_comments, 
                                 .progress = TRUE)
toc()


## Scrape attachments ----
#' Identify attachments to download
attachments_unfltd = comments %>%
    select(comment_id, url = attachment_urls) %>% 
    unnest(url) %>%
    filter(!is.na(url)) %>% 
    mutate(type = str_match(url, 'contentType=([^&]*)')[,2], 
           attachment_num = str_match(url, 'attachmentNumber=([0-9]+)')[,2], 
           filename = str_c(comment_id, '-', attachment_num), 
           url = str_c(url, '&api_key=', api_key))

count(attachments_unfltd, type)

attachments = attachments_unfltd %>% 
    filter(type %in% c('pdf', 'msw12', 'msw8', 'crtext')) %>% 
    mutate(ext = case_when(type == 'pdf' ~ 'pdf', 
                           type == 'msw12' ~ 'docx', 
                           type == 'msw8' ~ 'doc', 
                           type == 'crtext' ~ 'txt'), 
           path = str_c(attachments_folder, 
                        filename, '.', ext))

plan(sequential)
future_map2(attachments$url, attachments$path, 
            scrape,
            delay = 3.6*2,
            .progress = TRUE)


## TODOs:  use pdftools and officer packages for text extraction

#'             ## Convert to text
#'             txt_version = str_c(files_folder, '/', 
#'                                 attachment$filename, '.', 'txt')
#'             if (!file.exists(txt_version)) {
#'                 if (attachment$ext == 'pdf') {
#'                     ## pdftotext: 
#'                     ## <http://www.foolabs.com/xpdf/download.html>
#'                     system2('pdftotext', dl_version, txt_version)
#'                 } else if (attachment$ext == 'docx') {
#'                     ## Pandoc
#'                     system2('pandoc', c(dl_version, 
#'                                         '-o', txt_version))
#'                 } else if (attachment$ext == 'doc') {
#'                     warning(str_c('Cannot convert ', 
#'                                   attachment$filename,
#'                                   ' from doc to txt automatically'))
#'                 } else {
#'                     stop('Unknown file type')
#'                 }
#'             }
#'         }
#' 
#' 
#' save(comments, file = '1_comments.Rdata')
#' comments %>%
#'     select(-comment_text, -attachment_urls) %>%
#'     write_excel_csv('1_comment_metadata.csv')
#' 
#' sessionInfo()
#' 
