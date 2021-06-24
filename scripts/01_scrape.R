## NB parallelly <= 1.25.0: <https://github.com/HenrikBengtsson/future/issues/511>
library(tidyverse)
library(furrr)
library(tictoc)
library(assertthat)

## Package with functions for accessing the API
## devtools::install(file.path('..', 'reg.gov.api'))
library(reg.gov.api)

source('api_key.R')

prefix = '01_'

data_folder = file.path('..', 'data')
attachments_folder = file.path(data_folder,
                               str_c(prefix, 'attachments'))

docket_id = 'EPA-HQ-OA-2018-0259'


# Step 1: Get all documents for the docketId ----
message('Step 1: Get all documents in the docket')

docs = docket_id %>%
    doc_list() %>%
    filter(documentType %in% c('Rule', 'Proposed Rule')) %>%
    select(id,
           objectid = objectId,
           type = documentType,
           title = title, 
           posted = postedDate) %>%
    as_tibble()


# Step 2: Get all comments for each document using objectId ----

# Step 2a: Get number of comments for each document ----
message('Step 2a: Number of comments for each document')
# comment_counts('090000648320bc9e')

docs = docs %>%
    mutate(comments = map_int(objectid, comment_counts)) %>%
    filter(comments > 0)


## 2b: Page through each block ----
message('Step 2b: Page through blocks of comments')
# foo = get_blocks('090000648320bc9e', total_comments = 9292, verbose = FALSE)
# foo = get_blocks(docs$objectid[1], docs$comments[1])
# reg.gov.api:::get_comment_page('090000648320bc9e', verbose = TRUE)
# debugonce(reg.gov.api:::get_block)
# foo = reg.gov.api:::get_block('090000648320bc9e', verbose = TRUE)
# debugonce(get_blocks)
# get_blocks('090000648320bc9e', total_comments = 500, verbose = FALSE) %>%
#     filter(!duplicated(.))

plan(multisession, workers = 4)
tic()
comments = with(docs, 
                map2_dfr(objectid, comments, get_blocks, verbose = FALSE)) %>%
    filter(!duplicated(.))
toc()
plan(sequential)

assert_that(are_equal(nrow(comments), 
                      docs %>% 
                          pull(comments) %>% 
                          sum()), 
            msg = 'Number of retrieved comments doesn\'t matching expected number')
assert_that(are_equal(nrow(comments), 22390L), 
            msg = 'Number of retrieved comments doesn\'t match reproducibility check')

## Step 3: Scrape and parse each comment (metadata+text) ----
## Step 3a: Scrape ----
message('3a: Scrape comment metadata + text')

# scrape_comment('EPA-HQ-OA-2018-0259-4769', verbose = TRUE)
# scrape_comment('EPA-HQ-OA-2018-0259-1111')

## NB API rate limit is 1k queries/hour
# plan(sequential)
json_files = comments %>%
    # head() %>%
    pull(id) %>%
    map_chr(scrape_comment,
            scrape_folder = file.path(data_folder, str_c(prefix, 'comments')),
            delay = 3600/1000,
            verbose = FALSE)


## Step 3b: Parse ----
message('3b: Parse comments')

# parser('../data/01_comments/EPA-HQ-OA-2018-0259-1010.json') %>%
#     unnest(attachments) %>%
# view()
# parser(json_files[1], verbose = FALSE) %>%
#     unnest(attachments) %>%
#     select(url, format)
# parser('../data/01_comments/EPA-HQ-OA-2018-0259-0445.json') %>%
#     unnest(attachments) %>%
#     select(url, format)

plan(multisession, workers = 6)
tic()
parsed = future_map_dfr(json_files,
                        reg.gov.api:::parser,
                        verbose = FALSE,
                        .progress = TRUE)
toc()
plan(sequential)


## Step 4: Scrape attachments ----
message('4: Scrape attachments')

attachments = parsed %>%
    filter(n_attachments > 0) %>%
    unnest(attachments) %>%
    filter(!is.na(url),
           format %in% c('docx', 'doc', 'pdf', 'txt')) %>%
    select(comment_id = id,
           objectId, attachment_id,
           docOrder, url, format) %>%
    mutate(file = str_c(comment_id, '-', docOrder, '.', format),
           path = file.path(attachments_folder, file))

message(nrow(attachments), ' total attachments')
attachments %>%
    filter(!file.exists(path)) %>%
    nrow() %>%
    message(' attachments to download')

plan(sequential)
tic()
future_map2(attachments$url, attachments$path,
            scrape,
            delay = 3.6*2,
            .progress = TRUE) %>%
    unlist() %>%
    negate(is.na)() %>% 
    all() %>% 
    assert_that(msg = 'Error in scraping attachments')
toc()


## Step 5: Write output ----
write_rds(parsed, file.path(data_folder,
                            str_c(prefix, 'comments.Rds')))
write_rds(attachments, file.path(data_folder,
                                 str_c(prefix, 'attachments.Rds')))
