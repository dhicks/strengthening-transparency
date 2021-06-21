#' Check whether the API key is defined.
api_key_check = function() {
    if (!exists('api_key')) {
        stop('api_key not defined.  Register for a key at <https://api.data.gov/signup/>.')
    }
}

#' Retrieve the documents associated with a docket
#'
#' @note No paging implemented here
#'
#' @param docket_id Docket identifier, eg, `'EPA-HQ-OA-2018-0259'`
#' @param verbose Display the API query
#' @return Data frame with document metadata
#' @export
doc_list = function(docket_id, verbose = FALSE)
{
    api_key_check()

    docket_query = str_c('https://api.regulations.gov/v4/documents',
                         '?',
                         'filter[docketId]=', docket_id, '&',
                         'page[size]=100', '&',
                         'api_key=', api_key)

    if (verbose) message(docket_query)

    docs = httr::GET(docket_query) %>%
        httr::content(as = 'text') %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        .$data %>%
        dplyr::rename_all(stringr::str_remove_all, 'attributes.')

    return(docs)
}


#' Number of comments associated with a document
#'
#' Queries the API and reports the associated comment count.
#'
#' @param doc_id The API identifier for the document, eg, `'090000648320bc9e'`
#' @return Value in the `meta:totalElements` field
#' @export
comment_counts = function(doc_id) {
    api_key_check()

    comment_count_query = stringr::str_c('https://api.regulations.gov/v4/comments', '?',
                                         'filter[commentOnId]=', doc_id, '&',
                                         'page[size]=5', '&',
                                         'api_key=', api_key)
    comment_count = httr::GET(comment_count_query) %>%
        httr::content(as = 'text') %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        {.$meta} %>%
        {.$totalElements}
    return(comment_count)
}


#' Get a single page of metadata with 250 comments
#'
#' @param doc_id API document identifier, eg, `'090000648320bc9e'`
#' @param page_num Page of results to retrieve
#' @param lastdate If not `NULL`, only comments modified on or after this date-time will be included in search results. Used for paging through blocks of 5000 comments.
#' @param verbose Display the API query
#' @return A data frame containing the comment metadata
get_comment_page = function(doc_id,
                            page_num = 1L,
                            lastdate = NULL,
                            verbose = FALSE) {
    api_key_check()

    comment_page_query = stringr::str_c('https://api.regulations.gov/v4/comments', '?',
                                        'filter[commentOnId]=', doc_id, '&',
                                        'page[size]=250', '&',
                                        'page[number]=', page_num, '&',
                                        'sort=lastModifiedDate,documentId', '&',
                                        'api_key=', api_key)
    if (!is.null(lastdate)) {
        comment_page_query = stringr::str_c(comment_page_query, '&',
                                            'filter[lastModifiedDate][ge]=', lastdate)
    }

    if (verbose) {
        message(comment_page_query)
    }

    content = httr::GET(comment_page_query) %>%
        httr::content(as = 'text') %>%
        jsonlite::fromJSON(flatten = TRUE)

    dataf = content$data %>%
        dplyr::rename_with(~ stringr::str_remove_all(., 'attributes.')) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(comment_on = doc_id)

    return(dataf)
}


#' Get metadata for a block of 5000 comments
#'
#' @param doc_id API document identifier, eg, `'090000648320bc9e'`
#' @param lastdate If not `NULL`, only comments posted on or after this date-time will be included in search results. Used for paging through blocks of 5000 comments.
#' @param block_size Number of comments in the block.  Used to determine how many pages need to be retrieved.
#' @param max_page Maximum number of pages to be retrieved; here mainly for debugging purposes
#' @param verbose Passed down to `get_comment_page`
#' @return A data frame containing the comment metadata
get_block = function(doc_id,
                     lastdate = NULL,
                     block_size = 5000,
                     max_pages = 20,
                     verbose = FALSE) {
    num_pages = min(block_size %/% 250 + 1, max_pages)

    dataf = furrr::future_map_dfr(1:num_pages,
                                  ~get_comment_page(doc_id,
                                                    .,
                                                    lastdate,
                                                    verbose = verbose),
                                  .progress = TRUE,
                                  .options = furrr::furrr_options(globals = 'api_key',
                                                                  packages = 'stringr'))

    return(dataf)
}


#' Subtract 5 hours and add 1 second to a date-time character string
#'
#' This function is used by `get_blocks` to page through the blocks of 5000 comments:  get the date-time for the last comment of the previous block, add 1 second, and use that as the threshold for the next block.  However, the API reports date-times in the format `YYYY-MM-DDTHH:mm:ssZ`, using UTC, while the query language only accepts the format `YYYY-MM-DD HH:mm:ss` using Eastern time(America/New York).
#'
#' @param json_date The date-time character string
#' @return A date-time character string, formatted as required by the API
wrangle_date = function(json_date)
{
    json_date %>%
        lubridate::ymd_hms() %>%
        lubridate::with_tz(tzone = 'America/New_York') %>%
        as.character()
}


#' Get metadata for all comments associated with a document
#'
#' Admittedly not the most perspicuous name
#'
#' @param doc_id API document identifier, eg, `'090000648320bc9e'`
#' @param total_comments Total number of comments associated with the document.  If `NULL`, API will be queried to get this number.
#' @param verbose Passed down to `get_block`
#' @return A data frame containing the comment metadata
#' @export
get_blocks = function(doc_id,
                      total_comments = NULL,
                      verbose = FALSE)
{
    if (is.null(total_comments)) {
        total_comments = comment_counts(doc_id)
    }

    block_max = 5000
    num_blocks = total_comments %/% block_max + 1

    for (i in 1:num_blocks) {
        if (i == 1) {
            block_size = min(total_comments, block_max)
            newdata = get_block(doc_id,
                                block_size = block_size,
                                verbose = verbose)
            dataf = newdata
        } else {
            block_size = min(total_comments - (i-1)*block_max, block_max)
            newdata = get_block(doc_id, lastdate, block_size, verbose = verbose)
            dataf = dplyr::bind_rows(dataf, newdata)
        }
        lastdate = newdata$lastModifiedDate %>%
            max() %>%
            wrangle_date()
    }

    return(dataf)
}

#' Scrape the data for a comment
#'
#' @note Scraping comments is strongly constrainted by the limit of 1000 queries/hour.  `scrape()` checks whether a file has already been downloaded, facilitating stopping and restarting this function.
#'
#' @param comment_id ID of the comment to scrape
#' @param scrape_folder Folder where the comment should be saved
#' @param ... Other options, passed to `scrape()`
#' @return Path to the scraped comment (invisible)
#' @export
scrape_comment = function(comment_id,
                          scrape_folder = file.path(data_folder,
                                                    str_c(prefix,
                                                          'comments')),
                          ...) {
    comment_query = str_c('https://api.regulations.gov/v4/comments/',
                          comment_id, '?',
                          'include=attachments', '&',
                          'api_key=', api_key)

    filename = str_c(comment_id, '.json')
    dl_path = file.path(scrape_folder, filename)

    scrape(comment_query, dl_path, ...)

    return(dl_path)
}


#' Parse scraped comment files
#'
#' @param file The scraped comment JSON file
#' @param verbose Print the path to the file being parsed
#' @return A data frame with the comment metadata and text.  If the comment has any attachments, metadata for these are given in the nested data frame `attachments`.
parser = function (file, verbose = FALSE) {
    null_to_na_recurse <- function(obj) {
        if (is.list(obj)) {
            obj <- jsonlite:::null_to_na(obj)
            obj <- lapply(obj, null_to_na_recurse)
        }
        return(obj)
    }

    if (verbose) message(file)

    json = jsonlite::read_json(file,
                               simplifyVector = TRUE,
                               simplifyDataFrame = TRUE,
                               flatten = TRUE) %>%
        null_to_na_recurse()

    dataf = json$data$attributes[c('docketId',
                                   'commentOnDocumentId',
                                   'objectId',
                                   'receiveDate',
                                   'duplicateComments',
                                   'title',
                                   'comment',
                                   'docAbstract')] %>%
        as_tibble() %>%
        mutate(id = json$data$id)

    included = json$included
    if (length(included) > 1) {
        attachments = included  %>%
            as_tibble() %>%
            rename_all(stringr::str_remove_all, 'attributes.') %>%
            mutate(fileFormats = map(fileFormats, as_tibble)) %>%
            unnest(fileFormats) %>%
            transmute(attachment_id = id,
                      docOrder,
                      url = get0('fileUrl', ifnotfound = NA),
                      format = tools::file_ext(url),
                      restrictReasonType)

        dataf$attachments = list(attachments)
        dataf$n_attachments = nrow(attachments)
    } else {
        dataf$n_attachments = 0L
    }

    return(dataf)
}
