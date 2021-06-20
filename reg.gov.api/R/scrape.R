#' Scrape a file using `curl`
#'
#' A wrapper around `curl::curl_download` that checks whether the target file already exists.
#'
#' @param url URL of the file to download
#' @param dl_target Path to the target file
#' @param force Force redownload?
#' @param delay Delay in seconds after completing download
#' @param verbose Display the URL being downloaded
#' @param ... Other parameters passed to `curl_download`
#' @return Value of `dl_target` (invisibly)
#' @export
scrape = function(url,
                  dl_target,
                  force = FALSE,
                  delay = 0,
                  verbose = FALSE,
                  ...
) {
    if (!file.exists(dl_target) | force) {
        if (verbose) {
            message(url)
        }
        curl::curl_download(url, dl_target, ...)
        if (delay > 0) {
            Sys.sleep(delay)
        }
    }
    return(invisible(dl_target))
}
