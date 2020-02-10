scrape = function(url, 
                  dl_target, 
                  force = FALSE, ## force redownload? 
                  delay = 0,
                  ... ## other parameters passed to curl_download
) {
    if (!file.exists(dl_target) | force) {
        message(url)
        curl::curl_download(url, dl_target, ...)
        if (delay > 0) {
            Sys.sleep(delay)
        }
    }
    return(TRUE)
}
