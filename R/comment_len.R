comment_len = function(df = FALSE) {
    ar = here::here('data', '03_annotations') |> 
        arrow::open_dataset() |> 
        filter(!pos %in% c('PUNCT', 'SPACE', 'NUM')) |> 
        count(comment_id, name = 'len')
    if (!df) {
        return(ar)
    } else {
        return(collect(ar))
    }
}

