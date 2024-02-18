# library(tidyverse)
# library(arrow)
# library(here)
# 
# text_ar = open_dataset(here('data', '03_text.parquet'))

kwic = function(corpus, target, text_col = text, window = 20) {
    query = glue::glue('.{0,[window]}[target].{0,[window]}', 
                       .open = '[', 
                       .close = ']') |> 
        as.character() |> 
        regex(ignore_case = TRUE)
    
    results = corpus |> 
        filter(str_detect({{ text_col }}, target)) |> 
        collect() |>
        mutate(extracts = str_extract_all({{ text_col }}, 
                                          query)) |> 
        select(-text) |> 
        unnest(extracts)
    class(results) = c('kwic', class(results))
    return(results)
}

print.kwic = function(self, 
                      extracts_col = extracts, id_col = 1) {
    pull(self, {{ extracts_col }}, {{ id_col }}) |> 
        print()
}
# debugonce(print.kwic)

# text_ar |>
#     head(1000) |>
#     kwic('available science')

