library(tidyverse)
library(readxl)
library(here)

data_dir = here('data')

all_identical = function(...) {
    identical(n_distinct(...), 1L)
}

xlsx = list.files(data_dir, 
                  pattern = '06_.*\\.xlsx', 
                  full.names = TRUE) |> 
    set_names(c('EL', 'CC', 'DJH'))

coding_df = map(xlsx, read_xlsx) |> 
    map(~ select(.x, -text)) |> 
    reduce(\(x, y) full_join(x, y, 
                             by = c('comment_id', 'url'))) |> 
    rowwise() |> 
    mutate(identical = 
               all_identical(c_across(starts_with('support'))
               )) |> 
    ungroup()

coding_df |> 
    filter(!identical) |> 
    view()



TODO: coding against PCA

write_csv(coding_df, here(data_dir, '07_coding.csv'))
