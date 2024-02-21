library(tidyverse)
library(readxl)
library(here)
library(irr)
library(openxlsx2)
library(assertthat)

data_dir = here('data')

all_identical = function(...) {
    identical(n_distinct(...), 1L)
}

xlsx = list.files(data_dir, 
                  pattern = '^06_.*\\.xlsx', 
                  full.names = TRUE) |> 
    set_names(c('EL', 'CC', 'DJH', 'JD'))

url_xwalk = xlsx['DJH'] |> 
    read_xlsx() |> 
    select(comment_id, url)

## https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/25635740#25635740
Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
        x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    match(x, ux) |> 
        tabulate() %>%
        {. == max(.)} %>% 
        magrittr::extract(ux, .)
}
# debugonce(Mode)
# Mode(c('n', 'n', 'NA', 'NA'))

coding_df = map(xlsx, readxl::read_xlsx) |>
    map(~ select(.x, -text)) |> 
    map(~ mutate(.x, support = 
                     if_else(support == 'NA', 'NA', 
                             tolower(support)))) |> 
    bind_rows(.id = 'coder') |> 
    pivot_wider(id_cols = c(comment_id), 
                names_from = coder, 
                values_from = c(support, notes)) |> 
    rowwise() |> 
    mutate(identical = all_identical(c_across(starts_with('support'))), 
           mode = list(Mode(c_across(starts_with('support'))))) |> 
    ungroup() |> 
    left_join(url_xwalk, by = 'comment_id') |> 
    select(comment_id, url, everything())

assert_that(identical(
    n_distinct(coding_df$comment_id), 
    nrow(coding_df)))

## IRR using Krippendorff's alpha
## Initial IRR (completely independent, no review) was 0.86
## After one round of revisions, 0.941
coding_df |> 
    select(starts_with('support')) |> 
    t() |> 
    kripp.alpha()

## After one round, discordance on only 32 comments
coding_df |>
    select(comment_id, mode,
           starts_with('support'), starts_with('notes'),
           identical) |>
    filter(!identical) |> view()

# TODO: coding against PCA

## Output: full coding results
write_rds(coding_df, here(data_dir, '07_coding.Rds'))
## And discordances
wb_workbook() |> 
    wb_add_worksheet() |> 
    wb_add_data_table(x = {coding_df |> 
            filter(!identical) |> 
            select(comment_id, url, mode, everything()) |> 
            mutate(url = magrittr::set_class(url, 'hyperlink')) |> 
            mutate(across(everything(), ~ replace_na(.x, '')), 
                   mode = map_chr(mode, str_flatten)) |> 
            as.data.frame()
    }) |> 
    wb_set_sheetview(zoom_scale = 180) |>
    wb_set_col_widths(cols = 1, widths = 25) |> 
    wb_save(file = here(data_dir, '07_discordances.xlsx'), 
            overwrite = FALSE)
