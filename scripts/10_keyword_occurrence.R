library(tidyverse)
theme_set(theme_minimal())
library(ggforce)
library(patchwork)
library(here)
library(arrow)
library(tictoc)

extract_noun = function(df, in_col = bigram, out_col = noun) {
    mutate(df, {{ out_col }} := str_split_i({{ in_col }}, '_', -1))
}

## Load data ----
data_dir = here('data')
source(here('R', 'load_coding.R'))

coded = list(manual = load_manual_coding(filter_na = TRUE), 
             filtered = load_imputed_fltd(), 
             imputed = load_imputed_full())

## Analyze keyword occurrence ----
## TODO: use lookaheads/lookbacks to avoid catching "Strengthening Transparency in Regulatory Science" and "Environmental Protection Agency"
kword_occurence = function(coded_df) {
    open_dataset(here('data', '03_text.parquet')) |>
        inner_join(coded_df, by = 'comment_id') |> 
        ## <https://github.com/apache/arrow/issues/40220>
        collect() |>
        mutate(science =     str_detect(text, regex('scien', ignore_case = TRUE)), 
               health =      str_detect(text, regex('health|medic', ignore_case = TRUE)), 
               environment = str_detect(text, regex('(?!(?-i)Environmental Protection Agency)(enviro|ecolog)\\w+', ignore_case = TRUE)), 
               economy =     str_detect(text, regex('business|econom', ignore_case = TRUE)), 
               regulation =  str_detect(text, regex('(?!(?-i)Regulatory Science)(regulat\\w*)', ignore_case = TRUE))) |> 
        group_by(support, comment_id) |> 
        summarize(across(c(science, health, 
                           environment, economy, regulation), 
                         any)) |> 
        ungroup() |> 
        group_by(support) |> 
        summarize(n = n(),
                  across(c(science, health, 
                           environment, economy, regulation), 
                         mean)) |> 
        arrange(support)
}
map(coded, kword_occurence) |> 
    map(collect) |> 
    bind_rows(.id = 'coding')

