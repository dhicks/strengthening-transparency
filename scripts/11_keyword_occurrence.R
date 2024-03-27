library(tidyverse)
theme_set(theme_minimal())
library(ggforce)
library(patchwork)
library(gt)

library(here)
library(arrow)
library(glue)
library(tictoc)

source(here('R', 'extract_noun.R'))
source(here('R', 'tab_out.R'))
out_dir = here('out')

## Load data ----
data_dir = here('data')
source(here('R', 'load_coding.R'))

coded = list(manual = load_manual_coding(filter_na = TRUE), 
             filtered = load_imputed_fltd(), 
             imputed = load_imputed_full())

## Analyze keyword occurrence ----
## TODO: use lookaheads/lookbacks to avoid catching "Strengthening Transparency in Regulatory Science"
kword_occurence = function(coded_df) {
    open_dataset(here('data', '03_text.parquet')) |>
        inner_join(coded_df, by = 'comment_id') |> 
        ## <https://github.com/apache/arrow/issues/40220>
        collect() |>
        mutate(science = str_detect(text, 
                                    regex('(?!(?-i)Regulatory Science)(science)', 
                                          ignore_case = TRUE)), 
               health = str_detect(text, regex('health|medic', 
                                               ignore_case = TRUE)), 
               environment = str_detect(text, 
                                        regex('(?!(?-i)Environmental Protection Agency)(enviro|ecolog)', 
                                              ignore_case = TRUE)), 
               economy = str_detect(text, 
                                    regex('business|econom', 
                                          ignore_case = TRUE)), 
               regulation = str_detect(text, 
                                       regex('(?!(?-i)Regulatory Science)(regulat)', 
                                             ignore_case = TRUE))) |> 
        group_by(support, comment_id) |> 
        summarize(across(c(science, health, 
                           environment, economy, regulation), 
                         any)) |> 
        ungroup() |> 
        group_by(support) |> 
        summarize(n = n(),
                  across(c(science, health, 
                           environment, economy, regulation), 
                         lst(sum, mean))) |> 
        arrange(support)
}
kword_occurence(coded$manual)

kword_df = map(coded, kword_occurence) |> 
    map(collect) |> 
    bind_rows(.id = 'coding')
kword_df

## N is greater than in 09 bc we're not limited to docs w/ detected bigrams
kword_tab = kword_df |> 
    gt(groupname_col = 'coding', 
       rowname_col = 'support') |> 
    fmt_integer(c(n, contains('sum'))) |> 
    cols_label(n = 'N') |> 
    fmt_percent(contains('mean'), decimals = 0) |> 
    cols_merge_n_pct(science_sum, science_mean) |> 
    cols_merge_n_pct(health_sum, health_mean) |> 
    cols_merge_n_pct(environment_sum, environment_mean) |> 
    cols_merge_n_pct(economy_sum, economy_mean) |> 
    cols_merge_n_pct(regulation_sum, regulation_mean) |> 
    cols_label_with(fn = ~ str_remove(.x, '_sum')) |> 
    tab_footnote('(?!(?-i)Regulatory Science)(science)', 
                 locations = cells_column_labels(c(science_sum))) |> 
    tab_footnote('health|medic', 
                 locations = cells_column_labels(c(health_sum))) |> 
    tab_footnote('(?!(?-i)Environmental Protection Agency)(enviro|ecolog)', 
                 locations = cells_column_labels(c(environment_sum))) |> 
    tab_footnote('business|econom', 
                 locations = cells_column_labels(c(economy_sum))) |> 
    tab_footnote('(?!(?-i)Regulatory Science)(regulat)', 
                 locations = cells_column_labels(c(regulation_sum))) |> 
    tab_stub_indent(rows = everything(),
                    indent = 5) |> 
    tab_header('Keyword occurrence, by coding method and support')
kword_tab
tab_out(kword_tab, '11_keyword_tab')
