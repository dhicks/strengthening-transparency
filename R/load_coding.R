## Load the manual codes
load_manual_coding = function(filter_na = FALSE) {
    dataf = read_rds(here(data_dir, '07_coding.Rds')) |> 
        select(-starts_with('support'), 
               -starts_with('notes')) |> 
        mutate(support = case_when(
            mode == 'y' ~ 'support', 
            mode == 'n' ~ 'oppose', 
            TRUE ~ NA_character_
        )) |> 
        select(-mode)
    if (filter_na) {
        return(filter(dataf, !is.na(support)))
    } else {
        return(dataf)
    }
}

## Load the imputed codes for the full corpus
load_imputed_full = function() {
    here(data_dir, '08_imputed.parquet') |> 
        open_dataset() |> 
        rename(support = .pred_class) |> 
        select(-text)
}

## Load the imputed codes, filtering by a confidence threshold
load_imputed_fltd = function(threshold = .8) {
    load_imputed_full() |> 
        filter(.pred_oppose < 1 - threshold | .pred_oppose > threshold)
}

## Helper to map a function over the three datasets and return a combined dataframe
mapped = function(input, f) {
    input |> 
        map(collect) |> 
        map(f) |> 
        bind_rows(.id = 'coding')
}
mapped2 = function(input1, input2, f) {
    map2(input1, input2, f) |> 
        bind_rows(.id = 'coding') |> 
        mutate(coding = fct_inorder(coding))
}
