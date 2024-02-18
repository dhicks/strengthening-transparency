top_and_bottom = function(dataf, value_var, n) {
    top = top_n(dataf, n, wt = {{ value_var }}) |> 
        mutate(side = 'top')
    bottom = top_n(dataf, -n, wt = {{ value_var }}) |> 
        mutate(side = 'bottom')
    
    bind_rows(top, bottom) |>
        arrange(desc({{ value_var}} ), .by_group = TRUE)
}
