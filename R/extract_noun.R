extract_noun = function(df, in_col = bigram, out_col = noun) {
    mutate(df, {{ out_col }} := str_split_i({{ in_col }}, '_', -1))
}
