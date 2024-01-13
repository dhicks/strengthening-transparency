calculate_H = function(dtm, document, token, n, 
                       return_tokenwise = TRUE) {
    document = enquo(document)
    token = enquo(token)
    n = enquo(n)
    
    n_docs = dtm |> 
        count(!!document) |> 
        nrow()
    max_H = log2(n_docs)
    
    dtm |> 
        group_by(!!token) |> 
        mutate(p = !!n / sum(!!n), 
               H_term = -p*log2(p)) |> 
        summarize(n = sum(n),
                  H = sum(H_term)) |> 
        mutate(delta_H = max_H - H, 
               ndH = log10(n)*delta_H)
}
