## <https://juliasilge.com/blog/tidy-word-vectors/>
library(tidyverse)
library(tidytext)
library(widyr)
library(irlba)

library(arrow)
library(here)

word_vec_file = here('data', '10_word_vec.Rds')

## SVD step is rather slow, so we'll only rebuild the model if necessary
if (!file.exists(word_vec_file)) {
    ## Load data ----
    ## <https://universaldependencies.org/u/pos/>
    ann = open_dataset(here('data', '03_annotations')) |> 
        filter(pos %in% c('ADJ', 'NOUN', 'VERB', 'ADV')) |> 
        mutate(token = str_c(lemma, '_', pos)) |> 
        select(comment_id, doc_id, sentence_id, token)
    
    ## 20,787 comments
    ann |> 
        pull(comment_id) |> 
        n_distinct()
    
    ## Unigram probabilities ----
    unigram_p = ann |> 
        count(token) |> 
        collect() |> 
        mutate(p = n / sum(n))
    
    ## 78,744
    nrow(unigram_p)
    
    ## Co-occurrence probabilities and PMI ---
    coocc_p = ann |> 
        ## Construct unique sentence IDs
        mutate(context_id = str_c(doc_id, '-', sentence_id)) |> 
        select(context_id, token) |> 
        collect() |> 
        pairwise_count(token, context_id, 
                       diag = TRUE, sort = TRUE) |> 
        filter(n > 20) |> 
        mutate(p = n / sum(n)) |> 
        as_arrow_table()
    
    ## 8,364
    c(pull(coocc_p, item1), 
      pull(coocc_p, item2)) |> 
        n_distinct()
    
    pmi = coocc_p |> 
        left_join(unigram_p, 
                  by = join_by(item1 == token), 
                  suffix = c('', '1')) |> 
        left_join(unigram_p, 
                  by = join_by(item2 == token), 
                  suffix = c('', '2')) |> 
        mutate(pmi = log10(p / p1 / p2)) |> 
        select(item1, item2, p, p1, p2, pmi)
    
    # pmi |> 
    #     filter(item1 == 'science_NOUN', 
    #            str_detect(item2, '_NOUN')) |> 
    #     collect()
    # 
    # pmi |> 
    #     filter(item1 == 'environment_NOUN') |> 
    #     collect()
    
    ## SVD ----
    pmi_sparse = pmi |> 
        collect() |> 
        cast_sparse(item1, item2, pmi)
    
    pmi_svd = irlba(pmi_sparse, 256, maxit = 1e3)
    
    w_vecs = pmi_svd$u
    
    rownames(w_vecs) = rownames(pmi_sparse)
    
    ## Write out model ----
    write_rds(w_vecs, word_vec_file)
} else {
    w_vecs = read_rds(word_vec_file)
}

## Functions for searching for potential synonyms ----
vocab = rownames(w_vecs)

search_synonyms = function(target, 
                           word_vectors = w_vecs) {
    selected_vector = word_vectors[target,]
    {word_vectors %*% word_vectors[target,]} |> 
        magrittr::extract(,1) |> 
        enframe(name = 'token', value = 'similarity') |> 
        arrange(desc(similarity))
}
# search_synonyms('health_NOUN')

get_variants = function(pattern, n = 10, filter_pos = TRUE) {
    matches = str_subset(vocab, pattern)
    all_sims = map(matches, search_synonyms) |> 
        set_names(matches) |> 
        bind_rows(.id = 'match')
    if (filter_pos) {
        pos = function(string) {
            str_split_i(string, '_', -1)
        }
        all_sims = filter(all_sims, pos(match) == pos(token))
    }
    all_sims |> 
        group_by(match) |>
        top_n(n, similarity)
}

## Actually do the searching ----
## Science: 'scien'
## no synonyms that need to be included
## exclude: conscience, epascientificresearchtransperancyplan.pdf, Strengthening Transparency in Regulatory Science
## scientifically_ADV is most similar to right_ADV, correctly_ADV, importantly_ADV
get_variants('scien', n = 5) |> view(title = 'scien')

## Health: 'health|medic'
## no synonyms that need to be included
## exclude: publichealth.lacounty.gov
## healthy_ADJ is most similar to outdoor_ADJ, clean_ADJ, free_ADJ, young_ADJ
get_variants('health|medic', n = 5) |> view(title = 'health')

## Environment: 'enviro|ecolog'
## no synonyms to include
## exclude: Environmental Protection Agency
get_variants('enviro|ecolog', n = 5) |> view(title = 'environment')

## Economy: 'business|econom'
## include: industry
## exclude: socioeconomic, econometric
get_variants('business|econom|industr', n = 5) |> view(title = 'economy')

## Regulation: 'regulat'
## include: cost? 
## exclude: unregulated, regulations.gov
get_variants('regulat', n = 5) |> view(title = 'regulation')
