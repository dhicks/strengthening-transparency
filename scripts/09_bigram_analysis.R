library(tidyverse)

theme_set(theme_bw())
library(ggforce)
library(patchwork)
library(gt)

library(here)
library(arrow)
library(glue)

source(here('R', 'tab_out.R'))
source(here('R', 'extract_noun.R'))

## TODO: write out plots throughout

## Load data ----
data_dir = here('data')
out_dir = here('out')
source(here('R', 'load_coding.R'))

coded = list(manual = load_manual_coding(), 
             filtered = load_imputed_fltd(), 
             imputed = load_imputed_full())

map(coded, nrow)


## Load bigrams ----
load_bigrams = function(coded_df) {
    open_dataset(here(data_dir, 
                      '05_bigrams.parquet')) |> 
        inner_join(coded_df, by = 'comment_id') |> 
        filter(support %in% c('support', 'oppose')) |>
        collect() |>
        extract_noun()
}

bigrams = map(coded, load_bigrams)

bigrams |> 
    mapped(~ {.x |> 
            pull(comment_id) |> 
            n_distinct()})

n_docs = map(bigrams, ~ {.x |> 
        distinct(comment_id, support) |> 
        count(support, name = 'n_docs')})

## table: number of docs by support
n_docs_tab = bind_rows(n_docs, .id = 'dataset') |> 
    group_by(dataset) |> 
    mutate(perc = n_docs / sum(n_docs)) |> 
    ungroup() |> 
    pivot_wider(names_from = support, values_from = c(n_docs, perc)) |> 
    rowwise() |> 
    mutate(total = sum(c_across(starts_with('n_docs')))) |> 
    ungroup() |> 
    gt(rowname_col = 'dataset') |> 
    fmt_percent(starts_with('perc'), decimals = 0) |> 
    fmt_integer(c(starts_with('n_doc'), total)) |> 
    cols_merge(columns = ends_with('oppose'), 
               pattern = '{1} ({2})') |> 
    cols_merge(columns = ends_with('support'), 
               pattern = '{1} ({2})') |> 
    cols_label(n_docs_oppose ~ 'oppose', 
               n_docs_support ~ 'support') |> 
    tab_header('Document counts, by coding method and opposition/support')
n_docs_tab
tab_out(n_docs_tab, '09_n_docs')

## Noun analysis ----
## Pr(bigram is "science" | doc support)
noun_analysis = function(bigrams_df) {
    bigrams_df |> 
        count(support, comment_id, noun) |> 
        complete(nesting(support, comment_id), noun, fill = list(n = 0L)) |> 
        group_by(comment_id) |> 
        mutate(share = n / sum(n)) |> 
        ungroup()
}

bigram_noun_tab = map(bigrams, noun_analysis) |> 
    bind_rows(.id = 'dataset') |> 
    mutate(dataset = fct_inorder(dataset)) |> 
    group_by(dataset, noun, support) |> 
    summarize(n_docs = n_distinct(comment_id),
              across(share, 
                     lst(mean, median))) |> 
    ungroup() |> 
    pivot_wider(names_from = 'noun', 
                values_from = starts_with('share')) |> 
    gt(groupname_col = 'dataset', 
       rowname_col = 'support') |> 
    fmt_integer(n_docs) |> 
    cols_label(n_docs = 'N') |> 
    fmt_percent(c(contains('mean'), contains('median')), 
                decimals = 1) |> 
    tab_spanner('health', 
                columns = ends_with('health')) |> 
    cols_label(share_mean_health ~ 'mean', 
               share_median_health ~ 'median') |> 
    tab_spanner('science', 
                columns = ends_with('science')) |> 
    cols_label(share_mean_science ~ 'mean', 
               share_median_science ~ 'median') |> 
    tab_stub_indent(rows = everything(),
                    indent = 5) |> 
    tab_header('Bigram noun distribution, by coding method and noun') |> 
    tab_footnote('Pr(bigram noun is X | support)')
bigram_noun_tab
tab_out(bigram_noun_tab, '09_bigram_noun_tab')

noun_plot = function(bigrams_df, title) {
    bigrams_df |> 
        noun_analysis() |> 
        filter(noun == 'health') |>
        ggplot(aes(support, share, color = support)) +
        geom_violin(scale = 'width') +
        geom_sina(scale = 'width', alpha = .3) +
        stat_summary(geom = 'crossbar', fun = 'median', color = 'black',
                     show.legend = FALSE) +
        scale_y_continuous(name = 'share of bigrams that are health',
                           labels = scales::percent_format()) +
        scale_color_brewer(palette = 'Dark2', guide = 'none') +
        ggtitle(title)
}
bigrams[['manual']] |>
    noun_plot('test')

imap(bigrams, noun_plot) |> 
    wrap_plots(nrow = 1, guides = 'collect') +
    plot_layout(axes = 'collect') +
    plot_annotation('Bigram noun distribution') 

ggsave(here(out_dir, '09_bigram_noun_plot.png'), 
       height = 3, width = 6, bg = 'white', scale = 1.5)

## Bigram noun occurrence ----
## Pr(doc contains health/sci bigram | support)
noun_occurrence = function(bigram_df, doc_count_df) {
    bigram_df |> 
        distinct(support, comment_id, noun) |> 
        count(support, noun) |> 
        left_join(doc_count_df, by = 'support') |> 
        mutate(p = n / n_docs)
}
# debugonce(noun_occurrence)
# noun_occurrence(bigrams$manual, n_docs$manual)

noun_occ_tab = mapped2(bigrams, n_docs, noun_occurrence) |> 
    pivot_wider(names_from = 'noun', 
                values_from = c(n, p)) |> 
    gt(groupname_col = 'coding', 
       rowname_col = 'support') |> 
    fmt_integer(starts_with('n')) |> 
    cols_label(n_docs = 'total') |> 
    fmt_percent(c(contains('p')), 
                decimals = 0) |> 
    cols_merge(c(n_health, p_health), 
               pattern = '{1} ({2})') |> 
    cols_label(n_health ~ 'health') |> 
    cols_merge(c(n_science, p_science), 
               pattern = '{1} ({2})') |> 
    cols_label(n_science ~ 'science') |> 
    tab_stub_indent(rows = everything(),
                    indent = 5) |> 
    tab_header('Bigram noun occurrence, by coding method and noun') |> 
    tab_footnote('Pr(doc contains bigram noun | support)')
noun_occ_tab
tab_out(noun_occ_tab, '09_noun_occ_tab')

mapped2(bigrams, n_docs, noun_occurrence) |> 
    mutate(group = interaction(coding, support)) |> 
    ggplot(aes(support, p, group = group)) +
    geom_path(color = 'black', alpha = .25,
              position = position_dodge(width = .2)) +
    geom_point(aes(color = noun, shape = coding), 
               size = 2,
               position = position_dodge(width = .2)) +
    # facet_wrap(vars(coding)) +
    # scale_x_discrete(position = 'top') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'occurrence of bigram noun') +
    scale_color_brewer(palette = 'Set1') +
    ggtitle('Bigram noun occurrence')

ggsave(here(out_dir, '09_noun_occurrence.png'), 
       height = 4, width = 5, bg = 'white')


## Most/least common bigrams ----
## Pr(doc contains bigram | support)
bigram_occurrence = function(bigram_df, doc_count_df) {
    bigram_df |> 
        ## NB one-hot encoding / doc frequency
        distinct(support, comment_id, bigram) |> 
        count(support, bigram, name = 'df') |> 
        left_join(doc_count_df, by = 'support') |> 
        mutate(p = df / n_docs)
}
# bigram_occurrence(bigrams$manual, n_docs$manual)
p_df = map2(bigrams, n_docs, bigram_occurrence)

bigram_top_n = function(pr_bigram, n = 10) {
    pr_bigram |> 
        group_by(support) |> 
        top_n(n, wt = p) |> 
        arrange(support, desc(p)) |> 
        ungroup() |>
        print(n = 2 * n)
}
## TODO: readable table
# bigram_top_n(p_df, 10)
mapped(p_df, bigram_top_n)

## TODO: fix this? 
# ggplot(p_df, aes(fct_reorder(bigram, p, .desc = FALSE),
#                  p, color = support)) +
#     geom_point() +
#     coord_flip() +
#     facet_wrap(vars(coding))

## Likelihood ratio ----
log1kp = function(x, k = 5) {
    log10(x + 10^-k)
}

source(here('R', 'top_and_bottom.R'))

llr = function(pr_bigram) {
    pr_bigram |> 
        pivot_wider(id_cols = bigram, 
                    names_from = support,
                    values_from = p, 
                    values_fill = 0) |> 
        mutate(llr = log1kp(oppose) - log1kp(support))
}
p_df |> 
    map(llr) |> 
    map(~ top_and_bottom(.x, llr, 10))

## Top and bottom plot
llr_plot = function(llr_df, title, n = 15) {
    llr_df |> 
        top_and_bottom(llr, n) |> 
        mutate(side = case_when(
            side == 'top' ~ 'oppose', 
            side == 'bottom' ~ 'support')) |> 
        mutate(bigram = fct_reorder(bigram, llr), 
               side = fct_inorder(side)) |> 
        extract_noun() |> 
        ggplot(aes(bigram, ymax = llr, color = noun)) +
        geom_linerange(ymin = 0, linewidth = 1) +
        coord_flip() +
        facet_wrap(vars(side), ncol = 1, scales = 'free_y') +
        scale_color_brewer(palette = 'Set1') +
        labs(x = '', y = 'log likelihood ratio', title = title)
}
p_df |> 
    map(llr) |> 
    imap(llr_plot) |> 
    wrap_plots(nrow = 1, guides = 'collect') +
    plot_layout(axes = 'collect') +
    plot_annotation('Top bigrams, by log likelihood ratio', 
                    caption = 'log(x + 10^-5)')
ggsave(here(out_dir, '09_llr_top.png'), 
       height = 7, width = 8, bg = 'white', scale = 1.25)

# opts = options(pillar.sigfig = 10)
## For manual, medians are identical bc the median bigram appears in 1 opposing doc and no supporting docs
p_df |> 
    map(llr) |> 
    mapped(~ {.x |> 
            extract_noun() |> 
            group_by(noun) |> 
            summarize(llr = median(llr))})
p_df$manual |> 
    complete(support, bigram, fill = list(df = 0)) |> 
    group_by(support) |> 
    summarize(df = median(df))
log1kp(1/622) - log1kp(0)
# options(opts)

llr_by_noun_plot = function(llr_df, title) {
    llr_df |> 
        extract_noun() |> 
        mutate(bigram = fct_reorder(bigram, llr)) |> 
        ggplot(aes(noun, y = llr, color = noun)) +
        geom_violin(scale = 'width') +
        geom_sina(aes(label = bigram), scale = 'width') +
        # stat_summary(fun = median,
        #              geom = 'crossbar', 
        #              color = 'black') +
        geom_hline(yintercept = 0) +
        scale_color_brewer(palette = 'Set1', 
                           guide = 'none') +
        labs(y = 'log likelihood ratio\n←support                                 oppose→') +
        ggtitle(title)
}

p_df |> 
    map(llr) |> 
    imap(llr_by_noun_plot) |> 
    map(~ .x + scale_y_continuous(limits = c(-4, 4))) |> 
    wrap_plots(nrow = 1, guides = 'collect') +
    plot_layout(axis_titles = 'collect') +
    plot_annotation('Distribution of log likelihood ratios',
                    caption = 'log(x + 10^-5)')
ggsave(here(out_dir, '09_llr.png'), 
       height = 4, width = 6, scale = 1.5, bg = 'white')


## TODO: feed in the combined plot
# plotly::ggplotly()


## KWIC ----
source(here('R', 'kwic.R'))
text_ar = open_dataset(here('data', '03_text.parquet'))

## "very science" comes from "attacking the very science on which its regulations are"
kwic(text_ar, 'very science', window = 30)

## "less science" shows up where comments are concerned about reduced use of science in regulation
kwic(text_ar, 'less science')

## "well science" doesn't appear in corpus? 
kwic(text_ar, 'well science')

## "your own health," "my own health," "their own health"
kwic(text_ar, ' own health')

