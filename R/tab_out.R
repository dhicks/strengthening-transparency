tab_out = function(gt_tab, filename) {
    gtsave(gt_tab, here(out_dir, glue('{filename}.png')))
    write_rds(gt_tab, here(out_dir, glue('{filename}.Rds')))
}
## tab_out(n_docs_tab, '09_n_docs')

