library(tidyverse)
library(assertthat)

data_folder = file.path('..', 'data')

docs_csv = read_csv(file.path(data_folder, '06_docs.csv'))
docs_xlsx = readxl::read_excel(file.path(data_folder, 
                                         '06_docs_coded.xlsx'))

assert_that(identical(docs_csv$comment, docs_xlsx$comment), 
            msg = '06 PC docs are not identical')

ss_csv = read_csv(file.path(data_folder, '06_simple_sample.csv'))
ss_xlsx = readxl::read_xlsx(file.path(data_folder, 
                             '06_simple_sample_coded.xlsx'))

assert_that(identical(ss_csv$comment_id, ss_xlsx$comment_id), 
            msg = '06 SS docs are not identical')

