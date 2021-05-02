library(dplyr)
library(ensurer)
library(lubridate)
library(purrr)
library(sits)



#--- Configuration ----

samples_file <- "/home/alber.ipia/Documents/bdc_amazonia_tc/data/samples/samples_amazonia_tc.rds"
model_file   <- "/home/alber.ipia/Documents/bdc_amazonia_tc/results/first_classification/ml_model.rds"
stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))

ml_method <- sits::sits_rfor(trees = 2000)

source("./scripts/00_util.R")



#---- Script ----

samples_tb <- samples_file %>%
    readRDS() %>%
    is_sits_valid() %>%
    dplyr::filter(label %in% c("FORMAÇÃO FLORESTAL",
                               "LAVOURA TEMPORÁRIA",
                               "PASTAGEM"))
samples_tb %>%
    dplyr::count(label)
# FILTERED
# label                    n
# 1 FORMAÇÃO FLORESTAL  1284
# 2 LAVOURA TEMPORÁRIA   405
# 3 PASTAGEM             482
#
# ORIGINAL
# label                            n
# 1 AFLORAMENTO ROCHOSO              4
# 2 FORMAÇÃO CAMPESTRE              26
# 3 FORMAÇÃO FLORESTAL            1284
# 4 FORMAÇÃO SAVÃNICA               44
# 5 INFRAESTRUTURA URBANA            5
# 6 LAVOURA TEMPORÁRIA             405
# 7 MINERAÇÃO                        3
# 8 NÃO OBSERVADO                    6
# 9 OUTRA ÁREA NÃO VEGETADA          7
# 10 OUTRA FORMAÇÃO NÃO FLORESTAL    21
# 11 PASTAGEM                       482
# 12 RIO, LAGO E OCEANO              16

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(object = ml_model,
        file = model_file)
