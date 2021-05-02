.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

Sys.setenv("__SITS_DEBUG__" = TRUE)
Sys.setenv("__SITS_RESUME__" = TRUE)

library(sits)
library(dplyr)



#---- Configuration ----

# Level model (this is for the BIOME)
classification_name <- "first_classification"
my_tiles            <- "041048"


## Level data (for list of tiles in the BIOME)
project_dir   <- "/home/alber.ipia/Documents/bdc_amazonia_tc"
out_dir <- paste0(project_dir, "/results/", classification_name)
cube_name <- paste0("amazonia-tc_LC8_30_16D_STK-1")
model_file    <- paste0("/home/alber.ipia/Documents/bdc_amazonia_tc/results/",
                        classification_name,
                        "/ml_model.rds")

stopifnot(file.exists(model_file))
stopifnot(dir.exists(out_dir))



#---- Classify ----

data_cube <- sits::sits_cube(source = "BDC",
                             name = cube_name,
                             url = "http://datacube-005.dpi.inpe.br:8010/stac/",
                             collection = "LC8_30_16D_STK-1",
                             tiles = my_tiles,
                             start_date = "2018-01-10",
                             end_date   = "2018-12-31")
probs <- sits::sits_classify(data_cube,
                             ml_model = readRDS(model_file),
                             memsize = 15,
                             multicores = 10,
                             output_dir = out_dir)
probs <- dplyr::mutate(probs,
                       processing = tibble::tibble(start_time = start_time,
                                                   end_time = Sys.time()))
saveRDS(probs, file = file.path(out_dir, paste0(cube_name, "_results.rds")))
