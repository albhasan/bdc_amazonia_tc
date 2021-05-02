.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

library(dplyr)
library(readr)
library(sits)



#---- set up level classification ----

# # ## Level model (this is for the BIOME)
classification_name <- "first_classification"
my_tiles            <- "041048"
project_dir   <- "/home/alber.ipia/Documents/bdc_amazonia_tc"
out_dir <- paste0(project_dir, "/results/", classification_name)
cube_name <- paste0("amazonia-tc_LC8_30_16D_STK-1")
model_file    <- "/home/alber.ipia/Documents/bdc_amazonia_tc/results/first_classification/ml_model.rds"

stopifnot(file.exists(model_file))
stopifnot(dir.exists(project_dir))
stopifnot(dir.exists(out_dir))



#---- Script ----

ml_model <- readRDS(model_file)
my_labels <- ml_model %>%
    environment() %>%
    magrittr::extract2("data") %>%
    dplyr::pull(label) %>%
    unique() %>%
    sort()

my_labels %>%
    readr::write_lines(file = file.path(out_dir, "labels.txt"))

data_cube <- sits::sits_cube(source = "BDC",
                             name = cube_name,
                             url = "http://datacube-005.dpi.inpe.br:8010/stac/",
                             collection = "LC8_30_16D_STK-1",
                             tiles = my_tiles,
                             start_date = "2018-01-10",
                             end_date   = "2018-12-31")

cube_date_range <- data_cube %>%
    sits::sits_timeline() %>%
    range()

prob_files <- "/home/alber.ipia/Documents/bdc_amazonia_tc/results" %>%
    file.path(classification_name) %>%
    list.files(pattern = paste0("^", cube_name, ".+tif$"),
               full.names = TRUE) %>%
    ensurer::ensure_that(length(.) == length(my_tiles),
                         err_desc = "Probability file not found!")

for (my_file in prob_files) {
    probs_cube <- sits::sits_cube(source = "PROBS",
                                  name = paste0(cube_name, "_",
                                                tools::file_path_sans_ext(basename(my_file))),
                                  satellite = "LANDSAT-8",
                                  sensor = "OLI",
                                  start_date = cube_date_range[1],
                                  end_date   = cube_date_range[2],
                                  probs_labels = my_labels,
                                  probs_files = my_file)

    bayesian <- sits::sits_smooth(probs_cube,
                                  type = "bayes",
                                  window_size = 5,
                                  multicores = 10,
                                  memsize = 2,
                                  output_dir = out_dir)

    sits::sits_label_classification(bayesian,
                                    multicores = 10,
                                    memsize = 2,
                                    output_dir = out_dir)
}
