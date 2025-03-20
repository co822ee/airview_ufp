library(parallel)
library(sf)
library(tidyverse)
library(dplyr)
source("../EXPANSE_algorithm/scr/fun_call_lib.R")

tuneRF_b = T
nfold = 5

seed=123

in_dir <- 'data/raw/gee/'#/data/volume_2/airview_basel/
fnames <- list.files(in_dir, '.shp')
fnames <- fnames[grepl("v31", fnames)]
cities <- c("Basel", "Athens", "Barcelona", "Munich", "Lodz", "Rome")
# city <- cities[3]
