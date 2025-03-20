# fun_03_ptsToRoads_combined.R
ptsToRoads_combined <- function(cityName, in_dir, out_dir="data/processed/"){
  # in_dir
  # cityName
  # out_dir="data/processed/"
  out_fname <- paste0(out_dir, cityName, '_rawPtsLines_v3.gpkg')
  if(!file.exists(out_fname)){
    filenames <- list.files(in_dir, cityName)
    filenames <- filenames[grepl("_subv3", filenames)]
    
    infnames <- paste0(in_dir, filenames)
    
    ## Read in all the files
    source("src/fun_read_multiSF.R")
    sfsAll <- read_multiSF(infnames)
    ##
    
    st_write(sfsAll, out_fname)
  }
}


