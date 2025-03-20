# This file is to process the predictors' files from GEE

library(parallel)
library(sf)
library(dplyr)
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
## Process the predictor points from GEE (v1 script: fun_process_predDF.R)
cities <- c("Basel", "Athens", "Barcelona", "Munich", "Lodz", "Rome",  "Amsterdam", "Copenhagen", "Rotterdam")
process_predictors <- function(city){
  # city <- "Munich"
  # Input: 
  source("../expanse_cnossos_v2/src/snellius/src/fun_log.R")
  in_dirGEE <- "data/raw/Airview_extractedPredictors/"  
  # predictors extracted from GEE (using the orignal mean of mean data)
  local_pathfname <- paste0("data/processed/final_mobileData/", city, "_stat2_v3.rds")  
  # original mean of mean data (local)
  ######## Output: ########
  if(!dir.exists("data/temp/road_centroidPts/")) dir.create("data/temp/road_centroidPts/")
  predictor_pathfname <- paste0('data/temp/road_centroidPts/ptsPredictors_', city, '.Rdata')  
  # output predictor variables (cleaned)
  # spatialPts_pathfname <- paste0('data/temp/road_centroidPts/ptsUID_', city, '.Rdata')
  spatialPts_pathfname <- paste0('data/temp/road_centroidPts/ptsUID_', city, '.gpkg')              # This needs to be kept
  spatialLines_pathfname <- paste0('data/temp/road_centroidPts/linesUID_', city, '.Rdata')
  # output centroid points (sf dataframe) with UID and city available 
  bug_pathfname <- paste0('data/temp/bug.csv')
  
  fnames <- list.files(in_dirGEE, '.shp')
  fnames_sub <- fnames[grepl(city, fnames)]
  fnames_sub1 <- fnames_sub[grepl("extra", fnames_sub)] %>% sort
  fnames_sub2 <- fnames_sub[!grepl("extra", fnames_sub)] %>% sort
  
  predictors_list <- lapply(paste0(in_dirGEE, fnames_sub1), st_read) ##%>% do.call(rbind, .)
  predictors_list2 <- lapply(paste0(in_dirGEE, fnames_sub2), st_read) ##%>% do.call(rbind, .)
  
  predictors_df <- predictors_list %>% 
    do.call(rbind, .)# %>% 
    # st_transform(., st_crs(3035))
  predictors_df2 <- predictors_list2 %>% 
    do.call(rbind, .)# %>% 
    # st_transform(., st_crs(3035))
  
  # names(predictors_df)
  # names(predictors_df2)
  if(nrow(predictors_df)==nrow(predictors_df2)){
    pts_only <- predictors_df2 %>% dplyr::select(UID, city)
    eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
    pts_only_t <- pts_only %>% st_transform(., st_crs(eu_bnd))
    if(file.exists(spatialPts_pathfname)) file.remove(spatialPts_pathfname)
    st_write(pts_only_t, spatialPts_pathfname) #spatial data points
    
    predictors_df <- inner_join(predictors_df%>% 
                                  as.data.frame %>% 
                                  dplyr::select(-geometry), 
                                predictors_df2 %>% 
                                  as.data.frame %>% 
                                  dplyr::select(-geometry), by=c("UID", 'city'))
    predictors_df <- inner_join(predictors_df,
                                data.frame(min=1:6,
                                           roadtype=factor(c('highway', 'primary', 'secondary', 'tertiary','residential','unclassified'), 
                                                           levels=c('highway', 'primary', 'secondary', 'tertiary','residential','unclassified'))))
    
    nrow(predictors_df)
    names(predictors_df) <- gsub("__","_",names(predictors_df))
    predictors_df <- predictors_df %>% dplyr::select(-min)
    
    stat <- readRDS(local_pathfname)
    saveRDS(stat %>% mutate(city=city) %>% dplyr::select(UID, city), 
            spatialLines_pathfname)
    
    if(nrow(stat)==nrow(predictors_df)){ #39909 , 39926 for Athens
      saveRDS(predictors_df, predictor_pathfname)
    }else{
      # source("../expanse_cnossos_v2/src/snellius/src/fun_log.R")
      write_log(data.frame(city=city, bug='numbers of road segments are NOT aligned between local file and GEE export file',
                           script="fun_road_predictors.R"),
                bug_pathfname)
    }
  }else{
    
    # source("../expanse_cnossos_v2/src/snellius/src/fun_log.R")
    write_log(data.frame(city=city, bug='two export CSV files from GEE do not match with the nrow',
                         script="fun_road_predictors.R"), 
              bug_pathfname)
    
  }
}
lapply(cities, process_predictors)

################ need to produce the road segments by cities with UID ###################

## Athens' bug: numbers of road segments are NOT aligned between local file and GEE export file
stat[!stat$UID%in%predictors_df$UID,] %>% mapview # costlines
