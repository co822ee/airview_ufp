read_mobileData <- function(excAADT=F){
  ####### Read mobile data ###########
  campaignRoadInfo_pathfname <- "data/temp/stat_new/campaignRoadInfo_cities.Rdata"
  dfObsPredictors_pathfname <- "data/temp/stat_new/meanOfmean_predictors_cities.Rdata"
  df_obsPred <- readRDS(dfObsPredictors_pathfname)
  df_campRoad <- readRDS(campaignRoadInfo_pathfname)
  source("src/01_LUR/fun_find_buf_varnames.R")

  ## Remove variables of ind_ and por_ with small buffer sizes (<1km)
  exc_ind_names <- find_buf_varnames("ind_", names(df_obsPred), 1000, "<")
  exc_por_names <- find_buf_varnames("por_", names(df_obsPred), 1000, "<")

  df_obsPred_exc <- df_obsPred %>% select(-all_of(c(exc_por_names, exc_ind_names)))
  

  df_all <- inner_join(df_campRoad, 
                       as.data.frame(df_obsPred_exc) )
  df_all2 <- df_all %>% filter(campaign_text=="both")
  df_all2 <- lapply(unique(df_all2$city), function(city_target){
    # any(duplicated((df_all2 %>% filter(city==city_target))$UID))
    # which(duplicated((df_all2 %>% filter(city==city_target))$UID))[1]
    df_filtered <- df_all2 %>% filter(city==city_target)
    df_filtered[!duplicated(df_filtered$UID),]
  })
  df_all2 <- do.call(rbind, df_all2)

  ## Add building height related predictor variables
  new_predictors_f <- list.files("data/raw/gee/", pattern="*.csv", full.names=T)
  remove_geo_sysIndex <- function(sub_str, fnames){
    new_predictors_f1 <- fnames[grepl(sub_str, fnames)]
    new_predictors1  <- lapply(new_predictors_f1, read.csv) %>% 
        bind_rows() %>% 
        dplyr::select(-".geo", -"system.index")
    if(sub_str=="stat2pts_v31extra20240621"&!("lcz"%in%names(new_predictors1))){
      new_predictors1 <- new_predictors1 %>% dplyr::rename(lcz=b1) #local climate zone data
    }
    new_predictors1
  }
 
  new_predictorsList <- lapply(c("stat2pts_v31extra20240621", "stat2pts_v31buildHeight"), remove_geo_sysIndex, 
        fnames= new_predictors_f)
  new_predictors <- Reduce(inner_join, new_predictorsList)
  ## rename the airport variables name
  new_predictors <- new_predictors %>% rename_at(vars(starts_with("apor_2018_")), ~str_replace(., "apor_2018_", "apor_"))

  df_all2_new <- df_all2 %>% inner_join(., new_predictors, by=c("UID", "city"))
  ## Make lcz and zoneID and roadtype as factor
  df_all2_new$lcz <- factor(df_all2_new$lcz)
  df_all2_new$zoneID <- factor(df_all2_new$zoneID)
  df_all2_new$city <- factor(df_all2_new$city)
  df_all2_new$roadtype <- factor(df_all2_new$roadtype)
  # Remove supermarket
  df_all2_new <- df_all2_new %>% select(-matches("^Spm_"), -uwind, -vwind)
  ## Combine predictors: restaurant and fastfood
  # Combine the columns starting with "Fsf_" and "Rst_" by adding them with the same ending numbers
  fsf_cols <- names(df_all2_new)[grepl("^Fsf_", names(df_all2_new))]
  rst_cols <- names(df_all2_new)[grepl("^Rst_", names(df_all2_new))]
  
  # Ensure that the columns are matched correctly by their ending numbers
  for (fsf_col in fsf_cols) {
    suffix <- gsub("Fsf_", "", fsf_col)
    rst_col <- paste0("Rst_", suffix)
    if (rst_col %in% rst_cols) {
      combined_col_name <- paste0("FsfRst_", suffix)
      df_all2_new[[combined_col_name]] <- df_all2_new[[fsf_col]] + df_all2_new[[rst_col]]
    }
  }
  
  # Remove the original columns starting with "Fsf_" and "Rst_"
  df_all2_new <- df_all2_new %>% select(-matches("^Fsf_"), -matches("^Rst_"))

  # Remove variables of FstRst_ with small buffer sizes (<500m)
  source("src/01_LUR/fun_find_buf_varnames.R")
  exc_food_names <- find_buf_varnames("FsfRst_", names(df_all2_new), 500, "<")
  df_all2_new <- df_all2_new %>% dplyr::select(-all_of(exc_food_names))
  ########### predictor variables' namees ########
  select_predictor <- function(df_all, 
                               exc_names=c('system.index', 'obs', 'sta_code', 
                                           'component_caption', '.geo', 'year', 
                                           'cntr_code', 'xcoord', 'ycoord', 
                                           'sta_type', 'zoneID')){
    
    pred_c <- names(df_all)[!(names(df_all)%in%exc_names)]
    pred_c
  }
  exc_names <- c(names(df_campRoad)[!grepl("city", names(df_campRoad))], "geom", "driving_days", "passes", 
                 "UFPmean", "BC1mean", "BC6mean", "zoneID",
                 names(df_obsPred)[grepl("speed", names(df_obsPred))],
                 names(df_obsPred)[grepl("TROP", names(df_obsPred))],
                 names(df_obsPred)[grepl("omi", names(df_obsPred))],
                 names(df_obsPred)[grepl("MACC", names(df_obsPred))],
                 names(df_obsPred)[grepl("pop", names(df_obsPred))&!grepl("pop20", names(df_obsPred))],
                 names(df_obsPred)[grepl("dehm", names(df_obsPred))]
  )
  df_all2_new <- df_all2_new %>% rename(AADT_onRoad=AADT_resNo)
  allPred_vars <- select_predictor(df_all2_new, exc_names) %>% 
    sort
  df_all2_new <- df_all2_new %>% as.data.frame() %>% ungroup()
  df_all2_new$index <- 1:nrow(df_all2_new)
  ######### add the negative direction of effect for SLR #####
  df_all2_new <- df_all2_new %>% 
      mutate_at(vars(starts_with("nat_"), starts_with("ugr_"), starts_with("precip"), "wind", "alt10_enh"), list(~ . * -1))
  if(excAADT){
    allPred_vars <- allPred_vars[!grepl("AADT_onRoad", allPred_vars)]
  }
  list(df_all2_new, allPred_vars)
  # summary(df_all2)
}
