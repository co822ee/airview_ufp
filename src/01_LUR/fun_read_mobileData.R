read_mobileData <- function(){
  ####### Read mobile data ###########
  campaignRoadInfo_pathfname <- "data/temp/stat_new/campaignRoadInfo_cities.Rdata"
  dfObsPredictors_pathfname <- "data/temp/stat_new/meanOfmean_predictors_cities.Rdata"
  df_obsPred <- readRDS(dfObsPredictors_pathfname)
  df_campRoad <- readRDS(campaignRoadInfo_pathfname)
  
  names(df_campRoad)[names(df_campRoad)%in%names(df_obsPred)]
  df_all <- inner_join(df_campRoad, df_obsPred)
  df_all2 <- df_all %>% filter(campaign_text=="both")
  df_all2 <- lapply(unique(df_all2$city), function(city_target){
    # any(duplicated((df_all2 %>% filter(city==city_target))$UID))
    # which(duplicated((df_all2 %>% filter(city==city_target))$UID))[1]
    df_filtered <- df_all2 %>% filter(city==city_target)
    df_filtered[!duplicated(df_filtered$UID),]
  })
  df_all2 <- do.call(rbind, df_all2)
  ########### predictor variables' namees ########
  select_predictor <- function(df_all, 
                               exc_names=c('system.index', 'obs', 'sta_code', 
                                           'component_caption', '.geo', 'year', 
                                           'cntr_code', 'xcoord', 'ycoord', 
                                           'sta_type')){
    
    pred_c <- names(df_all)[!(names(df_all)%in%exc_names)]
    pred_c
  }
  exc_names <- c(names(df_campRoad), "driving_days", "passes", 
                 "UFPmean", "BC1mean", "BC6mean", 
                 names(df_obsPred)[grepl("speed", names(df_obsPred))],
                 names(df_obsPred)[grepl("TROP", names(df_obsPred))],
                 names(df_obsPred)[grepl("omi", names(df_obsPred))],
                 names(df_obsPred)[grepl("MACC", names(df_obsPred))],
                 names(df_obsPred)[grepl("pop", names(df_obsPred))&!grepl("pop20", names(df_obsPred))],
                 names(df_obsPred)[grepl("dehm", names(df_obsPred))]
  )
  allPred_vars <- select_predictor(df_all2, exc_names) %>% 
    sort
  df_all2 <- df_all2 %>% as.data.frame() %>% ungroup() #%>% dplyr::select(-geom)
  df_all2$index <- 1:nrow(df_all2)
  list(df_all2, allPred_vars)
  # summary(df_all2)
}