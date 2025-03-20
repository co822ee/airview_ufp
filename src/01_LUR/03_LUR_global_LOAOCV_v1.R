source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/01_LUR/fun_read_mobileData_v1.R")
data_lists <- read_mobileData(T)
df_all2 <- data_lists[[1]] 
allPred_vars <- data_lists[[2]] 
df_all2_xy <- lapply(unique(df_all2$city), function(city_sub){
  pts <- st_read(paste0("data/temp/road_centroidPts/ptsUID_", city_sub, ".gpkg"))
  df_all2 %>% 
    filter(city==city_sub) %>% 
    inner_join(cbind(pts %>% as.data.frame() %>% dplyr::select(-geom), pts %>% st_coordinates()), .)
}) %>% do.call(rbind, .)
df_all2_xy <- df_all2_xy %>% mutate(XY=X*Y)
allPred_vars_xy <- c(allPred_vars, "X", "Y")
allPred_vars_xy2 <- c(allPred_vars, "X", "Y", "XY")
source("src/01_LUR/fun_regroup.R")
df_all2 <- regroup_spatial_indicators(df_all2)
##### conduct leave one city out #####
cities <- unique(df_all2$city)
cvs_str1 <- "ufpLOAOCV_ufpallv31_v2"
csv_names <- paste0(cvs_str1, cities)
out_dir <- "data/workingData/LOAOCV/"
if(!dir.exists(out_dir)) dir.create(out_dir)

loaocv_results <- mclapply(seq_along(cities), function(i){
  csv_name <- csv_names[i]
  city_sub <- cities[i]
  obs_var <- "UFPmean"
  df_all2$index <- 1:nrow(df_all2)
  train_df <- df_all2 %>% dplyr::filter(city!=city_sub) %>% ungroup %>% as.data.frame()
  test_df <- df_all2 %>% dplyr::filter(city==city_sub) %>% ungroup %>% as.data.frame()
  
  source("src/fun_mclapplySLR.R")
  if(!file.exists(paste0(out_dir, "SLR_summary_model_", csv_name, ".csv"))){
    slr_lists <- trainSLRAll(csv_name, train_sub=train_df, allPred_vars, testB=T, test_df, 
                             outDir=out_dir, obs_var=obs_var)
    slr_lists
  }
}, mc.cores = length(cities))
saveRDS(loaocv_results, paste0("data/temp/", cvs_str1, "5-foldCV_results", '.Rdata'))

listcsvs <- list.files(out_dir, paste0("SLR_result_all_", cvs_str1), full.names = T) %>% 
  lapply(., read.csv)

csv_tests <- listcsvs %>% 
  do.call(rbind, .) %>% 
  filter(df_type=='test')
csv_tests <- csv_tests[!(is.na(csv_tests$obs)|is.na(csv_tests$slr)),]
out_err <- function(csv_df, model_str){
  err_df <- error_matrix(csv_df$obs, csv_df$slr) %>% 
    as.data.frame() %>% 
    mutate(.=round(., 2)) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(model=model_str)
  err_df
}
out_err(csv_tests, "LOAOCV")

unique(csv_tests$city) %>% 
  lapply(., function(city_sub){
    df_er <- csv_tests %>% filter(city==city_sub)
    
    error_df <- round(data.frame(error_matrix(df_er$obs, df_er$slr)), 3)[c(1,2,7, 8),]
    error_df <- data.frame(t(data.frame(error_df)))
    colnames(error_df) <- c('RMSE','RRMSE','MSE-R2','R2')
    error_df$city <- city_sub
    error_df
  }) %>% do.call(rbind,.)


############ 1) Global model (with deconvolution) ###############
### updates
# Prepare the environment and data
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/01_LUR/fun_read_mobileData_v1.R")
source("src/01_LUR/fun_read_deconv.R")
source("src/01_LUR/fun_prep_deconv_variables.R")
data_lists <- read_mobileData(T)
df_all2 <- data_lists[[1]] 
allPred_vars <- data_lists[[2]] 
source("src/01_LUR/fun_regroup.R")
df_all2 <- regroup_spatial_indicators(df_all2)

cities <- unique(df_all2$city)%>%as.character
deconv_dfs <-  mclapply(cities, read_deconv_define_movingWindow, 
                        df_original=df_all2, optimal_roll_window=400, 
                        mc.cores = length(cities))

deconv_dfs <- do.call(rbind, deconv_dfs)
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFPdeconv),]
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFP_diff),]

## Select the variables
pred_lists <- prep_deconv_variables(allPred_vars, bufNo = 1000,
                                    background_varNoBuf=c("anbh", "CTMufp", "lcz", "precip", "pressure", "temp", "wind", "zoneID", "alt10_enh", "X", "Y"),
                                    local_varNoBuf=c("roadtype", "AADT_onRoad", "pop2020"))
var_background <- pred_lists[[1]]
var_local <- pred_lists[[2]]
var_local <- var_local[!grepl("AADT_onRoad", var_local)]
cities <- unique(deconv_dfs$city)
cvs_str1 <- "ufpLOAOCV_ufpallv31_v2"
csv_names <- paste0(cvs_str1, cities, "_background")
csv_names2 <- paste0(cvs_str1, cities, "_local")
out_dir <- "data/workingData/LOAOCV/"
if(!dir.exists(out_dir)) dir.create(out_dir)

mclapply(seq_along(cities), function(i){
  csv_name <- csv_names[i]
  csv_name2 <- csv_names2[i]
  city_sub <- cities[i]
  obs_var <- "UFPmean"
  df_all2$index <- 1:nrow(df_all2)
  train_df <- deconv_dfs %>% dplyr::filter(city!=city_sub) %>% ungroup %>% as.data.frame()
  test_df <- deconv_dfs %>% dplyr::filter(city==city_sub) %>% ungroup %>% as.data.frame()
  
  source("src/fun_mclapplySLR.R")
  if(!file.exists(paste0(out_dir, "SLR_summary_model_", csv_name, ".csv"))){
    slr_lists1 <- trainSLRAll(csv_name, train_sub=train_df, var_background, testB=T, test_df, 
                             outDir=out_dir, obs_var="UFPdeconv")
    slr_lists2 <- trainSLRAll(csv_name2, train_sub=train_df, var_local, testB=T, test_df, 
                             outDir=out_dir, obs_var="UFP_diff")
  }
}, mc.cores = length(cities))
