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

csv_name <- 'global_ufpallv31_v2'   #global_ufpallv31_v1Extra3Citiesv2: without AMS, COP winsorized

####### SLR (pooled model withou deconv) #########
seed <- 123
source("../expanse_multiyear/src/00_fun_create_fold.R")
df_all2 <- df_all2[!is.na(df_all2$UFPmean),]
df_all2$year <- 2019
tuneRF_b <- T
df_all2 <- create_fold(df_all2, seed, strt_group=c("lcz", "city", "passes", "driving_days"), 
                       nfold = nfold)
y_varname <- "UFPmean"

df_all2 <- df_all2[complete.cases(df_all2[, c(y_varname, allPred_vars)]),]


source("src/fun_mclapplySLR.R")
if(!dir.exists("data/workingData/5fold")) dir.create("data/workingData/5fold")
fivefoldCVResult <- mclapply(1:nfold, function(fold_i){
  source("src/fun_mclapplySLR.R")
  trainSLRfold(csv_name, fold_i, df_all2, allPred_vars, outDir='data/workingData/5fold/')
}, mc.cores = 5)
saveRDS(fivefoldCVResult, paste0("data/temp/", csv_name, "_5-foldCV_results", '.Rdata'))



####### SLR city specific model #########
seed <- 123
source("../expanse_multiyear/src/00_fun_create_fold.R")
df_all2 <- df_all2[!is.na(df_all2$UFPmean),]
df_all2$year <- 2019
tuneRF_b <- T
df_all2 <- create_fold(df_all2, seed, strt_group=c("lcz", "city", "passes", "driving_days"), 
                       nfold = nfold)
y_varname <- "UFPmean"

source("src/fun_mclapplySLR.R")

cities <- unique(df_all2$city)%>%as.character
results <- mclapply(cities, function(city_sub) {
  csv_name <- paste0(city_sub, '_ufpallv31_v2')  
  # Filter data for the specific city
  city_data <- df_all2 %>% filter(city == city_sub)
  # Implement 5-fold CV
  fivefoldCVResult <- lapply(1:nfold, function(fold_i){
    source("src/fun_mclapplySLR.R")
    trainSLRfold(csv_name, fold_i, city_data, allPred_vars, outDir='data/workingData/5fold/')
  })
}, mc.cores = length(cities))

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
csv_name <- 'global_ufpallv31_v2'   #global_ufpallv31_v1Extra3Citiesv2: without AMS, COP winsorized

seed <- 123
source("../expanse_multiyear/src/00_fun_create_fold.R")
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFPmean),]
deconv_dfs$year <- 2019
tuneRF_b <- T
deconv_dfs <- create_fold(deconv_dfs, seed, strt_group=c("lcz", "city", "passes", "driving_days"), 
                       nfold = nfold)
y_varname <- "UFPmean"
deconv_dfs <- deconv_dfs[complete.cases(deconv_dfs[, c(y_varname, allPred_vars)]),]

source("src/fun_mclapplySLR.R")
if(!dir.exists("data/workingData/5fold")) dir.create("data/workingData/5fold")
mclapply(1:nfold, function(fold_i){
  source("src/fun_mclapplySLR.R")
  csv_name1 <- ('global_ufpallv31_v2_background')  #global_ufpallv31_v1Extra3Cities2_background
  csv_name2 <- ('global_ufpallv31_v2_local')  #global_ufpallv31_v1Extra3Cities2_local
  
  trainSLRfold(csv_name1, fold_i, deconv_dfs, allPred_vars, outDir='data/workingData/5fold/', obs_var="UFPdeconv")
  trainSLRfold(csv_name2, fold_i, deconv_dfs, allPred_vars, outDir='data/workingData/5fold/', obs_var = "UFP_diff")
}, mc.cores = 5)




####### Random forest (without hyperparameter tuning) #########
source("src/fun_mclapplyRF.R")
mclapplyRF_default(paste0(csv_name, "_defaultHyper"), df_all2, allPred_vars, seed=123, 
                    num_trees=500, m_try=50, vi_importance = "permutation", min_node_size = 20000,
                    output_dir="data/workingData/5fold/")

####### Random forest (hyperparameter tuning) #########
lapply(1:nfold, function(fold_i){
  csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
  #f# SLR: select predictors
  test_sub <- df_all2[df_all2$nfold==fold_i,]
  train_sub <- df_all2[-test_sub$index, ] #df_all2$index starts from 1 to the length.
  
  #------------------Above code is needed for all algorithms----------------------
  # #--------- RF: split data into train, validation, and test data--------
  print("--------------- RF ---------------")
  set.seed(seed)
  train_df <- train_sub
  test_df <- test_sub
  x_varname <- c(allPred_vars) #"x_trun", "y_trun"  ,  "cntr_code"
  print("RF predictors:")
  print(x_varname)
  ## LLO CV 
  if(tuneRF_b){
    #f# RF: tune hyperparameter
    hyper_grid <- expand.grid(
      mtry = seq(10, 80, by=20),
      ntrees = seq(100, 700, by=300),
      min.node.size = seq(4000, 20000, by=2000),
      OOB_RMSE = 0,
      OOB_R2 = 0,
      valid_RMSE = 0,
      valid_R2 = 0
    )

    source("src/fun_tune_rf_cluster.R")
    hyper_grid <- tune_rf_cluster(train_df, test_df, #valid_df,
                                  y_varname='UFPmean',
                                  x_varname,
                                  csv_name_fold, hyperGrid = hyper_grid, mc_cores=detectCores()-5)
  }
  #f# RF: train the model
  hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))
  source("../EXPANSE_algorithm/scr/fun_opt_rf.R")
  # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
  rf_result <- opt_rf(train_df, test_df,
                      y_varname='UFPmean',
                      x_varname = x_varname,
                      csv_name_fold, hyper_grid, tuneRF_b,
                      outputselect = c("UID", "rf", "obs", "res",
                                       "nfold", "index", "df_type"))
  source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
  plot_rf_vi(csv_name_fold, var_no = 15)
  # plot_rf_vi(paste0(csv_name_fold, "_50"), var_no = 10)
  # rf_result2$eval_test
  # Model Performance evaluation:
  rf_result$eval_train
  rf_result$eval_test
})

