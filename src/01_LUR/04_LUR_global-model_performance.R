#### evaluate the model performance  ####
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/01_LUR/fun_read_mobileData_v1.R")
source("src/01_LUR/fun_read_deconv.R")

data_lists <- read_mobileData()
df_all2 <- data_lists[[1]] 
deconv_dfs <- mclapply(cities, read_deconv, df_original=df_all2, mc.cores = length(cities))
deconv_dfs <- do.call(rbind, deconv_dfs)


##### global (pooled) models with and without deconvolution ######
source("src/fun_eval_slr_perfm.r")
background_df <- eval_slr_perfm(deconv_dfs, "UFPdeconv", "global_ufpallv31_v1Extra3Cities3_background") #72689
local_df <- eval_slr_perfm(deconv_dfs, "UFP_diff", "global_ufpallv31_v1Extra3Cities3_local") #72629
pooled_df <- eval_slr_perfm(deconv_dfs, "UFPmean", "global_ufpallv31_v1Extra3Citiesv3") 
all_df <- inner_join(background_df %>% 
                        dplyr::select(index, slr) %>% 
                        rename(UFPdeconv=slr), 
                    local_df %>% 
                        dplyr::select(index, slr) %>% 
                        rename(UFP_diff=slr), 
                    by="index")
all_df <- all_df %>% mutate(slr_deconv=UFPdeconv+UFP_diff)
all_df <- all_df %>% inner_join(., deconv_dfs %>% dplyr::select(index, UFPmean))

error_matrix(all_df$UFPmean, all_df$slr_deconv) %>% 
    as.data.frame() %>% 
    mutate(.=round(., 2)) %>% 
    rename(deconv=".")
error_matrix(pooled_df$obs, pooled_df$slr) %>% 
    as.data.frame() %>% 
    mutate(.=round(., 2)) %>% 
    rename(NoDeconv=".")

summary(pooled_df$slr)
summary(pooled_df$obs)

####### global (pooled) models with and without deconvolution (with interaction) ###
## Please refer to script 04_LUR_global-spatial_indicator