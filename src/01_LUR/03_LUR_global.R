# 20240625 updates: 
#### global_ufpallv31_v1Extra3Cities: with 3 cities (9 in total)
#### global_ufpallv31_v1Extra3Cities_background
#### global_ufpallv31_v1Extra3Cities_local
#### 2) with direction of effects added
#### 3) include airports (with buffer >= 1km) and local climate zone datat
#### 4) exclude industrial areas and port with large buffer sizes (>=1km)
#### 5) exclude uwind and vwind variables
#### 6) exclude supermarkets, zoneID
#### 7) make roadtypes as factors
#### 8) make zoneID as factors 
#### 9) use different subset of variables for the deconvolution methods
#### notes: https://www.notion.so/UFP-meeting-20240613-dc62f2d05d1546119b8d7795c2614552
# dataset update (v1, 20240520) 
# global_ufpallv31_v1: without 3 extra cities, 
# global_ufpallv31_v1_background
# global_ufpallv31_v1_local

# 1) exclude fastfood and rst with small buffer (<500m)
# global_ufpallv31_v1Extra3Citiesv2

############### 2. SLR (global model) ###############
############ 1) Global model ###############
# Prepare the environment and data
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
csv_name <- 'global_ufpallv31_v2'   #global_ufpallv31_v1Extra3Citiesv2: without AMS, COP winsorized; global_ufpallv31_v1Extra3Citiesv3: with AADT_onRoad; global_ufpallv31_v2: without AADT_onRoad

# Force basel to be Continental
df_all2 <- df_all2 %>% mutate(climate_zone=as.character(climate_zone),
                              climate_zone=ifelse(city=="Basel", "Continental", climate_zone)) %>% 
  mutate(climate_zone=factor(climate_zone))
table(df_all2$climate_zone, df_all2$city)
histogram((df_all2$climate_zone))
## regrouping (end)

# Fit linear model
source("src/fun_mclapplySLR.R") 
allPred_vars
slr_lists <- trainSLRAll(csv_name, df_all2, allPred_vars, T, df_all2, obs_var="UFPmean", outDir='data/workingData/')
lm_model <- slr_lists[[2]]


######### Manually remove the variables with negative direction of effects #########
read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, ".csv"))
selected_predictors <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, ".csv"))$variables[-1]
#remove nat_10000, and then allR_10000
source("src/01_LUR/fun_manual_changeSLR")
slr_1 <- exc_var_slr(selected_predictors, "nat_10000", df_all2, "UFPmean")
summary(slr_1)

# allR_10000 still negative
slr_2 <- exc_var_slr(selected_predictors, "nat_10000|allR_10000", df_all2, "UFPmean")
summary(slr_2)
vif(slr_2)
# Everything all right
# Output: 
output_slr_summary(slr_2, csvname=paste0(csv_name, "_manual"), "data/workingData/")

######### test (global_ufpallv31_v1Extra3Cities)#########
# Test removing rst and fst variables
df_all2_testElev <- df_all2 %>% mutate(alt10_enh=sqrt(ifelse(alt10_enh*-1<0, 0, alt10_enh*-1))*-1)
slr_lists <- trainSLRAll(paste0(csv_name, "testElev"), df_all2_testElev, allPred_vars, T, df_all2_testElev, obs_var="UFPmean", outDir='data/workingData/')

slr_lists <- trainSLRAll(paste0(csv_name, "testNoFsfRst"), df_all2, 
                         allPred_vars[!grepl("FsfRst", allPred_vars)], FALSE, df_all2, obs_var="UFPmean", outDir='data/workingData/')

slr_lists2 <- trainSLRAll(paste0(csv_name, "testNoCTM"), df_all2, 
                         allPred_vars[!grepl("CTMufp", allPred_vars)], FALSE, df_all2, obs_var="UFPmean", outDir='data/workingData/')
# Test coordinates
slr_lists_xy <- trainSLRAll(paste0(csv_name, "testXY"), df_all2_xy, allPred_vars_xy, FALSE, df_all2_xy, 
                         obs_var="UFPmean", outDir='data/workingData/')
slr_lists_xy_noMet <- trainSLRAll(paste0(csv_name, "testXYnoMet"), df_all2_xy, allPred_vars_xy[!grepl("temp|precip|pressure|wind", allPred_vars_xy)], 
                            FALSE, df_all2_xy, 
                            obs_var="UFPmean", outDir='data/workingData/')
summary(slr_lists_xy_noMet[[2]])

slr_lists_xy2 <- trainSLRAll(paste0(csv_name, "testXY2"), df_all2_xy, allPred_vars_xy2, FALSE, df_all2_xy, 
                         obs_var="UFPmean", outDir='data/workingData/')
slr_lists_xy3 <- trainSLRAll(paste0(csv_name, "testXY3"), df_all2_xy, c(allPred_vars, "XY"), FALSE, df_all2_xy, 
                         obs_var="UFPmean", outDir='data/workingData/')
# Test city
slr_lists_city <- trainSLRAll(paste0(csv_name, "test_city"), df_all2 %>% mutate(city=factor(city)), 
                              c(allPred_vars, "city"), FALSE, df_all2, obs_var="UFPmean", outDir='data/workingData/')
slr_lists_region <- trainSLRAll(paste0(csv_name, "test_region"), df_all2, 
                              c(allPred_vars, "region"), FALSE, df_all2, obs_var="UFPmean", outDir='data/workingData/')
# Force city in

force_var <- function(var_name, df_all_f, remove_var=NA){
  vars_total <- as.character(unique(df_all_f[,var_name]))
  selected_predictors <- read.csv("data/workingData/SLR_summary_model_global_ufpallv31_v1Extra3Citiesv3.csv")$variables[-1]
  if(!is.na(remove_var)){
    selected_predictors <- selected_predictors[selected_predictors!=remove_var]
  }
  
  slr_city <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors, var_name), collapse="+"))), df_all_f)
  df_all_f$slr <- predict(slr_city, df_all_f)
  df_all_f <- df_all_f[!is.na(df_all_f$slr),]
  df_all_f <- df_all_f[!is.na(df_all_f$UFPmean),]
  out_err_df <- error_matrix(df_all_f$UFPmean, df_all_f$slr) %>% 
      as.data.frame() %>% 
      mutate(.=round(., 2))
  names(out_err_df) <- paste0("slr_", var_name)
  
  
  df_coefs <- slr_city$coefficients %>% as.data.frame
  names(df_coefs) <- "coef"
  df_coefs$predictor_name <- row.names(df_coefs)
  
  
  intercept_df <- df_coefs[df_coefs$predictor_name=="(Intercept)",]
  intercept_df2 <- df_coefs[grepl(paste0("^", var_name), df_coefs$predictor_name),]
  intercept_df2$coef <- intercept_df2$coef+intercept_df$coef
  
  intercept_final <- rbind(intercept_df %>% mutate(note="Baseline"),
                           intercept_df2 %>% mutate(note=gsub(var_name, "", predictor_name)))
  var_baseline <- vars_total[!(vars_total %in% unique(intercept_final$note))]
  final_coefDF <- intercept_final %>% mutate(coef_type="intercept") %>% 
    mutate(note=ifelse(note=="Baseline", var_baseline, note)) %>% 
    mutate(spatial_var=var_name)
  
  p1 <- ggplot(final_coefDF, aes(x = coef, y = note)) +
    facet_grid(.~coef_type, scale="free")+
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Coefficient Values varying by ", var_name),
         x = "Coefficient Value",
         y = var_name) +
    theme_bw()
  print(p1)
  
  list(model=summary(slr_city), vif=vif(slr_city), err_df=out_err_df, final_coefDF=final_coefDF)
}

force_var("city", df_all2, NA)
force_var("city", df_all2, "apor_1000")

force_var("region", df_all2)
force_var("region", df_all2, "alt10_enh")
force_var("lcz", df_all2)
force_var("lcz_group", df_all2)
force_var("lcz_group", df_all2, "nat_10000")

force_var("climate_zone", df_all2)
force_var("climate_zone", df_all2, "apor_1000") #ind_6000, apor_1000

df_all2 <- df_all2 %>% rename(LCZ=lcz_group, BCZ=climate_zone)
coef_df <- list(force_var("BCZ", df_all2, "mjrR_2000")$final_coefDF,
     force_var("LCZ", df_all2, "nat_10000")$final_coefDF,
     force_var("region", df_all2, "alt10_enh")$final_coefDF,
     force_var("city", df_all2, "apor_1000")$final_coefDF,
     force_var("BCZ", df_all2, "apor_1000")) %>% do.call(rbind, .)
saveRDS(coef_df, "data/temp/varyingInterceptOnly_coef_df.rds")
source("src/fun_vis.R")
ggplot(coef_df, aes(x = coef, y = note)) +
  facet_grid(spatial_var~coef_type, scale="free")+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Coefficient values"),
       x = "Coefficient Value",
       y = "") +
  scale_y_discrete(limits=rev)+
  theme_bw()+
  g_theme


force_var("XY", df_all2_xy)
force_var("XY", df_all2_xy, "ugr_10000")

force_var("Y", df_all2_xy)

ggplot(df_all2, aes(x=UFPmean, y=city))+
  geom_boxplot()+
  labs(x="UFP mean of means")

ggplot(df_all2 %>% group_by(city) %>%  summarise(UFPmean=mean(UFPmean, na.rm=T)), aes(x=UFPmean, y=city))+
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()+
  labs(x="Average UFP measured (mean of means)")

# Test log transform
slr_lists <- trainSLRAll(paste0(csv_name, "testLogTrans"), df_all2 %>% mutate(UFPmean=log(UFPmean)), allPred_vars, FALSE, df_all2, obs_var="UFPmean", outDir='data/workingData/')
slr_lists[[2]] %>% summary
selected_predictors <- read.csv("data/workingData/SLR_summary_model_global_ufpallv31_v1Extra3CitiestestLogTrans.csv")$variables[-1]
slr_log <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors), collapse="+"))), df_all2 %>% mutate(UFPmean=log(UFPmean)))
df_all2$slr_predictedLog <- predict(slr_log, df_all2 %>% mutate(UFPmean=log(UFPmean)))%>%exp
summary(df_all2$slr_predictedLog)
df_all2 <- df_all2[!is.na(df_all2$slr_predictedLog),]
df_all2 <- df_all2[!is.na(df_all2$UFPmean),]
error_matrix(df_all2$UFPmean, df_all2$slr_predictedLog) %>% 
    as.data.frame() %>% 
    mutate(.=round(., 2)) %>% 
    rename(slr_log=".")

lm_model <- slr_lists[[2]]
slr_lists_xy[[2]] %>% summary

slr_lists <- trainSLRAll(paste0("withCity_", csv_name), df_all2, c(allPred_vars, "city"), FALSE, df_all2_dir, "UFPmean")

#### EDA: check the spatial variability of the spatial indicators (climate zone and city)
table(df_all2$zoneID, df_all2$city)
# Original setting
selected_predictors <- read.csv("data/workingData/SLR_summary_model_global_ufpallv31_v1Extra3Citiesv2.csv")$variables[-1]
slr_o <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors), collapse="+"))), df_all2)
df_all2$pred_o <- predict(slr_o, df_all2)
df_all2 <- df_all2[!is.na(df_all2$UFPmean),]
df_all2 <- df_all2[!is.na(df_all2$pred_o),]
error_matrix(df_all2$UFPmean, df_all2$pred_o) %>% 
  as.data.frame() %>% 
  mutate(.=round(., 2)) %>% 
  rename(slr_original=".")

# Test what would happen if we forced other spatial indicators (e.g., city)
selected_predictors <- read.csv("data/workingData/SLR_summary_model_global_ufpallv31_v1.csv")$variables[-1]
slr_climate <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors), collapse="+"))), df_all2)
slr_city <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors, "city"), collapse="+"))), df_all2)
slr_ctm <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors, "CTMufp"), collapse="+"))), df_all2)
slr_city_mAADT <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors[!grepl("AADT", selected_predictors)], "city*AADT_resNo"), collapse="+"))), df_all2)
slr_city_mAADT2 <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors[!grepl("AADT", selected_predictors)], "zoneID*AADT_resNo"), collapse="+"))), df_all2)
slr_city_mmjrR <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors[!grepl("mjrR_50", selected_predictors)], "city*mjrR_50"), collapse="+"))), df_all2)
slr_city_mfsfRst <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors[!grepl("FsfRst_500", selected_predictors)], "city*FsfRst_500"), collapse="+"))), df_all2)
 
summary(slr_climate)
summary(slr_city)
summary(slr_ctm)
summary(slr_city_mAADT)
summary(slr_city_mAADT2)
summary(slr_city_mmjrR)


vif(slr_climate)
vif(slr_city)
vif(slr_ctm)
vif(slr_city_mAADT)
vif(slr_city_mAADT2)

slr_city_mAADT2 <- lm("UFPmean ~ por_6000+mjrR_50+FsfRst_500+nat_100+FsfRst_50+ugr_2500+ind_100+city*AADT_resNo", df_all2)
summary(slr_city_mAADT2)
vif(slr_city_mAADT2)

slr_city_mmjrR <- lm("UFPmean ~ AADT_resNo+por_6000+mjrR_50+FsfRst_500+nat_100+FsfRst_50+ugr_2500+ind_100+city*mjrR_50", df_all2)
summary(slr_city_mmjrR)
vif(slr_city_mmjrR)

slr_city_mfsfRst2 <- lm("UFPmean ~ AADT_resNo+por_6000+mjrR_50+FsfRst_500+nat_100+ugr_2500+ind_100+mjrR_50+city*FsfRst_50", df_all2)
summary(slr_city_mfsfRst2)
vif(slr_city_mfsfRst2)
##### -> inspiration: do some combination of the spatial indicator with the selected variables (one by one and see whether the R2 would increase and of course need to check for p value and vif)
## factor zoneID
slr_climate2 <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors), collapse="+"))), df_all2 %>% mutate(zoneID=as.factor(zoneID)))
slr_climate3 <- lm("UFPmean ~ zoneID + AADT_resNo + por_6000 + mjrR_50 + FsfRst_500 + nat_100 + vwind + FsfRst_50 + ugr_2500 + ind_100 + AADT_resNo*zoneID", 
                    df_all2 %>% mutate(zoneID=as.factor(zoneID)))
summary(slr_climate3)
vif(slr_climate3)

summary(slr_climate2)


# slr_city_mmjrR2 <- lm("UFPmean ~ AADT_resNo*city+por_6000+mjrR_50+FsfRst_500+nat_100+FsfRst_50+ugr_2500+ind_100+city*mjrR_50", df_all2)
# summary(slr_city_mmjrR2)
# vif(slr_city_mmjrR2)
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
# deconv_dfs <- mclapply(cities, read_deconv, df_original=df_all2, mc.cores = length(cities))
# Optimal roll_window for UFP based on traffic data correlation comparison:  200 , city:  Lodz 
# Optimal roll_window for UFP based on traffic data correlation comparison:  500 , city:  Basel 
# Optimal roll_window for UFP based on traffic data correlation comparison:  400 , city:  Rome 
# Optimal roll_window for UFP based on traffic data correlation comparison:  400 , city:  Barcelona 
# Optimal roll_window for UFP based on traffic data correlation comparison:  600 , city:  Athens 
# Optimal roll_window for UFP based on traffic data correlation comparison:  1800 , city:  Copenhagen 
# Optimal roll_window for UFP based on traffic data correlation comparison:  100 , city:  Rotterdam 
# Optimal roll_window for UFP based on traffic data correlation comparison:  300 , city:  Munich 
# Optimal roll_window for UFP based on traffic data correlation comparison:  1200 , city:  Amsterdam 
# median(c(200,500,400,400,600,1800,100,300,1200))
# = 400
deconv_dfs <-  mclapply(cities, read_deconv_define_movingWindow, 
          df_original=df_all2, optimal_roll_window=400, 
          mc.cores = length(cities))


deconv_dfs <- do.call(rbind, deconv_dfs)
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFPdeconv),]
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFP_diff),]

## Create shp files to upload to GEE
head(deconv_dfs)
deconv_dfs_sf <- st_as_sf(deconv_dfs, coords=c("X", "Y"), crs=st_crs(3035))
head(deconv_dfs_sf)
st_write(deconv_dfs_sf, "data/temp/deconv_3035.shp")
st_write(st_transform(deconv_dfs_sf, st_crs(4326)), "data/temp/deconv_4326.shp")
## Inspect the weirdly high anamoly in Rotterdam
deconv_dfs_sf_rot <- deconv_dfs_sf %>% filter(city=="Rotterdam")
summary(deconv_dfs_sf_rot$UFPdeconv)
quantile(deconv_dfs_sf_rot$UFPdeconv, probs=c(0.5,0.7,0.8,0.85,0.9,0.95,0.99))
deconv_dfs_sf_rot %>% filter(UFPdeconv>70000) %>% nrow
deconv_dfs_sf_rot %>% filter(UFPdeconv>90580) %>% nrow

summary((deconv_dfs_sf_rot %>% filter(UFPdeconv<74146))$UFPdeconv)

mapviewOptions(fgb = FALSE, georaster = FALSE, native.crs=T)
mapview(deconv_dfs_sf_rot %>% filter(UFPdeconv>74146))

## Select the variables
pred_lists <- prep_deconv_variables(allPred_vars, bufNo = 1000,
                                  background_varNoBuf=c("anbh", "CTMufp", "lcz", "precip", "pressure", "temp", "wind", "zoneID", "alt10_enh", "X", "Y"),
                                  local_varNoBuf=c("roadtype", "AADT_onRoad", "pop2020"))
var_background <- pred_lists[[1]]
var_local <- pred_lists[[2]]
var_local <- var_local[!grepl("AADT_onRoad", var_local)]
csv_name <- paste0('global_ufpallv31_v2_background')  #global_ufpallv31_v1Extra3Cities2_background
csv_name2 <- paste0('global_ufpallv31_v2_local')  #global_ufpallv31_v1Extra3Cities2_local


source("src/fun_mclapplySLR.R") 
slr_lists <- trainSLRAll(csv_name, deconv_dfs, var_background, T, deconv_dfs, 
                         outDir = "data/workingData/", obs_var="UFPdeconv")
slr_lists2 <- trainSLRAll(csv_name2, deconv_dfs, var_local, T, deconv_dfs, 
                          outDir = "data/workingData/", obs_var="UFP_diff")

lm_model <- slr_lists[[2]]
lm_model2 <- slr_lists2[[2]]
# negative coefficient value for allR_700 in lm_model2

######### Manually remove the variables with negative direction of effects #########
read.csv(paste0("data/workingData/SLR_summary_model_", csv_name2, ".csv"))
selected_predictors <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name2, ".csv"))$variables[-1]
#remove nat_10000, and then allR_10000
source("src/01_LUR/fun_manual_changeSLR")
slrdiff_1 <- exc_var_slr(selected_predictors, "allR_700", deconv_dfs, "UFP_diff")
summary(slrdiff_1)
# Everything all right
# Output: 
output_slr_summary(slrdiff_1, csvname=paste0(csv_name2, "_manual"), "data/workingData/")


## What would happen if i removed the outliers in Rotterdam
csv_name <- paste0('global_ufpallv31_v1Extra3Cities3_background_excRotOutlier')  #global_ufpallv31_v1Extra3Cities2_background
csv_name2 <- paste0('global_ufpallv31_v1Extra3Cities3_local_excRotOutlier')  #global_ufpallv31_v1Extra3Cities2_local

deconv_dfs_exc <- rbind(deconv_dfs %>% filter(city=="Rotterdam", UFPdeconv<74146),
                        deconv_dfs %>% filter(city!="Rotterdam"))
source("src/fun_mclapplySLR.R") 
slr_lists <- trainSLRAll(csv_name, deconv_dfs_exc, var_background, T, deconv_dfs_exc, 
                         outDir = "data/workingData/", obs_var="UFPdeconv")
slr_lists2 <- trainSLRAll(csv_name2, deconv_dfs_exc, var_local, T, deconv_dfs_exc, 
                          outDir = "data/workingData/", obs_var="UFP_diff")

lm_model <- slr_lists[[2]]
lm_model2 <- slr_lists2[[2]]


# To visualize the distribution of the deconvoluted values
source("../airview_basel/src/fun_edaPlots.R")
densityPlots(deconv_dfs%>%mutate(UFPdeconv=log(UFPdeconv)), "UFPdeconv")
ggplot(deconv_dfs)+
  geom_density(aes(x=log(UFPdeconv)))+
  theme_bw()
ggplot(deconv_dfs)+
  geom_density(aes(x=UFP_diff))+
  theme_bw()



deconv_dfs$UFPdeconv
deconv_dfs$UFP_diff

######### 2) big global model (archive) ########
readDF <- function(city, fnames, in_dir){
  source("src/fun_process_predDF.R")
  csv_name <- paste0(city, '_ufpallv31') #No variables (stat_v2)
  process_predDF(city, fnames, in_dir)
}
in_dir <- 'data/raw/gee/'#/data/volume_2/airview_basel/
fnames <- list.files(in_dir, '.shp')
fnames <- fnames[grepl("v31", fnames)]

allDF <- lapply(cities, readDF, fnames=fnames, in_dir=in_dir)
# check the predictors
# lapply(seq_along(allDF), function(i){
#   length(allDF[[i]][[2]])
# }) %>% unlist
max_i <- lapply(allDF, `[[`, 1) %>% lapply(., ncol) %>% unlist %>% which.max
all_dfs <- lapply(seq_along(allDF), function(i){
  out_df <- allDF[[i]][[1]]
  
  all_varnames <- names(allDF[[max_i]][[1]])
  if(ncol(out_df)!=ncol(allDF[[max_i]][[1]])){
    
    if(!"UFPsd"%in%names(out_df)) {
      out_df$UFPsd <- NA
    }
    if(!"BC1sd"%in%names(out_df)) {
      out_df$BC1sd <- NA
    }
    if(!"BC6sd"%in%names(out_df)) {
      out_df$BC6sd <- NA
    }
  }
  out_df2 <- out_df[,all_varnames]
  out_df2$city=cities[i]
  out_df2
})
all_dfs <- do.call(rbind, all_dfs)
# all_dfs <- all_dfs %>% mutate(id=1:nrow(all_dfs)) %>% dplyr::select(-nfold)
dim(all_dfs)
table(all_dfs$city)
pred_c_total <- allDF[[1]][[2]]
pred_c_totalGlobal <- c(pred_c_total[!grepl("lau_id", pred_c_total)], "X", "Y")  ##unclear how to incorporate the x, y coordinates
write.table(pred_c_totalGlobal, "data/temp/pred_c_totalGlobal.txt", quote=F, row.names=F, col.names = F)
summary(all_dfs$X)###long (centroid points)
summary(all_dfs$Y)###lat (centroid points)


all_dfs$lau_id = as.numeric(as.factor(all_dfs$selectProp))
df_all2 <- all_dfs[!is.na(all_dfs$UFPmean),]
saveRDS(df_all2, "data/temp/df_all2.Rdata") #different folds than the city-specific (2023/02/13) - need to use this to train the city specific models
######
df_all2 <- readRDS("data/temp/df_all2.Rdata")
source("src/fun_mclapplySLR.R")
fivefoldCVResult <- mclapplySLR(paste0("global_ufpallv31"), df_all2, pred_c_totalGlobal)
fivefoldCVResult
trainSLRfold(paste0("global_ufpallv31"), 1, df_all2, pred_c_totalGlobal)

# Not yet run... (because it takes too much time... need to subsample the mobile data)
source("src/fun_mclapplyRF.R")
fivefoldCVRFResult <- mclapplyRF(paste0("global_ufpallv31"), df_all2, pred_c_totalGlobal)
fivefoldCVRFResult


