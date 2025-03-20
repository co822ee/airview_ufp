source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/fun_vis.R")
library(dplyr)
library(readr)
library(lemon)
# Set the directory path for the CSV files
path <- "data/raw/Airview_extractedPredictors_extVal/"#/Volumes/My Passport for Mac/github/airview_basel/
# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
# Print the filtered files
files
obs_names <- list("UFPobs", "PNC_mn_", "UFPobs", "UFPobs", "UFPobs", "mean_pnc_h", "UFPobs")
id_names <- list("ID", "id", "ID", "sta_name", "id", "site_id", "id")
loc_names  <- list( "NL-Claire", "Augsburg", "NL-EXPOsOMICS", "GUAN", "Amsterdam", "Rome", "Switzerland")
city_specifics <- list(NA, "Munich", NA, "Munich", "Amsterdam", "Rome", "Basel")

unify_df <- function(csv_name, obs_name, id_name, location_name){
  ### Combine all the data frame and separated by locations columns files[i], obs_names[[i]], id_names[[i]], loc_names[[i]]
  in_df <- read.csv(csv_name)
  if(location_name=="GUAN"){
    obs_df <- read.csv(paste0('data/raw/external_validation/GUAN/guan_obs_raw.csv'))
    in_df <- in_df %>% rename(sta_name=sta_nam)
    in_df <- inner_join(in_df, obs_df)
  }
  in_df_c <- in_df %>% 
    rename(id = !!sym(id_name), UFPobs = !!sym(obs_name)) %>% 
    mutate(loc=location_name) %>% 
    dplyr::select(-".geo", -"system.index") %>% 
    rename_at(vars(starts_with("apor_2018_")), ~str_replace(., "apor_2018_", "apor_"))
  source("src/01_LUR/fun_find_buf_varnames.R")
  # if(location_name=="NL-Claire"){
  #   claire_new = read.csv("data/temp/nl_CLAIRE_agr.csv")
  #   in_df_c  <- right_join(in_df_c %>% dplyr::select(-UFPobs), 
  #                                    claire_new %>% dplyr::select(UFP_adj_diff, ID) %>% rename(UFPobs=UFP_adj_diff, id=ID))
  #   in_df_c <- in_df_c[!duplicated(in_df_c$id),]
  # }

  if(location_name=='Amsterdam'){
    filtered_id <- read.csv("data/temp/nl_ams_filtered.csv")
    in_df_c <- in_df_c[in_df_c$id%in%filtered_id$id,]
  }
  if(location_name=='Rome'){
    in_df_c <- in_df_c[!(in_df_c$id%in%c(8,15)),]
  }
  ## Remove variables of ind_ and por_ with small buffer sizes (<1km)
  exc_ind_names <- find_buf_varnames("ind_", names(in_df_c), 1000, "<")
  exc_por_names <- find_buf_varnames("por_", names(in_df_c), 1000, "<")
  
  in_df_c <- in_df_c %>% select(-all_of(c(exc_por_names, exc_ind_names)))
  
  in_df_c$lcz <- factor(in_df_c$lcz)
  #   in_df_c$zoneID <- factor(in_df_c$zoneID)
  in_df_c <- in_df_c %>% rename_all(~gsub("__", "_", .))
  #   in_df_c$city <- factor(in_df_c$city)
  #   in_df_c$roadtype <- factor(in_df_c$roadtype)
  # Remove supermarket
  df_all2_new <- in_df_c %>% select(-matches("^Spm_"), -uwind, -vwind)
  
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
  # Truncate the elevation to 760
  
  df_all2_new <- df_all2_new %>% rename(AADT_onRoad=AADT_resNoXY)
  df_all2_new <- df_all2_new %>% as.data.frame() %>% ungroup()
  ######### add the negative direction of effect for SLR #####
  df_all2_new <- df_all2_new %>% 
    mutate_at(vars(starts_with("nat_"), starts_with("ugr_"), starts_with("precip"), "wind", "alt10_enh"), list(~ . * -1))
  # Truncate the elevation
  df_all2_new <- df_all2_new %>% mutate(alt10_enh=ifelse(alt10_enh< -760, -760, alt10_enh))
  df_all2_new
}

tbl_all_lists <- lapply(seq_along(files), function(i){#seq_along(filtered_files)
  unify_df(files[i], obs_names[[i]], id_names[[i]], loc_names[[i]])
})
# Find common column names across all data frames in the list
common_cols <- Reduce(intersect, lapply(tbl_all_lists, names))

# Bind all data frames in the list using only the common columns
combined_tbl_all <- do.call(rbind, lapply(tbl_all_lists, function(df) df[, common_cols]))
summary(combined_tbl_all$alt10_enh)


#### Visualize the UFP measurements across location #######
table(combined_tbl_all$loc)
ggplot(combined_tbl_all, aes(y = loc, x = UFPobs)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of measured UFP by data source", y = "", x = "UFP counts (particles/cm^3)") +
  theme_minimal()+
  scale_y_discrete(limits=rev)+
  g_theme
ggsave("results/figures/paper/3-3_extVal_boxplots.pdf", width=9, height=5.5)

# Generate a summary table of UFPobs by location
ufp_summary <- combined_tbl_all %>%
  group_by(loc) %>%
  summarise(
    n = n(),
    Mean_UFP = mean(UFPobs, na.rm = TRUE) / 1000,
    SD_UFP = sd(UFPobs, na.rm = TRUE) / 1000,
    Min_UFP = min(UFPobs, na.rm = TRUE) / 1000,
    Quantile_10th_UFP = quantile(UFPobs, 0.1, na.rm = TRUE) / 1000,
    Median_UFP = median(UFPobs, na.rm = TRUE) / 1000,
    Quantile_90th_UFP = quantile(UFPobs, 0.9, na.rm = TRUE) / 1000,
    Max_UFP = max(UFPobs, na.rm = TRUE) / 1000
  )
# Output the summary table to a CSV file
write.csv(ufp_summary, "results/output/paper/3-3_extVal_ufp_summary_by_location.csv", row.names = FALSE)


######## visualize the traffic distribution #########
# Create a histogram of the aadt_50 column, separated by loc
ggplot(combined_tbl_all, aes(x = aadt_50)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of AADT_50 by Location", x = "AADT_50", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ loc, scales = "free_y")

ggplot(combined_tbl_all, aes(x = loc, y = aadt_50)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.6) +
  labs(x = "", y = "aadt_50") +
  g_theme+
  theme(axis.text.x = element_text(angle = 20, hjust = 0.9, vjust=1.))
ggsave("results/figures/paper/3-3_extVal_boxplots_aadt.pdf", width=4.8, height=6)

ggplot(combined_tbl_all, aes(x = loc, y = AADT_onRoad)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of on-road AADT by Location", x = "Location", y = "on-road AADT") +
  theme_minimal()


ggplot(combined_tbl_all, aes(x = loc, y = alt10_enh)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.6) +
  labs(x = "", y = "alt10_enh") +
  g_theme+
  theme(axis.text.x = element_text(angle = 20, hjust = 0.9, vjust=1.))

###############################
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/01_LUR/fun_read_mobileData_v1.R")

source("src/01_LUR/fun_read_deconv.R")
source("src/01_LUR/fun_prep_deconv_variables.R")
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

# Force basel to be Continental
df_all2 <- df_all2 %>% mutate(climate_zone=as.character(climate_zone),
                              climate_zone=ifelse(city=="Basel", "Continental", climate_zone)) %>% 
  mutate(climate_zone=factor(climate_zone))


cities <- unique(df_all2$city)%>%as.character
deconv_dfs <-  mclapply(cities, read_deconv_define_movingWindow, 
          df_original=df_all2, optimal_roll_window=400, 
          mc.cores = length(cities))
deconv_dfs <- do.call(rbind, deconv_dfs)
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFPdeconv),]
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFP_diff),]


source("src/02_vis/fun_out_err.R")
# Read in the SLR model structure from the CSV file
fname="global_ufpallv31_v2"
make_SLRprediction <- function(fname, extVal_tbl, model_str, deconvB=F, obsname="UFPobs"){
  
  #global_ufpallv31_v2
  if(deconvB){
    if(length(list.files("data/workingData/", paste0("SLR_summary_model_", fname, "_background_manual.csv")))>0){
      slr_background_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_background_manual.csv"))
    }else{
      slr_background_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_background.csv"))
    }
    if(length(list.files("data/workingData/", paste0("SLR_summary_model_", fname, "_local_manual.csv")))>0){
      slr_local_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_local_manual.csv"))
    }else{
      slr_local_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_local.csv"))
    }
    # slr_background_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_background.csv"))
    # slr_local_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_local.csv"))
    # Extract the selected predictor variables
    background <- slr_background_structure$variables
    local  <- slr_local_structure$variables
    
    background <- background[!grepl("Final", background)]
    local <- local[!grepl("Final", local)]
    
    # Develop a linear regression model using the selected predictors
    slr_model <- lm(UFP_diff ~ ., data = deconv_dfs[, c("UFP_diff", local)])
    slr_model2 <- lm(UFPdeconv ~ ., data = deconv_dfs[, c("UFPdeconv", background)])
    
    extVal_tbl$slr <- predict(slr_model, newdata = extVal_tbl) + predict(slr_model2, newdata = extVal_tbl)
    
  }else{
    if(length(list.files("data/workingData/", paste0("SLR_summary_model_", fname, "_manual.csv")))>0){
      slr_model_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,"_manual.csv"))
    }else{
      slr_model_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,".csv"))
    }
    
    # Extract the selected predictor variables and their coefficients
    selected_predictors <- slr_model_structure$variables
    selected_predictors <- selected_predictors[!grepl("Final", selected_predictors)]
    if(all(slr_model_structure$variables[!grepl("Final", slr_model_structure$variables)] %in% selected_predictors)){
      # selected_coefficients <- slr_model_structure$beta[!grepl("Final", slr_model_structure$variables)]
      # # Calculate the prediction using the extracted coefficients
      # predictors_data <- extVal_tbl[, selected_predictors]
      # predictors_data <- as.matrix(predictors_data)
      # intercept <- slr_model_structure$beta[slr_model_structure$variables == "Final"]
      # predictors_data[1:2,] * selected_coefficients
      # extVal_tbl$slr <- intercept + rowSums(predictors_data * selected_coefficients)
      
      #             extVal_tbl$slr <- lapply(1:nrow(predictors_data), function(i){
      #     intercept + (predictors_data[i,] * selected_coefficients)%>%apply(., 1, sum)
      # }) %>% unlist
      
      slr_model_structure <- read.csv(paste0("data/workingData/SLR_summary_model_", fname,".csv"))
      # Extract the selected predictor variables
      selected_predictors <- slr_model_structure$variables
      selected_predictors <- selected_predictors[!grepl("Final", selected_predictors)]
      
      # Develop a linear regression model using the selected predictors
      slr_model <- lm(UFPmean ~ ., data = df_all2[, c("UFPmean", selected_predictors)])
      
      extVal_tbl$slr <- predict(slr_model, newdata = extVal_tbl)
    }        
    
    
  }
  extVal_tbl <- extVal_tbl[!is.na(extVal_tbl$slr),]
  extVal_tbl <- extVal_tbl[!is.na(extVal_tbl[,obsname]),]
  if(unique(extVal_tbl$loc)=="NL-Claire"){
    # Reorder the data frame based on the slr values
    extVal_tbl <- extVal_tbl[order(extVal_tbl$slr, decreasing = T),]

    # Aggregate the slr and obsname in every 5 group
    extVal_tbl <- extVal_tbl %>%
      mutate(group = ceiling(row_number() / 5)) %>%
      group_by(group) %>%
      summarise(slr = mean(slr), !!obsname := mean(!!sym(obsname))) %>% 
      as.data.frame()
    
  }
  extVal_tbl %>% mutate(model=model_str)
  
}

make_errorMatrix <- function(fname, extVal_tbl, model_str, deconvB=F, obsname="UFPobs"){
  extVal_tbl2 <- make_SLRprediction(fname, extVal_tbl, model_str, deconvB, obsname)
  out_err(extVal_tbl2, model_str, obsname, "slr")
}
### Claire #####
i=1
loc_sub <- unique(combined_tbl_all$loc)[i]
combined_tbl_all2 <- combined_tbl_all %>% filter(loc==loc_sub)
table(combined_tbl_all2$loc)

ext_valid_em <- lapply(seq_along(unique(combined_tbl_all$loc)), function(i){
  loc_sub <- unique(combined_tbl_all$loc)[i]
  combined_tbl_all2 <- combined_tbl_all %>% filter(loc==loc_sub)
  
  if(is.na(city_specifics[[i]])){
    rbind(
      make_errorMatrix("global_ufpallv31_v2", combined_tbl_all2, "pooled") %>% 
        mutate(loc=loc_sub),
      make_errorMatrix("global_ufpallv31_v2", combined_tbl_all2, "pooled-deconv", T) %>% 
        mutate(loc=loc_sub))
  }else{
    rbind(
      make_errorMatrix("global_ufpallv31_v2", combined_tbl_all2, "pooled") %>% 
        mutate(loc=loc_sub),
      make_errorMatrix("global_ufpallv31_v2", combined_tbl_all2, "pooled-deconv", T) %>% 
        mutate(loc=loc_sub),
      make_errorMatrix("Rome_ufpallv31_v2", combined_tbl_all2, "citySpecific") %>% 
        mutate(loc=loc_sub))
  }
})
ext_valid_em <- ext_valid_em %>% do.call(rbind, .)
# Remove city-specific model performance for GUAN
ext_valid_em2 <- ext_valid_em[!(ext_valid_em$model=="citySpecific"&ext_valid_em$loc=="GUAN"),]
ext_valid_em2 <- ext_valid_em2[!(ext_valid_em2$model=="citySpecific"&ext_valid_em2$loc=="Switzerland"),]
err_plots_df2 <- ext_valid_em2 %>% 
  gather(., "matrix", "value", c("r2", "RMSE", "RRMSE", "bias")) %>% 
  mutate(value=ifelse(matrix=='RMSE'|matrix=='bias', 
                      round(value, 0) %>% as.character(), 
                      ifelse(value==2, "2.00", str_pad(as.character(value), pad="0", width=4, side="right"))))
best_df <-  rbind(
  err_plots_df2 %>% 
    filter(matrix=="r2") %>% 
    group_by(matrix, loc) %>% 
    summarize(max_value = max(as.numeric(value)%>%abs, na.rm = TRUE),
              model = model[which.max(as.numeric(value)%>%abs)]
    ),
  err_plots_df2 %>% 
    group_by(matrix, loc) %>% 
    filter(matrix!="r2") %>% 
    summarize(max_value = min(as.numeric(value)%>%abs, na.rm = TRUE),
              model = model[which.min(as.numeric(value)%>%abs)],
    )
  
) %>%
  mutate(modelFill = "The best")
err_plots_df2_2 <- left_join(err_plots_df2,
                             best_df) %>% 
  mutate(modelFill=ifelse(is.na(modelFill), ".", modelFill))
library(lemon)

ggplot(err_plots_df2_2 %>% 
        mutate(matrix=ifelse(matrix=='r2', "R2", matrix)) %>% 
        mutate(value=ifelse(matrix=='RMSE', as.character(round(as.numeric(value)/1000, 1)), value)) %>% 
        mutate(value=ifelse(matrix=="RMSE", ifelse(grepl("\\.", value), value, paste0(value, ".0")), value)) %>% 
        mutate(value=ifelse(matrix=='bias', as.character(round(as.numeric(value)/1000, 1)), value)) %>% 
        mutate(value=ifelse(matrix=="bias", ifelse(grepl("\\.", value), value, paste0(value, ".0")), value)),
       aes(y=model, x=loc, fill=modelFill))+
  g_theme+
  theme(axis.text.x = element_text(angle = 20, hjust = 0.9, vjust=1.),
        text = element_text(size=13))+
  geom_tile(alpha=0.2)+
  scale_fill_manual(values=c('The best'='red','.'='transparent'))+
  labs(y='', fill='')+
  facet_rep_wrap(matrix~., ncol=2, repeat.tick.labels=T)+
  geom_text(aes(label=value))
ggsave("results/figures/paper/3-3_modelPerformance_heatmapExtVal.pdf", width=13, height=8)
# read_err("global_ufpallv31_v2", df_all2, "pooled-overall", obsname='UFPmean')  #. Test whether it corresponds to our results


##### scatterplots ######

ext_valid_df <- lapply(seq_along(unique(combined_tbl_all$loc)), function(i){
  loc_sub <- unique(combined_tbl_all$loc)[i]
  combined_tbl_all2 <- combined_tbl_all %>% filter(loc==loc_sub)
  
  if(is.na(city_specifics[[i]])){
    rbind(
      make_SLRprediction("global_ufpallv31_v2", combined_tbl_all2, "pooled") %>% 
        mutate(loc=loc_sub),
      make_SLRprediction("global_ufpallv31_v2", combined_tbl_all2, "pooled-deconv", T) %>% 
        mutate(loc=loc_sub)) %>% 
    dplyr::select(slr, UFPobs, model, loc)
  }else{
    rbind(
      make_SLRprediction("global_ufpallv31_v2", combined_tbl_all2, "pooled") %>% 
        mutate(loc=loc_sub),
      make_SLRprediction("global_ufpallv31_v2", combined_tbl_all2, "pooled-deconv", T) %>% 
        mutate(loc=loc_sub),
      make_SLRprediction("Rome_ufpallv31_v2", combined_tbl_all2, "citySpecific") %>% 
        mutate(loc=loc_sub)) %>% 
    dplyr::select(slr, UFPobs, model, loc)
  }
})
ext_valid_df <- ext_valid_df %>% do.call(rbind, .)
# Remove city-specific model performance for GUAN
ext_valid_df <- ext_valid_df[!(ext_valid_df$model=="citySpecific"&ext_valid_df$loc=="GUAN"),]
ext_valid_df <- ext_valid_df[!(ext_valid_df$model=="citySpecific"&ext_valid_df$loc=="Switzerland"),]

cor(ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"),]$slr, ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"),]$UFPobs)
# Remove the anomalies in Rome data
nrow(ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"),])
nrow(ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"&ext_valid_df$slr<28),])
cor(ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"&ext_valid_df$slr<28),]$slr, ext_valid_df[(ext_valid_df$model=="pooled-deconv"&ext_valid_df$loc=="Rome"&ext_valid_df$slr<28),]$UFPobs)^2
table(ext_valid_df$loc, ext_valid_df$model)


write.csv(ext_valid_df, "results/output/ext_evaluation_df_roel.csv", row.names = FALSE)
# Generate a summary table of UFPobs by location
ext_valid_df$slr=ext_valid_df$slr/1000
ext_valid_df$UFPobs=ext_valid_df$UFPobs/1000
ufp_summary <- ext_valid_df %>%
  filter(model=="pooled") %>% 
  group_by(loc) %>%
  summarise(
    n = n(),
    Mean_UFP = mean(UFPobs, na.rm = TRUE),
    SD_UFP = sd(UFPobs, na.rm = TRUE),
    Min_UFP = min(UFPobs, na.rm = TRUE),
    Quantile_10th_UFP = quantile(UFPobs, 0.1, na.rm = TRUE),
    Median_UFP = median(UFPobs, na.rm = TRUE),
    Quantile_90th_UFP = quantile(UFPobs, 0.9, na.rm = TRUE),
    Max_UFP = max(UFPobs, na.rm = TRUE)
  )
# Output the summary table to a CSV file
write.csv(ufp_summary, "results/output/paper/3-3_extVal_ufp_summary_by_location_v2.csv", row.names = FALSE)



source('../expanse_roadtraffic/src/fun_scatterplot.R')
summary(ext_valid_df$aadt_50)
ext_valid_df$model%>%unique

source('../expanse_roadtraffic/src/fun_scatterplot.R')
pLists2 <- lapply(unique(ext_valid_df$loc), function(city_sub){
  in_df <- ext_valid_df %>% filter(loc==city_sub) %>% mutate(model2=factor(model, levels=c("pooled", "pooled-deconv", "citySpecific")))
  xy.limits <- range(c(in_df[,"slr"], in_df[,"UFPobs"]))
  xy.limits[1] <- xy.limits[1]-1
  xy.limits[2] <- xy.limits[2]+1
  # Custom labeller function for facet labels
  custom_labeller <- function(labels) {
    labels <- as.character(labels)
    paste0("(", letters[seq_along(labels)], ") ", labels)
  }
  model_levels <- c("pooled", "pooled-deconv", "citySpecific")[c("pooled", "pooled-deconv", "citySpecific")%in%unique(in_df$model)]
  blandAltman_roadtype(in_df, "slr", "UFPobs", "Predicted UFP from pooled SLR (in particles/cm^3)", 
                       paste0("Observed UFP values in ", city_sub," \n(in particles/cm^3)"), 
                       roadtype_var = "model", n_rows=2, thres_n = 10, 
                       roadtypelevel = model_levels)
  # Plot with facet_wrap and custom labeller
  ggplot(in_df, aes(x = slr, y = UFPobs)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_continuous(limits = xy.limits) + 
    scale_y_continuous(limits = xy.limits) + 
    coord_fixed(ratio = 1) +
    facet_wrap(.~model2, ncol = 1, labeller = labeller(model2 = custom_labeller)) +
    g_theme +
    labs(
      x = "Predicted UFP \n(in thousand particles/cm^3)", 
      y = "Observed UFP values \n(in thousand particles/cm^3)", 
      title = city_sub
    )
  
  if(length(unique(in_df$model))==3){
    ggsave(paste0("results/figures/paper/3-3_externalVald_scatter_", city_sub, "_v2.pdf"), 
          width=7, height=9)
  }else{
    ggsave(paste0("results/figures/paper/3-3_externalVald_scatter_", city_sub, "_v2.pdf"), 
          width=5, height=6.5)
  }
})

pLists2 <- lapply(unique(ext_valid_df$loc), function(city_sub){
  in_df <- ext_valid_df %>% filter(loc==city_sub) %>% mutate(model2=factor(model, levels=c("pooled", "pooled-deconv", "citySpecific")))

  model_levels <- c("pooled", "pooled-deconv", "citySpecific")[c("pooled", "pooled-deconv", "citySpecific")%in%unique(in_df$model)]
  if(length(unique(in_df$model))==3){
    pdf(paste0("results/figures/paper/3-3_externalVald_blandAltman_", city_sub, "_v2.pdf"), 
          width=7, height=9)
  }else{
    pdf(paste0("results/figures/paper/3-3_externalVald_blandAltman_", city_sub, "_v2.pdf"), 
          width=5, height=6.5)
  }
  blandAltman_roadtype(in_df, "slr", "UFPobs", "Predicted UFP from pooled SLR (in particles/cm^3)", 
                       paste0("Observed UFP values in ", city_sub," \n(in particles/cm^3)"), 
                       roadtype_var = "model", n_rows=2, thres_n = 10, 
                       roadtypelevel = model_levels)
  dev.off()
})

