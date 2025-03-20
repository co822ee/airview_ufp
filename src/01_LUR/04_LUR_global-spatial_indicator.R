# This is to create new linear regression that includes spatial indicators

source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
source("src/01_LUR/fun_read_mobileData_v1.R")
source("src/01_LUR/fun_read_deconv.R")
source("src/01_LUR/fun_prep_deconv_variables.R")
data_lists <- read_mobileData()
df_all2 <- data_lists[[1]] 
allPred_vars <- data_lists[[2]] 
source("src/01_LUR/fun_regroup.R")
df_all2 <- regroup_spatial_indicators(df_all2)

cities <- unique(df_all2$city)%>%as.character
deconv_dfs <- mclapply(cities, read_deconv, df_original=df_all2, mc.cores = length(cities))
deconv_dfs <- do.call(rbind, deconv_dfs)
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFPdeconv),]
deconv_dfs <- deconv_dfs[!is.na(deconv_dfs$UFP_diff),]

source("src/01_LUR/fun_regroup.R")
deconv_dfs <- regroup_spatial_indicators(deconv_dfs)


# spatial_pred <- "city" #zoneID, city, CTMufp
# df_all <- deconv_dfs
# y_var <- 'UFPdeconv'  #UFPdeconv, UFP_diff
# csv_name <- csv_name_deconv
####### build a SLR that could do combination (elimination) using city
slr_multi_spatial <- function(spatial_pred, df_all, y_var, csv_name){
  selected_predictors <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name,".csv"))$variables[-1]
  # Create a new data.frame to store the results
  results_df <- data.frame(variable = character(), p_value = numeric(), vif_value = numeric(), 
                           r_value = numeric(), spatial_pred = character(), csv_name = character(), formula_char = character(),
                           stringsAsFactors = FALSE)
  # Original result:
  formula <- as.formula(paste0(y_var, " ~ ", paste0(selected_predictors, collapse="+")))
  # Fit the model
  model <- lm(formula, data = df_all)
  
  # Calculate VIF
  model_vif <-tryCatch({
    vif(model)
  }, error = function(e){
    NA
  })
  if(!is.null(ncol(model_vif))) {
    if(ncol(model_vif)>2) model_vif <- model_vif[,ncol(model_vif)]
  }
  # Extract p-values
  p_values <- summary(model)$coefficients[,4]
  r_value <- summary(model)$adj.r.squared
  results_df <- rbind(results_df, 
                      data.frame(variable = "original setting", p_value = max(p_values, na.rm = TRUE), vif_value = max(model_vif, na.rm = TRUE),
                                 r_value=r_value, spatial_pred=spatial_pred, csv_name=csv_name, formula_char=deparse(formula)%>%paste0(., collapse='')))
  # Loop through each predictor to test the combination effect with spatial indicator
  for (predictor in selected_predictors) {
    # Create the formula for the model including interaction with spatial indicator
    selected_predictors_new <- selected_predictors
    interaction_predictor <- paste0(spatial_pred, "*", predictor)
    p_values <- NA
    r_value <- NA
    model_vif <- NA
    if(predictor!="zoneID"&!grepl("por_", predictor)){
      cat("Check for predictor variable ", predictor, '\n')
      formula <- as.formula(paste0(y_var, " ~ ", paste0(c(selected_predictors_new[!grepl(predictor, selected_predictors_new)], interaction_predictor), collapse="+")))
      # Fit the model
      model <- lm(formula, data = df_all)
      
      # Calculate VIF
      
      model_vif <-tryCatch({
        vif(model)
      }, error = function(e){
        NA
      })
      if(!is.null(ncol(model_vif))) {
        if(ncol(model_vif)>2) model_vif <- model_vif[,ncol(model_vif)]
      }
      # Extract p-values
      p_values <- summary(model)$coefficients[,4]
      r_value <- summary(model)$adj.r.squared
      # Check conditions for p-value and VIF
      if (any(p_values > 0.1, na.rm = TRUE) | any(model_vif > 3, na.rm = TRUE)) {
        # Exclude the variable one-by-one if conditions are met
        while(any(p_values > 0.1, na.rm = TRUE) | any(model_vif > 3, na.rm = TRUE)) {
          # Identify the variable with the highest VIF
          cat("Identify the variable with the highest VIF\n")
          if(is.na(all(model_vif))){
            break
          }else{
            if(max(model_vif)>3){
              max_vif_var <- names(which.max(model_vif))
              if((max_vif_var==predictor)|grepl(":", max_vif_var)|(max_vif_var==spatial_pred)|grepl(spatial_pred, max_vif_var)) break          
              # Remove the variable with the highest VIF from the model
              selected_predictors_new <- selected_predictors_new[selected_predictors_new != max_vif_var]
            }
          }
          
          # Update the formula without the variable with the highest VIF and pvalue
          cat("Update the formula without the variable with the highest VIF and pvalue\n")
          formula <- as.formula(paste0(y_var, " ~ ", paste0(c(selected_predictors_new[!grepl(predictor, selected_predictors_new)], interaction_predictor), collapse="+")))
          print(formula)
          # Refit the model
          model <- lm(formula, data = df_all)
          
          # Recalculate VIF and p-values
          model_vif <-tryCatch({
            vif(model)
          }, error = function(e){
            NA
          })
          if(!is.null(ncol(model_vif))) {
            if(ncol(model_vif)>2) model_vif <- model_vif[,ncol(model_vif)]
          }else{
            break
          }
          p_values <- summary(model)$coefficients[,4]
          r_value <- summary(model)$adj.r.squared
          
          if(max(p_values)>0.1){
            max_p_var <- names(which.max(p_values))
            if((max_p_var==predictor)|grepl(":", max_p_var)|(max_p_var==spatial_pred)|grepl(spatial_pred, max_p_var)) break
            # Remove the variable with the highest VIF from the model
            selected_predictors_new <- selected_predictors_new[selected_predictors_new != max_p_var]
          }        
          cat("Update the formula without the variable with the highest VIF and pvalue v2\n")
          formula <- as.formula(paste0(y_var, " ~ ", paste0(c(selected_predictors_new[!grepl(predictor, selected_predictors_new)], interaction_predictor), collapse="+")))
          print(formula)
          # Refit the model
          model <- lm(formula, data = df_all)
          
          # Recalculate VIF and p-values
          model_vif <-tryCatch({
            vif(model)
          }, error = function(e){
            NA
          })
          if(!is.null(ncol(model_vif))) {
            if(ncol(model_vif)>2) model_vif <- model_vif[,ncol(model_vif)]
          }else{
            break
          }
          p_values <- summary(model)$coefficients[,4]
          r_value <- summary(model)$adj.r.squared
        }
      }
      
      # Store the results
      results_df <- rbind(results_df, 
                          data.frame(variable = predictor, p_value = max(p_values, na.rm = TRUE), vif_value = max(model_vif, na.rm = TRUE),
                                     r_value=r_value, spatial_pred=spatial_pred, csv_name=csv_name, formula_char=deparse(formula)%>%paste0(., collapse='')))
    }
  }
  
  results_df
  
}
show_slr <- function( df_all, y_var, csv_name){
  selected_predictors <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name,".csv"))$variables[-1]
  
  # Original result:
  formula <- as.formula(paste0(y_var, " ~ ", paste0(selected_predictors, collapse="+")))
  # Fit the model
  model <- lm(formula, data = df_all)
  model
}
# show_slr(deconv_dfs, "UFPdeconv", "global_ufpallv31_v1Extra3Cities3_background") %>%  summary
# show_slr(deconv_dfs, "UFP_diff", "global_ufpallv31_v1Extra3Cities3_local") %>%  summary
# show_slr(deconv_dfs, "UFPmean", "global_ufpallv31_v1Extra3Citiesv3") %>%  summary

show_slr(deconv_dfs, "UFPdeconv", "global_ufpallv31_v2_background") %>%  summary
show_slr(deconv_dfs, "UFP_diff", "global_ufpallv31_v2_local_manual") %>%  summary
show_slr(deconv_dfs, "UFPmean", "global_ufpallv31_v2_manual") %>%  summary
deconv <- lapply(list("city", "zoneID", "CTMufp"), slr_multi_spatial, 
                 df_all=deconv_dfs, y_var="UFPdeconv", "global_ufpallv31_v1_background") %>% 
  do.call(rbind, .)
local <- lapply(list("city", "zoneID", "CTMufp"), slr_multi_spatial, 
                df_all=deconv_dfs, y_var="UFP_diff", "global_ufpallv31_v1_local") %>% 
  do.call(rbind, .)
one_slr <- lapply(list("city", "zoneID", "CTMufp"), slr_multi_spatial, 
                  df_all=deconv_dfs, y_var="UFPmean", "global_ufpallv31_v1") %>% 
  do.call(rbind, .)

## update 20240626
deconv <- lapply(list("city", "zoneID", "lcz"), slr_multi_spatial, 
                 df_all=deconv_dfs, y_var="UFPdeconv", "global_ufpallv31_v1Extra3Cities_background") %>% 
  do.call(rbind, .)
local <- lapply(list("city", "zoneID", "lcz"), slr_multi_spatial, 
                df_all=deconv_dfs, y_var="UFP_diff", "global_ufpallv31_v1Extra3Cities_local") %>% 
  do.call(rbind, .)
one_slr <- lapply(list("city", "zoneID", "lcz"), slr_multi_spatial, 
                  df_all=deconv_dfs, y_var="UFPmean", "global_ufpallv31_v1Extra3Cities") %>% 
  do.call(rbind, .)


## Update 20240703
one_slr <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
                  df_all=deconv_dfs, y_var="UFPmean", "global_ufpallv31_v1Extra3Citiesv2") %>% 
  do.call(rbind, .)
(one_slr) %>% filter(p_value<0.1, vif_value<3)
selected_predictors <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name,".csv"))$variables[-1]
lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors, "AADT_onRoad*zoneID"), collapse="+"))), deconv_dfs) %>% 
  summary
paste0(c(selected_predictors, "AADT_onRoad*zoneID"), collapse="+")


# Update 20241120
one_slr <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
                  df_all=deconv_dfs, y_var="UFPmean", "global_ufpallv31_v2_manual") %>% 
  do.call(rbind, .)

deconv <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
                 df_all=deconv_dfs, y_var="UFPdeconv", "global_ufpallv31_v2_background") %>% 
  do.call(rbind, .)
local <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
                df_all=deconv_dfs, y_var="UFP_diff", "global_ufpallv31_v2_local_manual") %>% 
  do.call(rbind, .)
## Update 20240730
# deconv <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
#                  df_all=deconv_dfs, y_var="UFPdeconv", "global_ufpallv31_v1Extra3Cities3_background") %>% 
#   do.call(rbind, .)
# local <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
#                 df_all=deconv_dfs, y_var="UFP_diff", "global_ufpallv31_v1Extra3Cities3_local") %>% 
#   do.call(rbind, .)

one_slr <- lapply(list("city", "zoneID", "lcz", "region"), slr_multi_spatial, 
                  df_all=deconv_dfs, y_var="UFPmean", "global_ufpallv31_v1Extra3Citiesv3") %>% 
  do.call(rbind, .)



interaction_var <- function(var_name, df_all_f, remove_var=NA){
  vars_total <- as.character(unique(df_all_f[,var_name]))
  selected_predictors <- read.csv("data/workingData/SLR_summary_model_global_ufpallv31_v2_manual.csv")$variables[-1]
  if(!is.na(remove_var)){
    selected_predictors <- selected_predictors[selected_predictors!=remove_var]
  }
  slr_city <- lm(as.formula(paste0("UFPmean ~ ", paste0(c(selected_predictors, paste0("aadt_50*", var_name)), collapse="+"))), df_all_f)
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
  
  slope_df <- df_coefs[df_coefs$predictor_name=="aadt_50",]
  slope_df2 <- df_coefs[grepl("aadt_50:", df_coefs$predictor_name),]
  slope_df2$coef <- slope_df2$coef+slope_df$coef
  slope_final <- rbind(slope_df %>% mutate(note="Baseline"),
                       slope_df2 %>% mutate(note=gsub(paste0("aadt_50:", var_name), "", predictor_name)))
  var_baseline <- vars_total[!(vars_total %in% unique(slope_final$note))]
  
  intercept_df <- df_coefs[df_coefs$predictor_name=="(Intercept)",]
  intercept_df2 <- df_coefs[grepl(paste0("^", var_name), df_coefs$predictor_name),]
  intercept_df2$coef <- intercept_df2$coef+intercept_df$coef
  
  intercept_final <- rbind(intercept_df %>% mutate(note="Baseline"),
                           intercept_df2 %>% mutate(note=gsub(var_name, "", predictor_name)))
  final_coefDF <- rbind(intercept_final %>% mutate(coef_type="intercept"),
                        slope_final %>% mutate(coef_type="slope")) %>% 
    mutate(note=ifelse(note=="Baseline", var_baseline, note),
           spatial_var = var_name)
  
  p1 <- ggplot(final_coefDF, aes(x = coef, y = note)) +
    facet_grid(.~coef_type, scale="free")+
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Coefficient Values varying by ", var_name),
         x = "Coefficient Value",
         y = var_name) +
    theme_bw()
  print(p1)
  list(model=summary(slr_city), vif=vif(slr_city), err_df=out_err_df, final_coefDF=final_coefDF)#, final_coefDF=final_coefDF
}
# 0) city interaction (with AADT)
interaction_var("city", deconv_dfs, NA)
interaction_var("city", deconv_dfs, "apor_1000")

# 1) Climate zone interaction (with AADT)
interaction_var("BCZ", deconv_dfs %>% rename(BCZ=climate_zone), NA)
interaction_var("BCZ", deconv_dfs %>% rename(BCZ=climate_zone), "apor_1000")

# 2) Region interaction (with AADT) - test
interaction_var("region", deconv_dfs, NA)
interaction_var("region", deconv_dfs, "apor_1000")
# interaction_var("region", deconv_dfs, c("alt10_enh", "ind_6000"))

# 3) LCZ interaction (with AADT) -  test
interaction_var("LCZ", deconv_dfs %>% rename(LCZ=lcz_group), NA)
interaction_var("LCZ", deconv_dfs %>% rename(LCZ=lcz_group), "apor_1000")

coef_df <- list(interaction_var("LCZ", deconv_dfs %>% rename(LCZ=lcz_group), "apor_1000")$final_coefDF,
                interaction_var("region", deconv_dfs, "apor_1000")$final_coefDF,
                interaction_var("BCZ", deconv_dfs %>% rename(BCZ=climate_zone), "apor_1000")$final_coefDF,
                interaction_var("city", deconv_dfs, "apor_1000")$final_coefDF) %>% do.call(rbind, .)
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
ggsave("results/figures/paper/appendix_varyingCoefs.pdf", width=6.8, height=8.5)

spatial_vars <- c("city", "LCZ", "BCZ", "region")
obs_dfs <- lapply(spatial_vars, function(spatial_var){
  deconv_dfs %>% rename(LCZ=lcz_group, BCZ=climate_zone) %>%
    group_by_at(spatial_var) %>%
    summarise(UFPdeconv=mean(UFPdeconv, na.rm=T),
              UFPmean=mean(UFPmean, na.rm=T),
              CTMufp=mean(CTMufp, na.rm=T),
              aadt_50=mean(aadt_50, na.rm=T)) %>% 
    gather(., 'spatial_var', 'note', all_of(spatial_var)) %>% 
    filter(!is.na(note))
}) %>% do.call(rbind, .)
obs_dfs <- obs_dfs %>% gather(., "coef_type", "coef", c("UFPdeconv", "UFPmean", "CTMufp", "aadt_50"))
all_dfs <- rbind(obs_dfs[,names(obs_dfs)] %>% mutate(obs_b="1"),
                 coef_df[,names(obs_dfs)] %>% mutate(obs_b="2"))
coef_df2 <- readRDS("data/temp/varyingInterceptOnly_coef_df.rds")
names(coef_df2)
coef_df2 <- coef_df2 %>% mutate(coef_type="varying intercept", obs_b="3")

all_dfs_final <- all_dfs %>% 
  mutate(coef_type=ifelse(coef_type=="intercept",
                               paste0("varying ", coef_type, " with\n varying slope"),
                               coef_type)) %>% 
  mutate(coef_type=ifelse(coef_type=="slope",
                               paste0("varying ", coef_type, " with\n varying intercept"),
                               coef_type)) 
all_dfs_final <- rbind(all_dfs_final, coef_df2[, names(all_dfs_final)])
head(all_dfs_final)
ggplot(all_dfs_final %>% 
         mutate(coef_type=factor(coef_type, 
                                 levels = c("varying slope with\n varying intercept", 
                                            "aadt_50",
                                            "varying intercept with\n varying slope", 
                                            "varying intercept",
                                            "UFPdeconv", "CTMufp", "UFPmean"))), 
       aes(x = coef, y = note, fill=obs_b)) +
  facet_grid(spatial_var~coef_type, scale="free")+
  geom_bar(stat = "identity", position = "dodge", col="grey") +
  labs(title = "",
       x = "Value",
       y = "") +
  scale_y_discrete(limits=rev)+
  scale_fill_manual(values=safe_colorblind_palette)+
  theme_bw()+
  g_theme+
  theme(legend.position="none",
        axis.text.x=element_text(angle = 22, hjust = 0.9, vjust=1.))
ggsave("results/figures/paper/appendix_spatiallyVaryingCoefs&Obs.pdf", width=14.7, height=8.5)
ggsave("results/figures/paper/appendix_spatiallyVaryingCoefs&Obs_aadt.pdf", width=18, height=8.0)
# deconv_dfs %>% rename(LCZ=lcz_group, BCZ=climate_zone) %>%
#   group_by(across(all_of(spatial_vars))) %>% #group_by_at(spatial_vars) %>%
#   summarise(UFPdeconv=mean(UFPdeconv, na.rm=T))

# 4) XY interaction (with AADT) -  test
interaction_var("XY", deconv_dfs, NA)
interaction_var("XY", deconv_dfs, "mjrR_2000")



ggplot(deconv_dfs, aes(x=UFPdeconv, y=climate_zone))+
  geom_boxplot()+
  labs(x="UFP deconvoluted values (background)")

ggplot(deconv_dfs %>% group_by(city) %>%  summarise(UFPdeconv=mean(UFPdeconv, na.rm=T)), 
       aes(x=UFPdeconv, y=city))+
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()+
  labs(x="Average UFP measured: deconvoluted values (background)")
ggplot(deconv_dfs %>% group_by(city) %>%  summarise(CTMufp=mean(CTMufp, na.rm=T)), 
       aes(x=CTMufp, y=city))+
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()+
  labs(x="Average CTM UFP estimates")

ggplot(deconv_dfs, aes(x=CTMufp, y=lcz_group))+
  geom_boxplot()+
  labs(x="CTM UFP estimates", y="")
ggplot(deconv_dfs, aes(x=CTMufp, y=city))+
  geom_boxplot()+
  labs(x="CTM UFP estimates", y="")

ggplot(deconv_dfs, aes(x=UFPmean, y=city))+
  geom_boxplot()+
  labs(x="UFP mean of means", y="")
boxplot(deconv_dfs$AADT_onRoad)
summary(deconv_dfs$AADT_onRoad)

ggplot(deconv_dfs %>% mutate(traffic_type=ifelse(AADT_onRoad>12115, "AADT > 12k", "AADT <= 12k")), 
       aes(x=UFPmean, y=traffic_type, fill=traffic_type))+
  geom_boxplot()+
  facet_wrap(.~city, scales="free")+
  labs(x="UFP mean of means", y="")


ggplot(deconv_dfs %>% 
         mutate(traffic_type=ifelse(AADT_onRoad < 1927, "1) AADT < 1927",
                                    ifelse(AADT_onRoad >= 1927 & AADT_onRoad < 12115, "2) 1927 <= AADT < 12115", "3) AADT > 12115"))), 
       aes(x=CTMufp, y=traffic_type, fill=traffic_type))+
  geom_boxplot()+
  facet_wrap(.~city, scales="free")+
  labs(x="UFP mean of means", y="")


ggplot(deconv_dfs %>% 
         mutate(traffic_type=ifelse(AADT_onRoad < 1927, "1) AADT < 1927",
                                    ifelse(AADT_onRoad >= 1927 & AADT_onRoad < 12115, "2) 1927 <= AADT < 12115", "3) AADT > 12115"))), 
       aes(x=CTMufp, y=traffic_type, fill=traffic_type))+
  geom_boxplot()+
  facet_wrap(.~city, scales="free")+
  labs(x="UFP mean of means", y="")

ggplot(deconv_dfs %>% 
         mutate(traffic_type=ifelse(AADT_onRoad < 1927, "1) AADT < 1927",
                                    ifelse(AADT_onRoad >= 1927 & AADT_onRoad < 12115, "2) 1927 <= AADT < 12115", "3) AADT > 12115"))), 
       aes(x=UFPmean, y=city, fill=traffic_type))+
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~traffic_type, scales="free")+
  labs(x="UFP mean of means", y="")


ggplot(deconv_dfs %>% 
         mutate(traffic_type=ifelse(AADT_onRoad < 1927, "1) AADT < 1927",
                                    ifelse(AADT_onRoad >= 1927 & AADT_onRoad < 12115, "2) 1927 <= AADT < 12115", "3) AADT > 12115"))) %>% 
         group_by(traffic_type, city) %>% 
         summarise(UFPmean=mean(UFPmean, na.rm=T)), 
       aes(x=UFPmean, y=city, fill=traffic_type))+
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~traffic_type, scales="free")+
  labs(x="Average UFP mean of means", y="")




table(deconv_dfs$lcz_group)
summary(deconv_dfs$CTMufp)

results_df2 <- rbind(deconv, local, one_slr) %>% filter(p_value<0.1, vif_value<3)




# Print the results
print(results_df2)
print(results_df2[!duplicated(results_df2$r_value), ])
print(results_df2[!duplicated(results_df2$r_value), ] %>% 
        select(-p_value, -vif_value)) %>% View

models <- lapply(1:nrow(results_df2 %>% filter(variable=='AADT_resNo')), function(row_i){
  formula <-  as.formula((results_df2 %>% filter(variable=='AADT_resNo'))[row_i, "formula_char"])
  lm(formula, deconv_dfs)
})
summary(models[[1]])#background global model
summary(models[[2]])#global model


# Predict variables
source("../EXPANSE_algorithm/scr/fun_gen_pred_df.R")
output_df <- function(model, df_all, y_var){
  slr_poll_test <- gen_pred_df(model, df_all, y_var)
  slr_poll_test <- slr_poll_test[!is.na(slr_poll_test$obs),]
  slr_poll_test <- slr_poll_test[!is.na(slr_poll_test$slr),]
  slr_poll_test
}
background_df <- output_df(models[[1]], deconv_dfs, "UFPdeconv")
source("src/fun_eval_slr_perfm.r")
######
local_df <- eval_slr_perfm(deconv_dfs, "UFP_diff", "global_ufpallv31_v1_local") 
local_df2 <- output_df(lm(as.formula(local[3, "formula_char"]), deconv_dfs), deconv_dfs, "UFP_diff")
combine_deconv_df <- function(backgroundDF, localDF, pred_df){
  all_df <- inner_join(backgroundDF %>% 
                         dplyr::select(index, slr) %>% 
                         rename(UFPdeconv=slr), 
                       localDF %>% 
                         dplyr::select(index, slr) %>% 
                         rename(UFP_diff=slr), 
                       by="index")
  all_df <- all_df %>% mutate(slr_deconv=UFPdeconv+UFP_diff)
  all_df <- all_df %>% inner_join(., pred_df %>% dplyr::select(index, UFPmean))
  all_df
}
all_df <- combine_deconv_df(background_df, local_df, deconv_dfs)
all_df2 <- combine_deconv_df(background_df, local_df2, deconv_dfs)


pooled_df <- output_df(models[[2]], deconv_dfs, "UFPmean")

error_matrix(all_df$UFPmean, all_df$slr_deconv) %>% 
  as.data.frame() %>% 
  mutate(.=round(., 2)) %>% 
  rename(deconv_interact=".")
## Force the combination 
error_matrix(all_df2$UFPmean, all_df2$slr_deconv) %>% 
  as.data.frame() %>% 
  mutate(.=round(., 2)) %>% 
  rename(deconv_interact=".")


error_matrix(pooled_df$obs, pooled_df$slr) %>% 
  as.data.frame() %>% 
  mutate(.=round(., 2)) %>% 
  rename(NoDeconv_interact=".")
