## 20240712
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
csv_name <- 'global_ufpallv31_v2'  

cities <- unique(df_all2$city)%>%as.character
results <- mclapply(cities, function(city_sub) {
  csv_name <- paste0(city_sub, '_ufpallv31_v2')  #dataset update (v1, 20240520) 
  # Filter data for the specific city
  city_data <- df_all2 %>% filter(city == city_sub)
  # Fit linear model
  source("src/fun_mclapplySLR.R") 
  slr_lists <- trainSLRAll(csv_name, city_data, allPred_vars, TRUE, city_data, 
                           obs_var="UFPmean", outDir='data/workingData/')
  lm_model <- slr_lists[[2]]
  # Summarize the model
  summary_lm <- summary(lm_model)
  # Return results
  list(
    city = city_sub,
    model_summary = summary_lm
  )
}, mc.cores = length(cities))

saveRDS(results, paste0("data/temp/", csv_name, "city_specific_models_results.Rdata"))
results <- readRDS(paste0("data/temp/", csv_name, "city_specific_models_results.Rdata"))

# Variables selected in each city
var_dfs <- lapply(results, function(result_list){
  city_sub <- result_list$city
  data.frame(city=city_sub, variables=row.names(result_list$model_summary$coefficients)[-1])
}) %>% do.call(rbind, .)
# add variables selected in Pooled model
globalModel_all <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, ".csv"))
var_global <- data.frame(city="Pooled", variables=globalModel_all$variables)
source("src/fun_vis.R")
var_dfs2 <- rbind(var_dfs, var_global) %>% 
  update_class() %>% 
  mutate(city=factor(city, levels = c(sort(as.character(unique(df_all2$city))), "Pooled")))



ggplot(var_dfs2 %>% mutate(col=ifelse(city=='Pooled', 'red', 'black')))+
  geom_tile(aes(x=city, y=variables, fill=col))+
  labs(x='model', y='', title=paste0('SLR: selected variables'))+
  scale_fill_manual(values=c('red'='red','black'='black'))+
  g_theme+
  theme(
    axis.text.x = element_text(angle = 20),
    legend.position = "none"
    # panel.border = element_rect(size = 0.25, linetype = 'solid', fill = 'transparent')
  )
ggsave("results/figures/global_city-selected_variables.pdf", width=8, height=9)
### grouping the variables based on the buffer sizes
pred_all <- as.character(unique(var_dfs2$variables))
pred_Buffer <- pred_all[grepl("_\\d+", pred_all)]

# Define the grouping function
group_by_range <- function(x) {
  num <- as.numeric(x)
  if (num >= 25 & num < 500) {
    return("25-500")
  } else if (num >= 500 & num < 1000) {
    return("500-1000")
  } else if (num >= 1000 & num < 5000) {
    return("1000-5000")
  } else if (num >= 5000 & num <= 10000) {
    return("5000-10000")
  } else {
    return("other")
  }
}

# Group variables based on the strings before the underscore and the defined ranges
buffer_groups <- split(pred_Buffer, list(sub("_(\\d+).*", "", pred_Buffer), 
                                         sapply(sub(".*_(\\d+).*", "\\1", pred_Buffer), group_by_range)))

# Print or use the grouped variables as needed
print(buffer_groups)
freq_df <- table(var_dfs2$variables, var_dfs2$city) %>% 
  as.data.frame() %>% 
  rename(variables=Var1, city=Var2)

# Group the values of Freq in freq_df using buffer_groups
freq_df$group <- sapply(freq_df$variables, function(var) {
  for (group in names(buffer_groups)) {
    if (var %in% buffer_groups[[group]]) {
      return(group)
    }
  }
  return(as.character(var))
})


print(freq_df)
freq_df2 <- freq_df %>% 
  group_by(group, city) %>% 
  summarise(Freq=sum(Freq)) %>% 
  ungroup() %>% 
  rename(variables=group) %>% 
  # update_class() %>% 
  mutate(city=factor(city, levels = c(sort(as.character(unique(df_all2$city))), "Pooled")))

result_vector <- c("aadt.25-500", "mjrR.1000-5000", "mjrR.25-500", "mjrR.500-1000", "mjrR.5000-10000", "allR.5000-10000", 
                  "CTMufp", 
                  "apor.1000-5000", "apor.5000-10000", "ind.1000-5000", "ind.5000-10000", "por.1000-5000", "por.5000-10000", 
                  "FsfRst.500-1000", "FsfRst.1000-5000", 
                  "imd.25-500", "nat.1000-5000", "nat.5000-10000", 
                  "res.500-1000", "res.1000-5000", 
                  "alt10_enh", "anbh", "canyon.25-500", "canyonSD.25-500", 
                  "ugr.25-500", "ugr.1000-5000","ugr.5000-10000", "pop20.25-500", "precip", "pressure", "temp",  "wind")
result_vector <- rev(result_vector)
freq_df2$variables2 <- factor(freq_df2$variables, levels = result_vector)

ggplot(freq_df2 %>% 
         mutate(col=ifelse(city=='Pooled', 'red', 'black'),
                variables = factor(variables, levels = result_vector)) %>% 
         filter(Freq!=0))+
  geom_tile(aes(x=city, y=as.numeric(variables), fill=col))+
  labs(x='model', y='', title=paste0('SLR: selected variables'))+
  scale_fill_manual(values=c('red'='red','black'='black'))+
  g_theme+
  
  theme(axis.text.x = element_text(angle = 20, hjust = 0.9, vjust=1.),
        legend.position = "none",
        axis.text.y = element_text(size = 13, hjust = 0) #// Adjusted for left alignment
  )+
  scale_y_continuous(# Add a second axis and specify its features
    breaks = (1:length(result_vector)),
    labels = result_vector,
    sec.axis = sec_axis(~ ., breaks = 1:length(result_vector), 
                        labels=result_vector) # Text for the right y-axis
  )

  # scale_y_continuous(breaks = 1:length(unique(irislabs1)),
  #                    labels = unique(irislabs1),
  #                    sec.axis = sec_axis(~.,
  #                                        breaks = 1:length(unique(irislabs1)),
  #                                        labels = unique(irislabs1)))
ggsave("results/figures/paper/3-2_modelStructure_global_city-selected_variables_grouped.pdf", width=7.7, height=7.3)

# Standardize the coefficient values
source("src/01_LUR/fun_norm_coef.R")
city_sub <- "Rotterdam"

local_coefs <- lapply(cities, function(city_sub){
  csv_name <- paste0(city_sub, '_ufpallv31_v1Extra3Citiesv2') 
  normalize_coef(paste0("data/workingData/SLR_summary_model_", csv_name, ".csv") %>% 
                   read.csv, df_all2) %>% mutate(city=city_sub)
  
})
local_coefs <- do.call(rbind, local_coefs)
ggplot(local_coefs %>% mutate(negVar=ifelse(beta_norm>=0, "Positive", "Negative"),
                              beta_norm_abs=abs(beta_norm)) %>% filter(variables!='pressure'))+
  geom_tile(aes(y=variables, x=city, fill=beta_norm_abs))+
  scale_y_discrete(limits=rev)+ 
  facet_wrap(negVar~.)+
  theme_bw()+
  scale_fill_gradient2(low = "white", high = "brown", mid = "pink")+
  theme(axis.title = element_text(size = 12),
        title = element_text(size=15),
        axis.text = element_text(size = 10),
        # legend.title = element_text(size = 16),
        # legend.text = element_text(size = 16),
        strip.text.y = element_text(size = 10)
  )+
  labs(title="Normalized coefficients ", x="normalized regression coefficients (multiplied by q99-q1)")
# Trash....


# use the global structure to build city-specific models


local_coefs <- lapply(cities, function(city_sub){
  print(city_sub)
  obs_var="UFPmean"
  city_data <- df_all2 %>% filter(city == city_sub)
  globalModel_all <- read.csv(paste0("data/workingData/SLR_summary_model_global_ufpallv31_v1Extra3Citiesv2.csv"))
  vars_selected <- globalModel_all$variables[-1]
  localModel <- lm(formula = as.formula(paste0(obs_var, "~", paste0(vars_selected[!grepl("zoneID", vars_selected)], collapse = "+"))), city_data)
  if(any(is.na(localModel$coefficients))){
    localModel <- lm(formula = as.formula(paste0(obs_var, "~", paste0(vars_selected[!grepl("zoneID", vars_selected)][!is.na(localModel$coefficients[-1])], collapse = "+"))), city_data)
  }
  summarymodel <- c("Final", names(localModel$coefficients)[-1])
  vif <- c("NA", vif(localModel))
  summarymodel <- cbind(summarymodel,
                        summary(localModel)$coefficients,
                        vif)
  colnames(summarymodel) <- c("variables", "beta", "Std.Error", "t", "P", "VIF")
  localModel_df <- as.data.frame(summarymodel)
  localModel_df[,2:6] <- sapply(localModel_df[,2:6], as.numeric)
  source("src/01_LUR/fun_norm_coef.R")
  local_coefs <- normalize_coef(localModel_df, df_all2) %>% 
    mutate(model=city_sub, poll=obs_var)## %>% 
  # mutate_at(vars(starts_with("nat_"), starts_with("ugr_"), starts_with("precip"), "wind", "alt10_enh"), list(~ . * -1))
  
  local_coefs
})
local_coefs <- do.call(rbind, local_coefs)
ggplot(local_coefs %>% filter(variables!="alt10_enh"&variables!="precip"))+
  geom_tile(aes(y=variables, x=model, fill=beta_norm))+
  scale_y_discrete(limits=rev)+ 
  theme_bw()+
  scale_fill_gradient2(low = "white", high = "brown", mid = "pink")+
  theme(axis.title = element_text(size = 12),
        title = element_text(size=15),
        axis.text = element_text(size = 10),
        # legend.title = element_text(size = 16),
        # legend.text = element_text(size = 16),
        strip.text.y = element_text(size = 10)
  )+
  labs(title="Normalized coefficients ", x="normalized regression coefficients (multiplied by q99-q1)")
#-> trash
# Add global ones
globalModel_all <- read.csv(paste0("data/workingData/SLR_summary_model_global_ufpallv31_v1Extra3Citiesv2.csv"))
global_coefs <- normalize_coef(globalModel_all %>% filter(variables!='zoneID'), df_all2) %>% 
  mutate(model='Pooled', poll=obs_var)
global_coefs <- global_coefs[,names(local_coefs)]
coefs_dfAll <- rbind(local_coefs, global_coefs) %>% 
  mutate(model=factor(model, levels = c(sort( as.character(unique(df_all2$city))), "Pooled")))
ggplot(coefs_dfAll)+
  geom_bar(aes(x=beta_norm, y=model), stat = 'identity', position='dodge')+
  scale_y_discrete(limits=rev)+ 
  facet_wrap(variables~., scale='free')+
  theme_bw()+
  scale_fill_gradient2(low = "white", high = "brown", mid = "pink")+
  theme(axis.title = element_text(size = 12),
        title = element_text(size=15),
        axis.text = element_text(size = 10),
        # legend.title = element_text(size = 16),
        # legend.text = element_text(size = 16),
        strip.text.y = element_text(size = 10)
  )+
  labs(title="Normalized coefficients ", x="normalized regression coefficients (multiplied by q99-q1)")
ggsave("results/figures/global_localCoefsnormalized.pdf", width=11, height=9)

