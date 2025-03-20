#################### Mobile data only (driving patterns ) #############
##### for Method section
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")
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
# Aggregate the mobile data
unique_uids_per_city <- df_all2 %>% 
  group_by(city) %>% 
  summarise(unique_UIDs = n_distinct(UID), # number of total unique driven road segments
            avg_count = round(mean(count), 1) # average unique driving days
  )



#### Total driving days (and period)
in_dir <- "data/processed/"
out_dir <- 'data/processed/final_mobileData/'
if(!dir.exists(out_dir)) dir.create(out_dir)


fnames <- list.files(in_dir, 'stat_v3.rds')
cityNames <- strsplit(fnames, "_") %>% 
  lapply(., `[[`, 1) %>% 
  unlist
cityNames <- gsub("stat", "", cityNames)
cityNames <- cityNames[nchar(cityNames)!=0]

df_counts <- lapply(cityNames, function(city_sub){
  stat_pathfname <- paste0("data/processed/", city_sub, "stat_v3.rds") ## averages on driving days and road segments
  
  # Calculate how many roads were only measured in one season 
  ## (We can calculate the number by road types and area type as well)
  stat_dfs <- readRDS(stat_pathfname)
  stat_dfs$date <- as.Date(paste(stat_dfs$year, stat_dfs$month, stat_dfs$day, sep = "-"))
  stat_dfs2 <- stat_dfs[order(stat_dfs$date),]
  # Number of total driving days:
  unique_dates <- length(unique(stat_dfs2$date))
  # Number of total driving days separated by campaigns:
  # determine the campaign
  stat_dfs2$date_diff <-c(0,  as.numeric(diff(stat_dfs2$date, lag=1)))
  # last day of the first campaign
  stat_dfs2[1,]$date
  stat_dfs2[which.max(stat_dfs2$date_diff)-1,]$date
  # first day of the second campaign
  stat_dfs2[which.max(stat_dfs2$date_diff),]$date
  stat_dfs2[nrow(stat_dfs2),]$date
  
  time_log <- data.frame(city=city_sub, 
                         start1=stat_dfs2[1,]$date, 
                         end1=stat_dfs2[which.max(stat_dfs2$date_diff)-1,]$date,
                         start2=stat_dfs2[which.max(stat_dfs2$date_diff),]$date,
                         end2=stat_dfs2[nrow(stat_dfs2),]$date)
  
  n1 <- length(unique(stat_dfs2$date[stat_dfs2$date<=time_log$end1]))
  n2 <- length(unique(stat_dfs2$date[(stat_dfs2$date<=time_log$end2)&(stat_dfs2$date>=time_log$start2)]))
  
  cbind(time_log, data.frame(n1=n1, n2=n2, unique_dates=unique_dates)) #unique_dates: total driving days
}) %>% do.call(rbind, .)
df_counts
# Final output
unique_uids_per_city
final_df <- inner_join(unique_uids_per_city, df_counts, by='city')
final_dft <- data.frame(t(final_df))
final_dft$rowname = row.names(final_dft)
write.csv(final_df, "results/output/paper/Table1_mobileData.csv", row.names = F)