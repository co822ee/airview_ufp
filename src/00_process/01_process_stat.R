## To generate some statistics of the mobile data:
# 1) campaign start and end time 



source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
if(!dir.exists("data/temp/stat_new/")) dir.create("data/temp/stat_new/")
newStat_pathfname <- "data/temp/stat_new/campaign_stat_cities.Rdata"
newStat2_pathfname <- "data/temp/stat_new/campaign_stat_cities2.Rdata"
gen_newStat <- function(city_sub){
  # Determine the campaign for the stat 
  source("../expanse_cnossos_v2/src/snellius/src/fun_log.R")
  if(!dir.exists("data/temp/log/")) dir.create("data/temp/log/")
  logcampaign_pathfname <- "data/temp/log/campaign_time.csv"
  stat_pathfname <- paste0("data/processed/", city_sub, "stat_v3.rds") ## averages on driving days and road segments
  
  # Calculate how many roads were only measured in one season 
  ## (We can calculate the number by road types and area type as well)
  stat_dfs <- readRDS(stat_pathfname)
  stat_dfs$date <- as.Date(paste(stat_dfs$year, stat_dfs$month, stat_dfs$day, sep = "-"))
  stat_dfs2 <- stat_dfs[order(stat_dfs$date),]
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
  write_log(time_log, logcampaign_pathfname)
  # stat 
  
  stat_dfs2$campaign <- c(rep(1, which.max(stat_dfs2$date_diff)-1), 
                          rep(2, nrow(stat_dfs2)-which.max(stat_dfs2$date_diff)+1))
  # View(stat_dfs2[which.max(stat_dfs2$date_diff)-2:which.max(stat_dfs2$date_diff)+4,])
  # By month
  # table(stat_dfs$month)
  stat_dfs2$city <- city_sub
  stat_dfs2 <- stat_dfs2 %>% as.data.frame() %>% dplyr::select(-geom)
  if('season'%in%names(stat_dfs2)) stat_dfs2 <- stat_dfs2 %>% dplyr::select(-season)
  if(city_sub%in%c("Amsterdam", "Copenhagen", "Rotterdam")){
    stat_dfs2 <- stat_dfs2 %>% dplyr::select(-matches("^NO2"))
  }
  stat_dfs2
} 
newstat <- lapply(c("Basel", "Athens", "Barcelona", "Munich", "Lodz", "Rome", "Rotterdam"), gen_newStat)
newstat2 <- lapply(c("Amsterdam", "Copenhagen"), gen_newStat)
newstat <- lapply(newstat, function(df_tbl){
  names(df_tbl) <- names(newstat[[1]])
  df_tbl
})
newstat <- do.call(rbind, newstat)

newstat2 <- lapply(newstat2, function(df_tbl){
  names(df_tbl) <- names(newstat2[[1]])
  df_tbl
})
newstat2 <- do.call(rbind, newstat2)

saveRDS(newstat, newStat_pathfname)
saveRDS(newstat2, newStat2_pathfname)
table(newstat$campaign, newstat$city)


######## determined which campaigns a road segment in a city was measured (both campaigns or either of the campaigns)######
library(dplyr)
newStat_pathfname <- "data/temp/stat_new/campaign_stat_cities.Rdata"
newStat2_pathfname <- "data/temp/stat_new/campaign_stat_cities2.Rdata"

campaignRoadInfo_pathfname <- "data/temp/stat_new/campaignRoadInfo_cities.Rdata"

calc_campaignRoadInfo <- function(newStatPathfname){
  newstat <- readRDS(newStatPathfname)
  class(newstat)
  # Find which roads (UIDs) are available in both campaigns or only available in one of the campaigns
  n_campaigns <- newstat %>% 
    group_by(city, UID, campaign) %>% 
    summarise(count=n()) %>% 
    mutate(measured_boolean = ifelse(count>0, 1, 0))
  measured_campaigns <- n_campaigns %>% 
    ungroup() %>% 
    group_by(city, UID) %>% 
    summarise(measured_nCampaigns=sum(measured_boolean)) # the number of campaigns with the roads being measured (min=1, max=2)

  both_campaignsUID <- measured_campaigns %>% filter(measured_nCampaigns==2) %>% mutate(campaign_text="both")
  one_campaignUID <- measured_campaigns %>% filter(measured_nCampaigns!=2) %>% mutate(campaign_text="only one")

  # Get a data frame that includes UID, city, campaign_text 
  rbind(one_campaignUID, both_campaignsUID)
  n_campaigns %>% head
  df_tbl <- inner_join(rbind(one_campaignUID, both_campaignsUID),
                      n_campaigns)
  # head(df_tbl) # This tells you the road segments have whether a road segment (UID) in a city has measurements from both campaign or from only one campaign
  # measured_nCampaigns: the number of campaigns where the road segment (UID) in a city was measured.
  # campaign_text: "only one" or "both"
  # campaign: the first campaign or the second campaign
  # count: the number of driving days within a campaign where a road segment was measred
  # measured_boolean: (can be discarded) temporarily created variable to help deriving the campaign_text
  df_tbl
}
df_tbl <- calc_campaignRoadInfo(newStat_pathfname)
df_tbl2 <- calc_campaignRoadInfo(newStat2_pathfname)
df_tbl_all <- rbind(df_tbl, df_tbl2)
saveRDS(df_tbl_all %>% dplyr::select(-measured_boolean), 
        campaignRoadInfo_pathfname)


