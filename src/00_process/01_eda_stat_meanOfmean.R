## To generate some statistics of the mobile data:
# 2) the statistics of the predictors and the mean of mean
### EDA: mean of mean
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
library(splitstackshape)#stratified
library(ggstance)


all_dflist <- lapply(c("Basel", "Athens", "Barcelona", "Munich", "Lodz", "Rome", "Amsterdam", "Rotterdam", "Copenhagen"),
  function(city){
    local_pathfname <- paste0("data/processed/final_mobileData/", city, "_stat2_v3.rds")
    predictor_pathfname <- paste0('data/temp/road_centroidPts/ptsPredictors_', city, '.Rdata')

    stat <- readRDS(local_pathfname)
    predictors_df <- readRDS(predictor_pathfname)

    var_names <- c("UFPmean",# "BC1mean", "BC6mean",
                  "passes", "driving_days", "UID")
    stat2 <- stat %>% dplyr::select(all_of(var_names))
    all_df <- inner_join(predictors_df, stat2)
    all_df
})
all_dflist <- lapply(all_dflist, function(df_tbl){
  df_tbl %>% dplyr::select(all_of(names(all_dflist[[length(all_dflist)]])))
})
all_df <- do.call(rbind, all_dflist)
unique(all_df$city)
saveRDS(all_df, "data/temp/stat_new/meanOfmean_predictors_cities.Rdata")

all_df <- readRDS("data/temp/stat_new/meanOfmean_predictors_cities.Rdata")

y_var <- "UFPmean" #UFPmdMd     BC1mean
all_df2 <- all_df %>% as.data.frame() #%>% select(-geom)
stat_tabs <- lapply(unique(all_df$city), function(city_sub){
  tbl1 <- rbind(summary(all_df2 %>% dplyr::filter(city==city_sub) %>% select("UFPmean")%>% na.omit()),
                n=paste0("N=", nrow(all_df2 %>% dplyr::filter(city==city_sub) %>% select("UFPmean"))))
  # tbl2 <- rbind(summary(all_df2 %>% dplyr::filter(city==city_sub) %>% select("BC1mean", "BC6mean") %>% na.omit()),
  #               n=paste0("N=", nrow(all_df2 %>% dplyr::filter(city==city_sub) %>% select("BC1mean", "BC6mean") %>% na.omit())))
  rbind(city_sub, tbl1)# cbind(tbl1, tbl2)
  # write.csv(rbind(city, tbl), "test.csv")
}) 
write.csv(do.call(rbind, stat_tabs), "results/output/stat_meanOfMean.csv")

source("../airview_basel/src/fun_edaPlots.R")
densityPlots(dftbl=all_dfs2, c("UFPmean","BC1mean",  "BC6mean"))
ggsave('results/figures/obs_density_meanMean.png', width=9, height=4, units='in', dpi=100)

boxplotPlots(dftbl=all_dfs2, c("UFPmean","BC1mean",  "BC6mean"))
ggsave('results/figures/obs_boxplot_meanOfMean.png', width=12, height=6, units='in', dpi=100)



p1 <- boxplotPlots(dftbl=all_dfs2 %>% filter(AADT_resNo<5000), c("UFPmean","BC1mean",  "BC6mean"))+
  labs(title="AADT<5000")
p2 <- boxplotPlots(dftbl=all_dfs2 %>% filter(AADT_resNo>5000), c("UFPmean","BC1mean",  "BC6mean"))+
  labs(title="AADT>5000")
tiff("results/figures/obs_boxplot_trafficMd.png",width=12, height=6, units='in', res=100)
grid.arrange(p1, p2)
dev.off()


### separated by campaigns
test <- readRDS("data/temp/stat_new/campaign_stat_cities.Rdata")
p1 <- boxplotPlots(dftbl=test %>% filter(campaign==1), c("UFPmean","BC1mean",  "BC6mean"))+
  labs(title="1st campaign")
p2 <- boxplotPlots(dftbl=test %>% filter(campaign==2), c("UFPmean","BC1mean",  "BC6mean"))+
  labs(title="2nd campaign")
tiff("results/figures/obs_boxplot_CampaignMean.png",width=12, height=6, units='in', res=100)
grid.arrange(p1, p2)
dev.off()


#### statistics (by campaigns)
# dim(test$)
ggplot(test)+
  geom_boxplot(aes(x=UFPmean, y=city))+
  facet_wrap(campaign~., scales = "free")+
  scale_y_discrete(limits=rev)


calc_campStat <- function(city_sub, test){
  met_info <- read.csv(paste0("data/raw/gee/met_campaigns", city_sub, ".csv"))
  stat_dfs <- test %>% filter(city==city_sub)
  # table(stat_dfs$month)
  stat_dfs2 <- stat_dfs 
  out_stats <- stat_dfs %>% 
    group_by(campaign) %>% 
    summarise(
      n_ufp=sum(!is.na(UFPmean)),
      UFPmean=mean(UFPmean, na.rm=T),
      n_bc1=sum(!is.na(BC1mean)),
      BC1mean=mean(BC1mean, na.rm=T),
      n_bc6=sum(!is.na(BC6mean)),
      BC6mean=mean(BC6mean, na.rm=T))     
  out_stats$city=city_sub
  out_stats2 <- cbind(out_stats, 
                      data.frame(meaPrecip = c(met_info$precip1, met_info$precip2),
                                 ttlPrecip = c(met_info$ttlPrecip1, met_info$ttlPrecip2),
                                 temp = c(met_info$temp1, met_info$temp2),
                                 wind = c(met_info$windv1, met_info$windv2),
                                 pressure = c(met_info$pressure1, met_info$pressure2)
                                 ))
  stat_tbls <- stat_dfs %>% group_by(UID,campaign) %>% 
    summarise(UFPmean = mean(UFPmean, na.rm=T),
              BC1mean = mean(BC1mean, na.rm=T),
              BC6mean = mean(BC6mean, na.rm=T))
  
  stat_tbls2 <- inner_join(stat_dfs %>% dplyr::select(UID), stat_tbls)
  
  spatialLines_pathfname <- paste0('data/temp/road_centroidPts/linesUID_', city_sub, '.Rdata')
  df_lines <- readRDS(spatialLines_pathfname)
  stat_tbls2_sf2 <- inner_join(df_lines, stat_tbls2)
  source("src/fun_edaMaps.R")
  lapply(sort(unique(stat_tbls2$campaign)), plotMaps_camp, y_var="UFPmean",
         df_tbl=stat_tbls2_sf2, citySub=city_sub)
  
  ## both campaign
  camp_info <- readRDS("data/temp/stat_new/campaignRoadInfo_cities.Rdata")
  camp_info <- (camp_info) %>% filter(campaign_text=="both", city==city_sub)
  camp_info <- camp_info[!duplicated(camp_info$UID),]
  stat_tbls2 <- stat_tbls %>% group_by(UID) %>% summarise(UFPmean=mean(UFPmean))
  stat_tbls2_sf3 <- inner_join(df_lines, stat_tbls2[stat_tbls2$UID%in%camp_info$UID,])
  stat_tbls2_sf3$campaign <- 'both'
  lapply(sort(unique(stat_tbls2_sf3$campaign)), plotMaps_camp, y_var="UFPmean",
         df_tbl=stat_tbls2_sf3, citySub=city_sub)
  list(out_stats2, stat_tbls)##First: overall statistic; second: seasonal averages over road segments
}

outL <- lapply(c("Athens", "Basel", "Barcelona", "Munich", "Lodz", "Rome"), 
               calc_campStat, test=test)#calc_seasonStat("Basel")

# statistic
stat_season <- outL %>% lapply(., `[[`, 1)
stat_season <- do.call(rbind, stat_season)
stat_season
write.csv(stat_season, "results/output/stat_campaigns.csv")
selected_names <- stat_season %>% select(c(-campaign, -n_ufp, -n_bc1, -n_bc6, -city)) %>% names
ggplot(stat_season %>% 
         gather(., "prop", "values", c(-campaign, -n_ufp, -n_bc1, -n_bc6, -city)) %>% 
         mutate(prop=factor(prop, levels=selected_names)))+
  geom_point(aes(x=as.factor(campaign), y=values, color=prop))+
  geom_line(aes(x=campaign, y=values, color=prop))+
  facet_grid(prop~city, scale='free_y')+
  theme_bw()+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 15))
ggsave('results/figures/obs_met_campaign.png', width=12, height=11, units='in', dpi=100)

######### 
boxplotPlots(dftbl=all_dfs2, c("drvng_d"))+
  facet_zoom(xlim=c(0,10), zoom.size=0.8, horizontal=F)+
  labs(title='unique driving days')
ggsave('results/figures/obs_boxplot_drivingDays.png', width=12, height=6, units='in', dpi=100)
boxplotPlots(dftbl=all_dfs2, c("passes"))+
  facet_zoom(xlim=c(0,50), zoom.size=0.8, horizontal=F)+
  labs(title='total passes')
ggsave('results/figures/obs_boxplot_passes.png', width=12, height=6, units='in', dpi=100)

