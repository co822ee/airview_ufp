prod_stat <- function(cityName, inDir=in_dir, outDir=out_dir){
  
  # How many passes are there on each road segment on each driving day?
  # The average of the observed values on each road segment on each driving day/pass
  if(!file.exists(paste0(outDir, cityName,'stat_v3.rds'))){
    rawptlinesv3 <- st_read(paste0(inDir, cityName, "_rawPtsLines_v3.gpkg"))
    if(cityName=="Amsterdam"|cityName=="Copenhagen"){
          stat <- rawptlinesv3 %>% as.data.frame() %>% group_by(UID, day, month, year, w_day, weekend) %>% 
              summarise(count=n(),
                        UFPmean = mean(UFP, na.rm=T),
                        UFPsd = sd(UFP, na.rm=T),
                        UFPq25 = quantile(UFP, na.rm=T, probs=0.25),
                        UFPq50 = quantile(UFP, na.rm=T, probs=0.5),
                        UFPq75 = quantile(UFP, na.rm=T, probs=0.75),
                        
                        BCmean = mean(BC, na.rm=T),
                        BCsd = sd(BC, na.rm=T),
                        BCq25 = quantile(BC, na.rm=T, probs=0.25),
                        BCq50 = quantile(BC, na.rm=T, probs=0.5),
                        BCq75 = quantile(BC, na.rm=T, probs=0.75),
                        
                        NO2mean = mean(NO2, na.rm=T),
                        NO2sd = sd(NO2, na.rm=T),
                        NO2q25 = quantile(NO2, na.rm=T, probs=0.25),
                        NO2q50 = quantile(NO2, na.rm=T, probs=0.5),
                        NO2q75 = quantile(NO2, na.rm=T, probs=0.75))

    }else{
          stat <- rawptlinesv3 %>% as.data.frame() %>% group_by(UID, day, month, year, w_day, weekend) %>% 
                    summarise(count=n(),
                              UFPmean = mean(UFP, na.rm=T),
                              UFPsd = sd(UFP, na.rm=T),
                              UFPq25 = quantile(UFP, na.rm=T, probs=0.25),
                              UFPq50 = quantile(UFP, na.rm=T, probs=0.5),
                              UFPq75 = quantile(UFP, na.rm=T, probs=0.75),
                              
                              BC1mean = mean(BC1_MA, na.rm=T),
                              BC1sd = sd(BC1_MA, na.rm=T),
                              BC1q25 = quantile(BC1_MA, na.rm=T, probs=0.25),
                              BC1q50 = quantile(BC1_MA, na.rm=T, probs=0.5),
                              BC1q75 = quantile(BC1_MA, na.rm=T, probs=0.75),
                              
                              BC6mean = mean(BC6_MA, na.rm=T),
                              BC6sd = sd(BC6_MA, na.rm=T),
                              BC6q25 = quantile(BC6_MA, na.rm=T, probs=0.25),
                              BC6q50 = quantile(BC6_MA, na.rm=T, probs=0.5),
                              BC6q75 = quantile(BC6_MA, na.rm=T, probs=0.75),
                              
                              NO2mean = mean(NO2, na.rm=T),
                              NO2sd = sd(NO2, na.rm=T),
                              NO2q25 = quantile(NO2, na.rm=T, probs=0.25),
                              NO2q50 = quantile(NO2, na.rm=T, probs=0.5),
                              NO2q75 = quantile(NO2, na.rm=T, probs=0.75))
          
          #2024-06-25 09:05:52 UTC - ...
    }
    # 10:59 - 11:00
    uid_sf <- rawptlinesv3 %>% dplyr::select(UID)
    uid_sf <- uid_sf[!duplicated(uid_sf$UID),]
    stat_final <- inner_join(stat, uid_sf)
    
    # Why there is one road segment have more than 4k observation time stamp in one driving day?
    stat_final[which.max(stat_final$count),]
    # saveRDS(stat, 'data/processed/stat.rds')
    stat_final <- stat_final %>% mutate(season=ifelse(month%in%(4:9), 'warm', 'cold'))
    saveRDS(stat_final, paste0(outDir, cityName,'stat_v3.rds'))
  }else{
    stat_final = readRDS(paste0(outDir, cityName,'stat_v3.rds'))
  }
  # stat_final
}
prod_statOfStat <- function(cityName, inDir=in_dir, outDir=out_dir){
  
  if(!file.exists(paste0(outDir, cityName,'_stat2_v3.rds'))){
    stat <- readRDS(paste0(inDir, cityName, "stat_v3.rds"))
    if(cityName=="Amsterdam"|cityName=="Copenhagen"){
      stat_2 <- stat %>% 
        as.data.frame() %>% 
        group_by(UID) %>% 
        summarise(driving_days=n(), 
                  passes=sum(count),
                  ############## UFP ##############
                  UFPmsd = mean(UFPsd, na.rm=T),
                  UFPmq25 = mean(UFPq25, na.rm=T),
                  UFPmq50 = mean(UFPq50, na.rm=T),
                  UFPmq75 = mean(UFPq75, na.rm=T),
                  
                  UFPmedMedian = median(UFPq50, na.rm=T),
                  UFPsd = sd(UFPmean, na.rm=T),
                  UFPq25 = quantile(UFPmean, na.rm=T, probs=0.25),
                  UFPq50 = quantile(UFPmean, na.rm=T, probs=0.5),
                  UFPq75 = quantile(UFPmean, na.rm=T, probs=0.75),
                  UFPmean = mean(UFPmean, na.rm=T),
                  
                  ############## BC ##############
                  BCmsd = mean(BCsd, na.rm=T),
                  BCmq25 = mean(BCq25, na.rm=T),
                  BCmq50 = mean(BCq50, na.rm=T),
                  BCmq75 = mean(BCq75, na.rm=T),
                  
                  BCmedMedian = median(BCq50, na.rm=T),
                  BCsd = sd(BCmean, na.rm=T),
                  BCq25 = quantile(BCmean, na.rm=T, probs=0.25),
                  BCq50 = quantile(BCmean, na.rm=T, probs=0.5),
                  BCq75 = quantile(BCmean, na.rm=T, probs=0.75),
                  BCmean = mean(BCmean, na.rm=T),

                  ############## NO2 ##############
                  NO2msd = mean(NO2sd, na.rm=T),
                  NO2mq25 = mean(NO2q25, na.rm=T),
                  NO2mq50 = mean(NO2q50, na.rm=T),
                  NO2mq75 = mean(NO2q75, na.rm=T),
                  
                  NO2medMedian = median(NO2q50, na.rm=T),
                  NO2sd = sd(NO2mean, na.rm=T),
                  NO2q25 = quantile(NO2mean, na.rm=T, probs=0.25),
                  NO2q50 = quantile(NO2mean, na.rm=T, probs=0.5),
                  NO2q75 = quantile(NO2mean, na.rm=T, probs=0.75),
                  NO2mean = mean(NO2mean, na.rm=T)
        )
    }else{
      stat_2 <- stat %>% 
        as.data.frame() %>% 
        group_by(UID) %>% 
        summarise(driving_days=n(), 
                  passes=sum(count),
                  ############## UFP ##############
                  UFPmsd = mean(UFPsd, na.rm=T),
                  UFPmq25 = mean(UFPq25, na.rm=T),
                  UFPmq50 = mean(UFPq50, na.rm=T),
                  UFPmq75 = mean(UFPq75, na.rm=T),
                  
                  UFPmedMedian = median(UFPq50, na.rm=T),
                  UFPsd = sd(UFPmean, na.rm=T),
                  UFPq25 = quantile(UFPmean, na.rm=T, probs=0.25),
                  UFPq50 = quantile(UFPmean, na.rm=T, probs=0.5),
                  UFPq75 = quantile(UFPmean, na.rm=T, probs=0.75),
                  UFPmean = mean(UFPmean, na.rm=T),
                  
                  ############## BC1 ##############
                  BC1msd = mean(BC1sd, na.rm=T),
                  BC1mq25 = mean(BC1q25, na.rm=T),
                  BC1mq50 = mean(BC1q50, na.rm=T),
                  BC1mq75 = mean(BC1q75, na.rm=T),
                  
                  BC1medMedian = median(BC1q50, na.rm=T),
                  BC1sd = sd(BC1mean, na.rm=T),
                  BC1q25 = quantile(BC1mean, na.rm=T, probs=0.25),
                  BC1q50 = quantile(BC1mean, na.rm=T, probs=0.5),
                  BC1q75 = quantile(BC1mean, na.rm=T, probs=0.75),
                  BC1mean = mean(BC1mean, na.rm=T),
                  
                  ############## BC6 ##############
                  BC6msd = mean(BC6sd, na.rm=T),
                  BC6mq25 = mean(BC6q25, na.rm=T),
                  BC6mq50 = mean(BC6q50, na.rm=T),
                  BC6mq75 = mean(BC6q75, na.rm=T),
                  
                  BC6medMedian = median(BC6q50, na.rm=T),
                  BC6sd = sd(BC6mean, na.rm=T),
                  BC6q25 = quantile(BC6mean, na.rm=T, probs=0.25),
                  BC6q50 = quantile(BC6mean, na.rm=T, probs=0.5),
                  BC6q75 = quantile(BC6mean, na.rm=T, probs=0.75),
                  BC6mean = mean(BC6mean, na.rm=T),
                  
                  ############## NO2 ##############
                  NO2msd = mean(NO2sd, na.rm=T),
                  NO2mq25 = mean(NO2q25, na.rm=T),
                  NO2mq50 = mean(NO2q50, na.rm=T),
                  NO2mq75 = mean(NO2q75, na.rm=T),
                  
                  NO2medMedian = median(NO2q50, na.rm=T),
                  NO2sd = sd(NO2mean, na.rm=T),
                  NO2q25 = quantile(NO2mean, na.rm=T, probs=0.25),
                  NO2q50 = quantile(NO2mean, na.rm=T, probs=0.5),
                  NO2q75 = quantile(NO2mean, na.rm=T, probs=0.75),
                  NO2mean = mean(NO2mean, na.rm=T)
        )
    }

    uid_sf <- stat %>% dplyr::select(UID, geom) %>% st_as_sf()
    uid_sf <- uid_sf[!duplicated(uid_sf$UID),]
    stat_2_final <- inner_join(stat_2, uid_sf)
    
    saveRDS(stat_2_final, paste0(outDir, cityName,'_stat2_v3.rds'))
    st_write(stat_2_final, paste0(outDir, cityName,'_stat2_v3.shp'))
  }else{
    stat_2_final = readRDS(paste0(outDir, cityName,'_stat2_v3.rds'))
  }
}
