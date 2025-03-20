add_dates_v2 <- function(i, full_names, cities, out_dir='data/temp/'){
  target_city <- cities[i]
  fname <- full_names[i]
  # out_fname <- paste0(out_dir, 'rawWithDates_', tools::toTitleCase(target_city), '.Rdata')
  out_fname <- paste0(out_dir, 'rawWithDates_', tools::toTitleCase(target_city), '.gpkg')
  print(target_city)
  if(!file.exists(out_fname)){
    dat2 <- fread(fname) # raw data
    #### Subset dataset for all cities
    # Notes: https://www.notion.so/Check-the-UFP-data-AMS-COP-and-Rotterdam-8e9d62326acd46d68e13c14e3196f9d5
    if(target_city=='amsterdam'){
        camp1 <- dat2 %>% dplyr::filter(Date>=as.Date("2019-05-20"), Date<=as.Date("2019-08-02"))  
        camp2 <- dat2 %>% dplyr::filter(Date>=as.Date("2019-12-16"), Date<=as.Date("2020-02-28"))  
        dat_subset <- rbind(camp1, camp2)
        # Check whether the outliers were excluded
        if(max(dat_subset$UFP, na.rm=T)<=500000&min(dat_subset$UFP, na.rm=T)>=500){
          # Winsorize the result
          q975 <- quantile(dat_subset$UFP, probs=c(0.975), na.rm=T) %>% as.numeric
          q25 <- quantile(dat_subset$UFP, probs=c(0.025), na.rm=T) %>% as.numeric
          dat_subset$UFP[dat_subset$UFP>=q975] <- q975
          dat_subset$UFP[dat_subset$UFP<=q25] <- q25
          
        }else{
          dat_subset$UFP[(dat_subset$UFP>500000)] <- 500000
          dat_subset$UFP[(dat_subset$UFP<500)] <- 500
        }
        
    }

    if(target_city=='copenhagen'){
        camp1 <- dat2 %>% dplyr::filter(Date>=as.Date("2018-11-15"), Date<=as.Date("2019-02-01"))  
        camp2 <- dat2 %>% dplyr::filter(Date>=as.Date("2019-04-29"), Date<=as.Date("2019-07-12"))  
        dat_subset <- rbind(camp1, camp2)
        # Check whether the outliers were excluded
        if(max(dat_subset$UFP, na.rm=T)<=500000&min(dat_subset$UFP, na.rm=T)>=500){
          # Winsorize the result
          q975 <- quantile(dat_subset$UFP, probs=c(0.975), na.rm=T) %>% as.numeric
          q25 <- quantile(dat_subset$UFP, probs=c(0.025), na.rm=T) %>% as.numeric
          dat_subset$UFP[dat_subset$UFP>=q975] <- q975
          dat_subset$UFP[dat_subset$UFP<=q25] <- q25
          
        }else{
          dat_subset$UFP[(dat_subset$UFP>500000)] <- 500000
          dat_subset$UFP[(dat_subset$UFP<500)] <- 500
        }
    }

    if(target_city=='rotterdam'){
        camp1 <- dat2 %>% dplyr::filter(Date>=as.Date("2022-11-16"), Date<=as.Date("2022-12-22"))  
        camp2 <- dat2 %>% dplyr::filter(Date>=as.Date("2023-05-23"), Date<=as.Date("2023-07-06"))  
        dat_subset <- rbind(camp1, camp2)
    }
    ttt <- unlist(lapply(strsplit(as.character(dat_subset$DateTime), ' '), `[`, 2))
    dat_subset$hour <- as.numeric(unlist(lapply(strsplit(as.character(as.character(ttt)), '\\:'), `[`, 1)))
    
    # dat_sf <- st_as_sf(dat_subset, coords=c('Longitude', 'Latitude'), 
    #                    crs=st_crs(4326))

    aq_data <- dat_subset %>% mutate(Date = as.Date(Date),
                                year = format(Date, "%Y") %>% as.numeric(),
                                year = ifelse(year==2011, 2021, year),
                                month = format(Date, "%m") %>% as.numeric(),
                                day = format(Date, "%d") %>% as.numeric(),
                                days_in_m = lubridate::days_in_month(Date) %>% as.numeric(),
                                w_day = lubridate::wday(Date, week_start = 1),
                                season = ifelse(month%in%(4:9), 'warm', 'cold'),
                                weekend = ifelse(w_day%in%(6:7), 1, 0))
    if(target_city=='amsterdam'|target_city=='copenhagen'){
      aq_data <- aq_data[(!is.na(aq_data$UFP))|(!is.na(aq_data$BC))|(!is.na(aq_data$NO2)),]
    }else{
      aq_data <- aq_data[(!is.na(aq_data$UFP))|(!is.na(aq_data$BC1_MA))|(!is.na(aq_data$BC6_MA))|(!is.na(aq_data$NO2)),]
    }
    # table(aq_data$month, aq_data$hour)
    aq_data <- aq_data[!is.na(aq_data$month),]
    aq_data <- aq_data[!is.na(aq_data$Longitude),]
    aq_data <- aq_data[!is.na(aq_data$Latitude),]
    aq_data_sf <- aq_data %>% 
      st_as_sf(., coords=c('Longitude', 'Latitude'), 
               crs=st_crs(4326)) %>% 
      st_transform(., st_crs(3035))
    
    bnd <- aq_data %>% 
      st_as_sf(., coords=c('Longitude', 'Latitude'), 
                       crs=st_crs(4326)) %>% 
      st_transform(., st_crs(3035)) %>% 
      st_bbox() %>% 
      st_as_sfc()
    if(!file.exists(paste0(out_dir, 'bnd_', tools::toTitleCase(target_city), '.shp'))){
      st_write(obj = bnd, dsn = paste0(out_dir, 'bnd_', tools::toTitleCase(target_city), '.shp'))
    }
    # saveRDS(aq_data_sf, out_fname)
    st_write(obj = aq_data_sf, dsn = out_fname)# using dat_subset
  }
}