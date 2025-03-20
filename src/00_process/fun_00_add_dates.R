add_dates <- function(fname, in_dir, out_dir='data/temp/'){
  target_city <- (strsplit(fname, "_")[[1]] %>% strsplit(., " "))[[2]][1]
  print(target_city)
  if(!file.exists(paste0('data/temp/rawWithDates_', target_city, '.gpkg'))){
    dat2 <- read.csv(paste0(in_dir, fname)) # raw data
    # View(dat2[is.na(dat2$Date),])
    # View(dat2[is.na(dat2$DateTime),])
    
    ttt <- unlist(lapply(strsplit(as.character(dat2$DateTime), ' '), `[`, 2))
    dat2$hour <- as.numeric(unlist(lapply(strsplit(as.character(as.character(ttt)), '\\:'), `[`, 1)))
    
    dat_sf <- st_as_sf(dat2, coords=c('Longitude', 'Latitude'), 
                       crs=st_crs(4326))
    
    if(target_city=='Barcelona'){
      aq_data <- dat_sf %>% mutate(Date = as.Date(DateTime),
                                   year = format(Date, "%Y") %>% as.numeric(),
                                   year = ifelse(year==2011, 2021, year),
                                   month = format(Date, "%m") %>% as.numeric(),
                                   day = format(Date, "%d") %>% as.numeric(),
                                   days_in_m = lubridate::days_in_month(Date) %>% as.numeric(),
                                   w_day = lubridate::wday(Date, week_start = 1),
                                   season = ifelse(month%in%(4:9), 'warm', 'cold'),
                                   weekend = ifelse(w_day%in%(6:7), 1, 0))
    }else{
      aq_data <- dat_sf %>% mutate(Date = as.Date(Date),
                                   year = format(Date, "%Y") %>% as.numeric(),
                                   year = ifelse(year==2011, 2021, year),
                                   month = format(Date, "%m") %>% as.numeric(),
                                   day = format(Date, "%d") %>% as.numeric(),
                                   days_in_m = lubridate::days_in_month(Date) %>% as.numeric(),
                                   w_day = lubridate::wday(Date, week_start = 1),
                                   season = ifelse(month%in%(4:9), 'warm', 'cold'),
                                   weekend = ifelse(w_day%in%(6:7), 1, 0))
    }
    aq_data <- aq_data[(!is.na(aq_data$UFP))|(!is.na(aq_data$BC1_MA))|(!is.na(aq_data$BC6_MA)),]
    table(aq_data$month, aq_data$hour)
    aq_data <- aq_data[!is.na(aq_data$month),]
    bnd <- aq_data %>% 
      st_transform(., st_crs(3035)) %>% 
      st_bbox() %>% 
      st_as_sfc()
    if(!file.exists(paste0(out_dir, 'bnd_', target_city, '.shp'))){
      st_write(obj = bnd, dsn = paste0(out_dir, 'bnd_', target_city, '.shp'))
    }
    st_write(obj = aq_data, dsn = paste0(out_dir, 'rawWithDates_', target_city, '.gpkg'))# using dat2
  }
}