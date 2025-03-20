library(sf)
library(dplyr)
library(lwgeom)
library(rgeos)

# packageurl = 'https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz'
# install.packages(packageurl, repos=NULL, type='source')

library(parallel)
library(pbapply)
library(dplyr)
library(data.table)

#######
raw_dir='data/raw/'
rawRoad_dir=paste0(raw_dir, 'Airview_Traffic/') # data downloaded from GEE (within the specified bnd) 25/09/2023
tmp_dir='data/temp/'
src_dir='src/'

###### process raw data points (add datetimes)
### data: EXPANSE cities
in_dir <- raw_dir
out_dir <- 'data/temp/'
source('src/00_process/fun_00_add_dates.R')
# fnames <- list.files(in_dir, '2023-07-20')
# fnames <- list.files(in_dir, '2024-03-15') # for Barcelona, Lodz
lapply(fnames, add_dates, in_dir=in_dir, out_dir=out_dir) 

### data: AMS, COP, ROT
target_dir <- "data/raw/nl, dk/"
out_dir <- 'data/temp/'
source('src/00_process/fun_00_add_dates_v2.R')

cities <- c("amsterdam", "copenhagen", "rotterdam")  #list.dirs(target_dir)
fnames <- list.files(paste0(target_dir, cities), pattern = ".csv", full.names = TRUE)
mclapply(1:length(fnames), add_dates_v2, out_dir=out_dir, cities=cities, full_names=fnames, 
         mc.cores = 3) 


### output:
## rawWithDates_[city].gpkg
## bnd_[city].shp  ### bnd shp file for GEE (to export road segment for later)
## (Manually) export bnd to GEE
## Then run the GEE script:users/youchenshenuu/airview/share_predTraffic_Airview.js

################ create 60m road segments ############
in_dir <- rawRoad_dir #contains files of osm network for cities
out_dir <- 'data/temp/'

source('src/00_process/fun_01_split_roads50m.R') #00_roads_seg50m.R
fnames <- list.files(in_dir, 'OSMroads_')
fnames <- fnames[grepl('.shp', fnames)]
lapply(fnames, split_roads50m, in_dir=in_dir, out_dir=out_dir, src_dir='src/')
### output:
## OSMroads_[city]_seg50m_v2.gpkg


############ snap raw data points to road segments (in parallel) ############
in_dir <- tmp_dir
out_dir='data/temp/snapped/'
source('src/00_process//fun_02_ptsToRoads.R')  #01_ptsToRoads.R
fnames <- list.files(in_dir, 'OSMroads_')
fnames <- fnames[grepl('_seg50m_v2.gpkg', fnames)]
cityNames <- gsub('OSMroads_', '', fnames) %>% 
  gsub('_seg50m_v2.gpkg','',.)

lapply(cityNames, ptsToRoads, in_dir=in_dir, n_cores=detectCores(), 
       out_dir=out_dir, core_no=10)
# lapply(c("Amsterdam", "Copenhagen", "Rotterdam"), ptsToRoads, in_dir=in_dir, n_cores=detectCores(), 
#        out_dir=out_dir, core_no=10)

### output:
## [index for core_no]_subv3[index for n_cores]_[city].gpkg

############ combined the snapped data as a whole (raw data in road segment) ############
in_dir <- 'data/temp/snapped/'
out_dir <- "data/processed/"
fnames <- list.files(in_dir, 'subv3')
fnames <- fnames[grepl('.gpkg', fnames)]
sub_i = strsplit(fnames, "_") %>% lapply(., length) %>% unlist
sub_i=(sub_i==3)
cityNames <- strsplit(fnames, "_")[sub_i] %>% 
  lapply(., `[[`, 3) %>% 
  unlist %>% 
  unique %>% 
  gsub('.gpkg', '', .)

source("src/00_process/fun_03_ptsToRoads_combined.R") # 01_ptsToRoads_combined.R
# lapply(cityNames, ptsToRoads_combined, 
#        in_dir=in_dir, out_dir=out_dir)
if(length(cityNames)>detectCores()){
  mc_cores = detectCores()
}else{
  mc_cores = length(cityNames)
}
mclapply(cityNames, ptsToRoads_combined, 
         in_dir=in_dir, out_dir=out_dir, mc.cores = mc_cores)


### output: [city]_rawPtsLines_v3.gpkg

############ process the raw data into averages ############
# 02_vis_ptsLines.R
in_dir <- "data/processed/"
out_dir <- 'data/processed/'
fnames <- list.files(in_dir, '_rawPtsLines_v3.gpkg')
fnames <- fnames[grepl('.gpkg', fnames)]
cityNames <- strsplit(fnames, "_") %>% 
  lapply(., `[[`, 1) %>% 
  unlist


##### function
source("src/00_process/fun_04_prodStat.R")
mclapply(cityNames, prod_stat, mc.cores = length(cityNames), outDir=out_dir, inDir=in_dir)
# coefficient of variation (https://en.wikipedia.org/wiki/Coefficient_of_variation)
rawptlinesv3%>%filter(UID==161542, day==5, month==12, year==2022, w_day==1, weekend==0)%>%summarise(sd=sd(UFP, na.rm=T), mean=mean(UFP, na.rm=T), min=min(UFP, na.rm=T), max=max(UFP, na.rm=T), q25=quantile(UFP, probs=0.25, na.rm=T), q50=quantile(UFP, probs=0.5, na.rm=T), q75=quantile(UFP, probs=0.75, na.rm=T))
rawptlinesv3%>%filter(UID==1, day==8, month==12, year==2022, w_day==4, weekend==0)%>%summarise(sd=sd(UFP, na.rm=T), mean=mean(UFP, na.rm=T), min=min(UFP, na.rm=T), max=max(UFP, na.rm=T), q25=quantile(UFP, probs=0.25, na.rm=T), q50=quantile(UFP, probs=0.5, na.rm=T), q75=quantile(UFP, probs=0.75, na.rm=T))
# UFPstat2_v3.shp

######
in_dir <- "data/processed/"
out_dir <- 'data/processed/final_mobileData/'
if(!dir.exists(out_dir)) dir.create(out_dir)


fnames <- list.files(in_dir, 'stat_v3.rds')
cityNames <- strsplit(fnames, "_") %>% 
  lapply(., `[[`, 1) %>% 
  unlist
cityNames <- gsub("stat", "", cityNames)
cityNames <- cityNames[nchar(cityNames)!=0]


### averages over averages per day
source("src/00_process/fun_04_prodStat.R")
mclapply(cityNames, prod_statOfStat, inDir=in_dir, outDir=out_dir, mc.cores = length(cityNames))


#######
in_dir <- "data/processed/final_mobileData/"

fnames <- list.files(in_dir, '_stat2_v3.rds')
cityNames <- strsplit(fnames, "_") %>% 
  lapply(., `[[`, 1) %>% 
  unlist
cityNames <- gsub("stat", "", cityNames)
cityNames <- cityNames[nchar(cityNames)!=0]
over_stats <- lapply(cityNames, function(cityName){
  
  stat2 <- readRDS(paste0(in_dir, cityName,'_stat2_v3.rds'))
  data.frame(city=cityName,
             ufp_n = nrow(stat2[!is.na(stat2$UFPmean),]),
             bc1_n = nrow(stat2[!is.na(stat2$BC1mean),]),
             bc6_n = nrow(stat2[!is.na(stat2$BC6mean),]),
             summary_drivingDays = paste0(round(summary(stat2$driving_days), 1), collapse = "; "),
             ufp_mean = mean(stat2$UFPmean, na.rm=T),
             bc1_mean = mean(stat2$BC1mean, na.rm=T),
             bc6_mean = mean(stat2$BC6mean, na.rm=T)
             
  )
}) %>% do.call(rbind,.)
write.csv(over_stats, "results/output/stat_processedMobileData.csv",row.names = F)

#######
# how variability would influence the difference between mean of mean and median of median
stat2 <- readRDS(paste0(in_dir, cityName,'_stat2_v3.rds'))
ggplot(stat2[!is.na(stat2$UFPsd),])+
  geom_point(aes(x=UFPmedMedian, y=UFPmean, color=UFPsd))+
  geom_abline(slope=1, intercept=0, color='red')
ggplot(stat2[!is.na(stat2$UFPsd),] %>% mutate(UFPsdsum=UFPsd+UFPmsd))+
  geom_point(aes(x=UFPmean, y=UFPsdsum))+
  geom_abline(slope=1, intercept=0, color='red')

ggplot(stat2[!is.na(stat2$UFPsd),] %>% mutate(UFPsdsum=UFPsd+UFPmsd) %>% filter(UFPsdsum<26518))+#Median
  geom_point(aes(x=UFPmedMedian, y=UFPmean, color=UFPsdsum))+
  geom_abline(slope=1, intercept=0, color='red')
ggplot(stat2[!is.na(stat2$UFPsd),] %>% mutate(UFPsdsum=UFPsd+UFPmsd) %>% filter(UFPsdsum>51997))+#3rd Qu.
  geom_point(aes(x=UFPmedMedian, y=UFPmean, color=UFPsdsum))+
  geom_abline(slope=1, intercept=0, color='red')

ggplot(stat2)+
  geom_point(aes(x=UFPmsd, y=UFPsd))+ 
  geom_abline(slope=1, intercept=0, color='red')

# UFPmsd: average intra-passes variability 
# UFPsd: intra-day variability

######## driving days vs passes

library(viridis)

plots2 <- lapply(cityNames, function(cityName){
  stat2 <- readRDS(paste0(in_dir, cityName,'_stat2_v3.rds'))
  
  tbl <- table(stat2$passes, stat2$driving_days)
  tbl <- as.data.frame(tbl) %>% 
    rename(passes = Var1, driving_days=Var2, n=Freq) %>% 
    mutate(driving_days=as.numeric(driving_days), 
           passes=as.numeric(passes), 
           n = as.numeric(n))
  
  tbl <- tbl %>% mutate(n=ifelse(n==0, NA, n))
  
  stat_tbl <- stat2 %>% 
    as.data.frame %>% 
    dplyr::select(UFPmean, passes, driving_days) %>% 
    group_by(passes, driving_days) %>% 
    summarise(UFPmedian=median(UFPmean, na.rm=T),
              UFPsd=sd(UFPmean, na.rm=T),
              UFPmean=mean(UFPmean, na.rm=T)
    )
  
  stat_tbl2 <- inner_join(stat_tbl, tbl)
  p1 <- ggplot(stat_tbl2)+
    geom_tile(aes(x=driving_days, y=passes, fill=UFPmedian))+
    scale_fill_stepsn(colours = viridis(option="plasma", n=15))+
    labs(title=cityName)
  p2 <- ggplot(stat_tbl2)+
    geom_tile(aes(x=driving_days, y=passes, fill=UFPsd))+
    scale_fill_stepsn(colours = viridis(option="plasma", n=15))+
    labs(title=cityName)
  p3 <- ggplot(stat_tbl2)+
    geom_tile(aes(x=driving_days, y=passes, fill=UFPmean))+
    scale_fill_stepsn(colours = viridis(option="plasma", n=15))+
    labs(title=cityName)
  p4 <- ggplot(tbl)+
    geom_tile(aes(x=driving_days, y=passes, fill=n))+
    scale_fill_stepsn(colours = viridis(option="plasma", n=15),
                      breaks=c(0, 50, 100, 500, 1000, 2000))+
    
    labs(title=cityName)
  list(p1, p2, p3, p4)
}) 

do.call(grid.arrange, plots2 %>% unlist)

