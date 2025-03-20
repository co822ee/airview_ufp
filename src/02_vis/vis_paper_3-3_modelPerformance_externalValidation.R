## External validation
source("../expanse_monthly/src/fun_callLib_dataAnalysis.R")
source("src/01_LUR/fun_prepEnv.R")

#### Swiss data
load("data/raw/external_validation/ch_marloes/data_for_utrecht_2024/UFP_data_SAPALDIA3.Rdata")
summary(UFP_data_SAPALDIA3)
unique(UFP_data_SAPALDIA3$area_site)%>%length  #74
dim(UFP_data_SAPALDIA3)                   #74
# Some UFP values are missing, and thus we only select ones with valid UFP values
UFP_data_swiss <- UFP_data_SAPALDIA3 %>% filter(!is.na(PNC))
dim(UFP_data_swiss) #n=67
unique(UFP_data_swiss$area_site)%>%length  #67
table(UFP_data_swiss$area_site)
head(UFP_data_swiss)
UFP_data_swiss_sf <- st_as_sf(UFP_data_swiss, coords=c("x_coord", "y_coord"), crs=st_crs(21781))
mapviewOptions(fgb = FALSE, georaster = FALSE, native.crs=T)
mapview(UFP_data_swiss_sf, layer.name="Swiss", zcol="PNC")
# Some campaigns have NA values in specific seasons
any(UFP_data_swiss[is.na(UFP_data_swiss$PNC_win),]$area_site%in%UFP_data_swiss[is.na(UFP_data_swiss$PNC_sum),]$area_site)
any(UFP_data_swiss[is.na(UFP_data_swiss$PNC_int),]$area_site%in%UFP_data_swiss[is.na(UFP_data_swiss$PNC_sum),]$area_site)
# -> every site has valid measurements from at least two seasons

UFP_data_swiss_sf2 <- UFP_data_swiss_sf %>% 
                          st_transform(., crs=st_crs(4326)) %>% 
                          rename(UFPobs=PNC, id=area_site) %>% 
                          dplyr::select(id, type, UFPobs)
st_write(UFP_data_swiss_sf2, 'data/temp/swiss_4326.shp')
st_write(st_transform(UFP_data_swiss_sf2, st_crs(3035)), "data/processed/filtered_externalPts/ch_4326.shp")

# Sites with elevation higher than 760m
summary(UFP_data_swiss$altitude)
(UFP_data_swiss %>% filter(altitude>760))$area_site

##### GUAN 
guan  <- read.csv("data/raw/external_validation/GUAN/guan_raw.csv")
summary(guan)

# Define the string
coordinate_string <- "10°45’23”"

change_str_to_degree <- function(coordinate_string_c){
  sapply(coordinate_string_c, function(coordinate_string){
    # Extract degrees, minutes, and seconds using regular expressions
    components <- regmatches(coordinate_string, gregexpr("\\d+", coordinate_string))
    components <- as.numeric(unlist(components))

    # Calculate the decimal degree
    degrees <- components[1]
    minutes <- components[2]
    seconds <- components[3]

    # Convert to decimal degrees
    decimal_degrees <- degrees + (minutes / 60) + (seconds / 3600)
    decimal_degrees
  })%>%as.numeric
}
guan$lat <- change_str_to_degree(guan$Latitude)
guan$long <- change_str_to_degree(guan$Longitude)
guan_sf <- st_as_sf(guan, crs=st_crs(4326), coords=c("long", "lat"))
guan_sf <- guan_sf %>% rename(ID=No., sta_name=Abbreviation)
mapview(guan_sf)
st_write(guan_sf, "data/raw/external_validation/GUAN/guan_crs4326.shp")
st_write(st_transform(guan_sf, st_crs(3035)), "data/processed/filtered_externalPts/guan_crs4326.shp")
guan <- st_transform(guan_sf, st_crs(3035))
mapview(Augsburg)+mapview(guan, color='red')

mapview(guan_sf)
###### DE
de <- read.csv("data/raw/external_validation/ULTRA3_measured_adjusted_original.csv")
head(de)
dim(de)
summary(de)
de_sf <- st_as_sf(de, crs=st_crs(31468), coords=c("x", "y"))
de_sf2 <- de_sf[!is.na(de_sf$PNC_mean_unadj),]
dim(de_sf2)
mapview(de_sf)
mapview(de_sf, zcol="PNC_mean_unadj")
mapview(de_sf, layer.name="Augsburg")

df_sf_out <- st_transform(de_sf %>% select(id, PNC_mean_unadj, PNC_median_unadj), st_crs(4326))
st_write(df_sf_out, "data/raw/external_validation/ULTRA3_measured_adjusted_crs4326.shp")
st_write(st_transform(df_sf_out, st_crs(3035)), "data/processed/filtered_externalPts/de_augsburg_crs4326.shp")
Augsburg <- st_transform(df_sf_out, st_crs(3035))
##### NL
# Claire project
nl_claire <- readxl::read_excel("data/raw/external_validation/nl_CLAIRE/UFP_extval_CLAIRE.xlsx")
nl_claire$ID <- as.numeric(nl_claire$ID)
head(nl_claire)
hist(nl_claire$UFP_adj_diff)
unique(nl_claire$ID) %>% length
dim(nl_claire)
table(nl_claire$Meas_nr, nl_claire$ID)
table(nl_claire$Meas_nr)
# Find the duplicated ID (with two-time measurements)
id_duplicated <- nl_claire$ID[duplicated(nl_claire$ID)]
# Find the number of station types
nl_claire[!duplicated(nl_claire$ID),] %>% 
      group_by(Location_type) %>% 
      summarise(count=n())
# The locations with 2-time measurements
nl_claire[duplicated(nl_claire$ID),] %>% 
      group_by(Location_type) %>% 
      summarise(count=n())

nl_claire_ag <- nl_claire %>% 
              group_by(ID) %>% 
              filter(ID%in%id_duplicated) %>% 
              summarise(POINT_X=unique(POINT_X), POINT_Y=unique(POINT_Y), 
                        UFP_adj_diff=mean(UFP_adj_diff), Location_type=unique(Location_type),
                        UFP_sd=sd(UFP_adj_diff))
write.csv(nl_claire_ag, "data/temp/nl_CLAIRE_agr.csv", row.names=F)
dim(nl_claire_ag)
table(nl_claire_ag$Location_type)
# Assuming your data frame is named 'df'
ggplot(nl_claire_ag, aes(x = UFP_adj_diff)) +
  geom_histogram(position = "dodge") + # Adjust binwidth as needed
  facet_grid(Location_type~.)+
  labs(title = "Histogram of UFP_adj_diff by Location Type",
       x = "UFP_adj_diff",
       y = "Count") +
  theme_bw()

nl_claire_sf <- nl_claire %>% st_as_sf(., coords=c("POINT_X", "POINT_Y"), crs=st_crs(28992))
mapviewOptions(fgb = FALSE, georaster = FALSE, native.crs=T)
mapview(nl_claire_sf, layer.name="NL-Claire")
mapview(nl_claire_sf, zcol="UFP_adj_diff")

if(!dir.exists("data/processed/filtered_externalPts/")) dir.create("data/processed/filtered_externalPts/")
st_write(st_transform(nl_claire_sf, st_crs(3035)), "data/processed/filtered_externalPts/nl_claire_sf.shp")

# Old Amsterdam (measured during 2002 and 2004)
nl_oldAms <- readxl::read_excel("data/raw/external_validation/nl_CLAIRE/UFP_AND_GIS_amsterdm2011.xlsx")
head(nl_oldAms)
table(nl_oldAms$countn)
table(nl_oldAms$Sitetype)
nl_oldAms_exc <- nl_oldAms %>% filter(countn>3)
table(nl_oldAms_exc$countn)
table(nl_oldAms_exc$Sitetype)
write.csv(nl_oldAms_exc, "data/temp/nl_ams_filtered.csv", row.names=F)
ggplot(nl_oldAms, aes(x = countmean)) +
  geom_histogram(position = "dodge") + # Adjust binwidth as needed
  facet_grid(Sitetype~.)+
  labs(title = "Histogram of UFP_adj_diff by Location Type",
       x = "countmean",
       y = "Count") +
  theme_bw()

nl_oldAms_sf <- nl_oldAms %>% st_as_sf(., coords=c("x", "y"), crs=st_crs(28992))
mapviewOptions(fgb = FALSE, georaster = FALSE, native.crs=T)
mapview(nl_oldAms_sf, layer.name="Amsterdam (2011)")
mapview(nl_oldAms_sf, zcol="countmean")

st_write(st_transform(nl_oldAms_sf[nl_oldAms_sf$id%in%nl_oldAms_exc$id,] %>% select(id, Sitetype, countmean), st_crs(3035)), "data/processed/filtered_externalPts/nl_oldAms_sf.shp")

# EXPOsOMICS data 
nl_expo <- read.csv("/Users/6368565/Documents/github/airview_basel/data/raw/external_validation/NL_exposomics/RUN_LongTerm.csv")
head(nl_expo)
names(nl_expo)
ggplot(nl_expo, aes(x = UFP)) +
  geom_histogram(position = "dodge") + # Adjust binwidth as needed
  labs(title = "Histogram of measured UFP",
       x = "UFP",
       y = "Count") +
  theme_bw()
nl_expo_sf <- nl_expo %>% st_as_sf(., coords=c("X", "Y"), crs=st_crs(28992))
mapview(nl_expo_sf, layer.name="NL - EXPOsOMICS")

nl_oldAms_sf <- nl_oldAms_sf %>% rename(UFPobs=countmean)
nl_claire_sf <- nl_claire_sf %>% rename(UFPobs=UFP_adj_diff)
nl_expo_sf <- nl_expo_sf %>% rename(UFPobs=UFP)

st_write(nl_oldAms_sf %>% st_transform(., crs=st_crs(4326)) %>% select(id, Sitetype, UFPobs), 'data/temp/nl_oldAms_4326.shp')
st_write(nl_claire_sf %>% st_transform(., crs=st_crs(4326)) %>% select(ID, Location_type, UFPobs), 'data/temp/nl_claire_4326.shp')
st_write(nl_expo_sf %>% st_transform(., crs=st_crs(4326)) %>% select(ID, UFPobs), 'data/temp/nl_exposomics_4326.shp')
st_write(nl_expo_sf %>% st_transform(., crs=st_crs(3035)) %>% select(ID, UFPobs), 'data/processed/filtered_externalPts/nl_exposomics_4326.shp')

####### IT
obs_tbl = read.csv("data/raw/external_validation/rome/2013_2014_rome_pnc_hh.csv")
site_tbl = read.csv("data/raw/external_validation/rome/sites_rome_pnc_hh.csv")
head(obs_tbl)
head(site_tbl)
# Check the time series data from one of the measuring station
obs_tbl <- obs_tbl %>% mutate(Date=as.Date(date))
obs_tbl <- obs_tbl %>% 
  mutate(Date=as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"),
         Hour=as.integer(format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), "%H")),
         Day=as.integer(format(Date, "%d")),
         Month=as.integer(format(Date, "%m")))
head(obs_tbl)
obs_tbl_t <- obs_tbl %>% filter(site_id==1)
plot(x=obs_tbl_t$Date, obs_tbl_t$mean_pnc_h)
ggplot(obs_tbl_t, aes(x = Date, y = mean_pnc_h)) +
  geom_line() +
  facet_wrap(.~Month, scales='free_x') +
  labs(title = "Time Series of mean_pnc_h by Month",
       x = "Date",
       y = "Mean PNC H") +
  theme_bw()
table(obs_tbl$Hour)

obs_tbl_stat <- obs_tbl %>% group_by(site_id) %>% summarise(mean_pnc_h=mean(mean_pnc_h, na.rm=T)) %>% ungroup()
# aggregate
obs_tbl_xy = inner_join(as.data.frame(obs_tbl_stat), 
                        site_tbl %>% select(site_id, Longitude, Latitude), by="site_id")
obs_sf <- st_as_sf(obs_tbl_xy, coords=c("Longitude", "Latitude"), crs=st_crs(4326))
mapviewOptions(fgb = FALSE, georaster = FALSE, native.crs=T)
mapview(obs_sf, zcol='mean_pnc_h')+
mapview(obs_sf%>%filter(site_id%in%c(8,15)), layer.name="Rome", color="red") #6,27,14,22
mapview(obs_sf, layer.name="Rome")
st_write(obs_sf, "data/temp/it_rome_4326.shp")
st_write(st_transform(obs_sf%>%filter(!site_id%in%c(8,15)), st_crs(3035)), "data/processed/filtered_externalPts/it_rome_4326.shp")

##################################################################
## results from GEE
de_results <- read.csv("data/raw/gee_result/pred_UFP_de_externalPtsAPM.csv")

out_err <- function(csv_df, model_str, obs_str='obs', pred_slr=slr){
  err_df <- error_matrix(csv_df[, obs_str], csv_df[, pred_slr]) %>% 
    as.data.frame() %>% 
    mutate(.=round(., 2)) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(model=model_str)
  err_df[,c(9,8,7,1,2)] %>% mutate(r2=cor(csv_df[, obs_str], csv_df[, pred_slr])^2 %>% 
                                     round(., 2)) %>% 
    mutate(mse_r2=1-(mean((csv_df[, obs_str]-csv_df[, pred_slr])^2))/mean((csv_df[, pred_slr]-mean(csv_df[, obs_str]))^2))
}
names(de_results)
out_err(de_results, "ExtValDE-Pooled", "PNC_mn_", "pooled_SLR")
out_err(de_results, "ExtValDE-Deconv", "PNC_mn_", "deconv_SLR")

# List all files first
files <- list.files("data/raw/gee_result/", pattern = "^pred_UFP_.*", full.names = TRUE)
# Filter out files containing 'bag' or 'random'
filtered_files <- files[!grepl("bag|random|de_externalPtsAPM.csv", files)]
# Print the filtered files
print(filtered_files)
obs_names <- list("PNC_mn_", "mean_pnc_h", "UFPobs", "UFPobs", "UFPobs")
id_names <- list("id", "site_id", "ID", "ID", "id")
loc_names  <- list("Augsburg", "Rome", "NL-Claire", "NL-EXPOsOMICS", "Amsterdam")
target_names <- c( "AADT_onRoad","CTMufp", "FsfRst_500", "alt10_enh", "apor_4000","background_SLR", "deconv_SLR", "lcz", "local_SLR", "mjrR_1000", "mjrR_50", "mjrR_5000", "nat_10000", "pooled_SLR", "por_10000", "por_1500", "precip", "roadtype", "ugr_10000")
# csv_fname <- "data/raw/gee_result/pred_UFP_it_rome5m.csv"
# obs_name <- "mean_pnc_h"
# id_name <- 'site_id'
# location_name <- "Rome"

unify_df <- function(csv_name, obs_name, id_name, location_name){
  ### Combine all the data frame and separated by locations columns
  in_df <- read.csv(csv_name)
  in_df_c <- in_df %>% 
    rename(id = !!sym(id_name), UFPobs = !!sym(obs_name)) %>% 
    mutate(loc=location_name) %>% 
    dplyr::select(all_of(target_names), UFPobs, id, loc)
  # dplyr::select(-system.index, -.geo)
  in_df_c              
  
}
tbl_all <- lapply(seq_along(filtered_files), function(i){#seq_along(filtered_files)
  unify_df(filtered_files[i], obs_names[[i]], id_names[[i]], loc_names[[i]])
}) %>% do.call(rbind, .)
unique(tbl_all$loc)
head(tbl_all)
# Revise tbl_all for NL to filter out one-time measurements
claire_new = read.csv("data/temp/nl_CLAIRE_agr.csv")
gee_claire_new  <- right_join(tbl_all %>% dplyr::select(-UFPobs) %>% filter(loc=='NL-Claire'), 
           claire_new %>% dplyr::select(UFP_adj_diff, ID) %>% rename(UFPobs=UFP_adj_diff, id=ID))
gee_claire_new <- gee_claire_new[!duplicated(gee_claire_new$id),]

# Ensure both data frames have the same columns
# Check column names
common_columns <- intersect(names(gee_claire_new), names(tbl_all))

# Select only the common columns from both data frames
gee_claire_new_common <- gee_claire_new[, common_columns]
tbl_all_common <- tbl_all[, common_columns]

# Bind the data frames
combined_df <- rbind(gee_claire_new_common, tbl_all_common %>% filter(loc!='NL-Claire'))


roadtype_proportions <- combined_df %>%
  group_by(roadtype, loc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(loc) %>%
  mutate(total = sum(count), proportion = round((count / total) * 100, 1))
roadtype_proportions <- inner_join(roadtype_proportions, 
                                   data.frame(roadtype=1:7,
                                              roadtype2=factor(c("highway", 'primary', 'secondary',
                                                                 'tertiary', 'residential', 'unclassified', 'NA'),
                                                               levels=c("highway", 'primary', 'secondary',
                                                                        'tertiary', 'residential', 'unclassified', 'NA'))))

View(roadtype_proportions)
write.csv(roadtype_proportions %>% arrange(., loc, roadtype), "results/output/externalVal_tbl_roadtypes.csv", row.names=F)
# Missing road types in Rome???
rome_missing_id  <- (combined_df %>% filter(loc=='Rome', roadtype==7))$id
mapview(obs_sf %>% mutate(withMissingRoad=ifelse(site_id%in%rome_missing_id, "T", "F")), layer.name="Rome", zcol="withMissingRoad")
# Airport
target_df <- combined_df %>% filter(apor_4000>50)
out_err(target_df, "Ext", "UFPobs", "pooled_SLR")
out_err(target_df, "Ext", "UFPobs", "deconv_SLR")
cor(target_df$deconv_SLR, target_df$UFPobs)
cor(target_df$pooled_SLR, target_df$UFPobs) # negative even
plot(target_df$pooled_SLR, target_df$UFPobs)
# The location of these stations???
loc_names  <- list("Augsburg", "Rome", "NL-Claire", "NL-EXPOsOMICS", "Amsterdam")
fnames <- list("data/raw/external_validation/ULTRA3_measured_adjusted_crs4326.shp",
               "data/temp/it_rome_4326.shp",
               'data/temp/nl_claire_4326.shp',
               'data/temp/nl_exposomics_4326.shp',
               'data/temp/nl_oldAms_4326.shp')
obs_names <- list("PNC_mn_", "mean_pnc_h", "UFPobs", "UFPobs", "UFPobs")
id_names <- list("id", "site_id", "ID", "ID", "id")
pts_sf <- lapply(seq_along(loc_names), function(i){
  sf_pts <- st_read(fnames[[i]])
  id_name <- id_names[[i]]
  obs_name <- obs_names[[i]]
  location_name <- loc_names[[i]]
  
  in_df_c <- st_read(fnames[[i]]) %>% 
    rename(id = !!sym(id_name), UFPobs = !!sym(obs_name)) %>% 
    mutate(loc=location_name, id=as.numeric(id)) %>% 
    dplyr::select(UFPobs, id, loc)
  # dplyr::select(-system.index, -.geo)
  filter_df <- target_df %>% filter(loc==location_name)
  in_df_c %>% filter(id%in%filter_df$id)
})
pts_sf <- do.call(rbind, pts_sf)
dim(pts_sf)
dim(target_df)
mapview(pts_sf)

# Archive:
# lapply(unique(combined_df$loc), function(city_sub){
#   in_df = combined_df %>% filter(loc==city_sub)
#   location_name <- city_sub
#   rbind(out_err(in_df, "ExtVal-Pooled", "UFPobs", "pooled_SLR") %>% mutate(loc=location_name), 
#         out_err(in_df, "ExtVal-Deconv", "UFPobs", "deconv_SLR") %>% mutate(loc=location_name))
# }) %>% do.call(rbind, .)

# source('../expanse_roadtraffic/src/fun_scatterplot.R')
# p1 <- scatterAll(combined_df, "pooled_SLR", "UFPobs", 
#                  "Predicted UFP from pooled SLR (in particles/cm^3)", 
#                  paste0("Observed UFP from external data in all (in particles/cm^3)"), '(A) Pooled SLR', densityP=F)
# p2 <- scatterAll(combined_df, "deconv_SLR", "UFPobs", 
#                  "Predicted UFP from pooled deconv SLR (in particles/cm^3)", 
#                  paste0("Observed UFP from external data in all (in particles/cm^3)"), '(B) Pooled deconv SLR', densityP=F)
# grid.arrange(p1, p2)
# write.csv(combined_df, "results/output/externalVal_tblAll.csv", row.names=F)

# pLists <- lapply(unique(combined_df$loc), function(city_sub){
#   in_df = combined_df %>% filter(loc==city_sub)
#   location_name <- city_sub
#   p1 <- scatterAll(in_df, "pooled_SLR", "UFPobs", 
#                    "Predicted UFP from pooled SLR (in particles/cm^3)", 
#                    paste0("Observed UFP values in ", city_sub,"  \n(in particles/cm^3)"), paste0('(A) Pooled SLR - ', city_sub), densityP=F)
#   p2 <- scatterAll(in_df, "deconv_SLR", "UFPobs", 
#                    "Predicted UFP from pooled deconv SLR (in particles/cm^3)", 
#                    paste0("Observed UFP values in ", city_sub," \n(in particles/cm^3)"), paste0('(B) Pooled deconv SLR - ', city_sub), densityP=F)
#   pdf(paste0("results/figures/paper/archive/3-3_externalVald_scatter", city_sub, ".pdf"), width=5.5, height=8)
#   grid.arrange(p1, p2)
#   dev.off()
# }) #%>% do.call(unlist, .)

# combined_df2 <- inner_join(combined_df, 
#                        data.frame(roadtype=1:7,
#                                   roadtype2=factor(c("highway", 'primary', 'secondary',
#                                                      'tertiary', 'residential', 'unclassified', 'NA'),
#                                                    levels=c("highway", 'primary', 'secondary',
#                                                             'tertiary', 'residential', 'unclassified', 'NA'))))
# safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
#                              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
# shapes <- c(0:3, 6:8)  # Different shapes for each road type

# # Manually assigning colors and shapes to ensure consistency across different data frames
# color_mapping <- setNames(safe_colorblind_palette[1:7], levels(combined_df2$roadtype2))
# shape_mapping <- setNames(shapes, levels(combined_df2$roadtype2))

# p1 <- scatterAll(combined_df2, "pooled_SLR", "UFPobs", 
#                  "Predicted UFP from pooled SLR (in particles/cm^3)", 
#                  paste0("Observed UFP values in ", city_sub,"  \n(in particles/cm^3)"), paste0('(A) Pooled SLR - ', city_sub), densityP=F, 
#                  colorList=list(T, "roadtype2", "roadtype"))+
#   theme(legend.position = "none")
# p2 <- scatterAll(combined_df2, "deconv_SLR", "UFPobs", 
#                  "Predicted UFP from pooled deconv SLR (in particles/cm^3)", 
#                  paste0("Observed UFP values in ", city_sub," \n(in particles/cm^3)"), paste0('(B) Pooled deconv SLR - ', city_sub), densityP=F,
#                  colorList=list(T, "roadtype2", "roadtype"))+
#   theme(legend.position = "bottom")
# pdf(paste0("results/figures/paper/3-3_externalVald_scatterAll.pdf"), width=5.5, height=8)
# grid.arrange(p1, p2)
# dev.off()

# pLists2 <- lapply(unique(combined_df2$loc), function(city_sub){
#   in_df = combined_df2 %>% filter(loc==city_sub)
#   location_name <- city_sub
  
#   p1 <- scatterAll(in_df, "pooled_SLR", "UFPobs", 
#                    "Predicted UFP from pooled SLR (in particles/cm^3)", 
#                    paste0("Observed UFP values in ", city_sub,"  \n(in particles/cm^3)"), paste0('(A) Pooled SLR - ', city_sub), densityP=F, 
#                    colorList=list(T, "roadtype2", "roadtype"))+
#     theme(legend.position = 'none')
#   p2 <- scatterAll(in_df, "deconv_SLR", "UFPobs", 
#                    "Predicted UFP from pooled deconv SLR (in particles/cm^3)", 
#                    paste0("Observed UFP values in ", city_sub," \n(in particles/cm^3)"), paste0('(B) Pooled deconv SLR - ', city_sub), densityP=F,
#                    colorList=list(T, "roadtype2", "roadtype"))+
#     theme(legend.position = 'none')
#   pdf(paste0("results/figures/paper/3-3_externalVald_scatter", city_sub, ".pdf"), width=5.5, height=8)
#   grid.arrange(p1, p2)
#   dev.off()
# })
# # lines  <- readLines("/Users/6368565/Documents/github/airview_basel/data/raw/external_validation/ebas_smps_pm1/Ebas_240918_1401/ES0019U.20240818140000.20240918111202.smps.particle_number_size_distribution.pm1.1mo.1h.ES05L_SMPS3082_NRT.ES05L_none.lev1.5.nas", n=20)


# # Road type
# # //1: highway
# # //2: primary
# # //3: secondary
# # //4: tertiary
# # //5: residential
# # //6: unclassified
# # //7: missing
# ggplot(combined_df)+
#   geom_point(aes(x=deconv_SLR, y=UFPobs, col=roadtype))
# source('../expanse_roadtraffic/src/fun_scatterplot.R')
# p1 <- scatterAll(de_results, "pooled_SLR", "PNC_mn_", 
#                  "Predicted UFP from pooled SLR (in particles/cm^3)", 
#                  "Observed UFP from external DE data (in particles/cm^3)", '(A) Pooled SLR', densityP=F)
# p2 <- scatterAll(de_results, "deconv_SLR", "PNC_mn_", 
#                  "Predicted UFP from pooled deconv SLR (in particles/cm^3)", 
#                  "Observed UFP from external DE data (in particles/cm^3)", '(B) Pooled deconv SLR', densityP=F)
# pdf("results/figures/paper/3-3_externalValdDE_scatter.pdf", width=10, height=6.6)
# grid.arrange(p1, p2, nrow=1)
# dev.off()
# # ggsave("results/figures/paper/3-3_scatter_slr_EUrandompoints.pdf", width=5.5, height=6.6)

# blandAltman(randompts, "pooled_SLR", "deconv_SLR", "Pooled SLR", "Deconv SLR", '')
# # ggsave("results/figures/paper/3-3_blandAltman_slr_EUrandompoints.pdf", width=6.6, height=5.5)

# scatterAll(randompts, "background_SLR", "CTMufp", "Deconv background SLR model estimates", 
#            "CTM global model estimates", '', densityP=F)
