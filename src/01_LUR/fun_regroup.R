regroup_spatial_indicators <- function(df_allAll){
  df_allAll <- lapply(unique(df_allAll$city), function(city_sub){
    city_sub <- as.character(city_sub)
    pts <- st_read(paste0("data/temp/road_centroidPts/ptsUID_", city_sub, ".gpkg"))
    df_allAll %>% 
      filter(city==city_sub) %>% 
      inner_join(cbind(pts %>% as.data.frame() %>% dplyr::select(-geom), pts %>% st_coordinates()), .)
  }) %>% do.call(rbind, .) %>% mutate(XY=X*Y)
  
  df_allAll <- df_allAll %>%
    mutate(city=as.character(city)) %>% 
    mutate(region = case_when(
      city %in% c("Amsterdam", "Rotterdam", "Copenhagen") ~ "north",
      city == "Lodz" ~ "east",
      city %in% c("Basel", "Munich") ~ "central",
      city %in% c("Athens", "Barcelona", "Rome") ~ "south",
      TRUE ~ NA_character_
    ))%>% mutate(region=factor(region), city=factor(city))
  ## regrouping (start)
  df_allAll <- df_allAll %>% 
    mutate(lcz_group = case_when(
      lcz %in% 1:3 ~ "Compact built-ups",
      lcz %in% 4:6 ~ "Open built-ups",
      lcz %in% 7:9 ~ "Built-ups",
      lcz %in% 11:17 ~ "Others",
      TRUE ~ NA_character_
    )) %>% mutate(lcz_group=factor(lcz_group))
  clim_tbl <- read.csv("data/raw/climate_code.csv")
  if(!("climate_zone"%in%names(df_allAll))){
    df_allAll <- inner_join(clim_tbl %>% mutate(climate_zone=factor(climate_zone),
                                                zoneID=factor(zoneID)), 
                            df_allAll, by="zoneID")
    # Force basel to be Continental
  }
  df_allAll <- df_allAll %>% mutate(climate_zone=as.character(climate_zone),
                                climate_zone=ifelse(city=="Basel", "Continental", climate_zone)) %>% 
    mutate(climate_zone=factor(climate_zone))
  df_allAll
}
