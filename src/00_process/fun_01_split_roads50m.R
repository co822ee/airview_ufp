split_roads50m <- function(fname, in_dir, out_dir='data/temp/', src_dir='src/'){
  cityName <- gsub('OSMroads_', '', fname) %>% 
    gsub('.shp','',.)
  out_fname <- paste0(out_dir, 'OSMroads_', cityName,'_seg50m_v2.gpkg')
  
  if(!file.exists(out_fname)){
    roads <- st_read(paste0(in_dir, fname))
    roads_t <- st_transform(roads, crs=st_crs(3035))
    roads_t$len <- st_length(roads_t)
    source(paste0(src_dir, 'fun_split_lines.R'))
    split <- split_lines(roads_t, 60, 'osm_id')
    #--------------- waiting the result to be done in VM (eu ufp)
    split$len <- st_length(split$geometry) %>% 
      as.numeric()
    
    # former function leaves out street pieces smaller than 60 meters, here, they are added to the dataset again
    #calculate length of all street pieces and subset for those <60m
    roads_t$len <- 
      st_length(roads_t$geometry) %>% 
      as.numeric ##st_length(roads_t)
    small <- subset(roads_t, roads_t$len<60)
    # really that some road segments were left out?
    any(!(small$osm_id %in% split$osm_id)) #TRUE
    #only keep if not already in split
    small <- subset(small, !(small$osm_id %in% split$osm_id))
    #bind them together
    roads_t_seg2 <- inner_join(split, 
                               roads_t %>% 
                                 as.data.frame %>% 
                                 dplyr::select(-'len', -'geometry'), by='osm_id')
    
    final <- rbind(roads_t_seg2 %>% dplyr::select(-'split_fID'), 
                   small)
    # Are all original roads included in the dataset?
    all(final$osm_id%in%roads_t$osm_id)#yes
    #Give each segment a new ID
    final$UID<- c(1:nrow(final))
    final$osm_id <- NULL
    st_write(final, out_fname)
  }
  
}

