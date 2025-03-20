ptsToRoads <- function(cityName, in_dir='data/temp/', n_cores=detectCores(),
                       out_dir='data/temp/snapped/', core_no=10){
  roadSeg_fname <- paste0(in_dir, 'OSMroads_', cityName,'_seg50m_v2.gpkg')
  
  # if(cityName=="Rotterdam"|cityName=="Amsterdam"|cityName=="Copenhagen"){
  #   rawpt_fname <- paste0(in_dir, 'rawWithDates_', cityName, '.Rdata')
  # }else{
  rawpt_fname <- paste0(in_dir, 'rawWithDates_', cityName, '.gpkg')
  # }
  
  if(file.exists(roadSeg_fname)&file.exists(rawpt_fname)){
    roads_t_seg2 <- st_read(roadSeg_fname) 
    
    if(cityName=="Rotterdam"|cityName=="Amsterdam"|cityName=="Copenhagen"){
      aq_data_t <- st_read(rawpt_fname)
      # aq_data_t <- readRDS(rawpt_fname)
    }else{
      aq_data <- st_read(rawpt_fname)
      aq_data_t <- st_transform(aq_data, crs=st_crs(3035))
    }
    
    
    source('src/fun_split_gpkg.R')
    split_no <- round(seq(0, nrow(aq_data_t), length.out=core_no+1))
    aq_data_t_lists <- pblapply(1:(length(split_no)-1), split_gpkg,
                                shp=aq_data_t, core_no=core_no)
    if(length(list.files(in_dir, paste0(cityName, "_rawPtsFinal_sub")))==0){
      lapply(seq_along(aq_data_t_lists), function(pts_i){
        st_write(aq_data_t_lists[[pts_i]], paste0(in_dir, cityName, '_rawPtsFinal_sub', pts_i, '.gpkg'))
      })
    }
    
    if(!dir.exists(out_dir)) dir.create(out_dir)
    
    # v3: to avoid points assigned to the wrong road segments by excluding road network 
    #     the correct method that Jules used:
    # 1) create a 30m circular buffer for the raw data points 
    # 2) dissolve the buffered 
    # 3) exclude road segments that are not completely falling into the dissolved buffer 
    # 4) snap the raw data points to the closest filtered road segments
    
    for(i in 1:core_no){ #1:cluster_no   (cluster_no+1):(cluster_no+8)  (cluster_no+9):core_no
      aq_data_t <- st_read(paste0(in_dir, cityName, '_rawPtsFinal_sub', i, '.gpkg'))
      split_no <- round(seq(0, nrow(aq_data_t), length.out=n_cores+1)) 
      
      aq_data_t_lists <- lapply(1:(length(split_no)-1), split_gpkg,
                                shp=aq_data_t, core_no=n_cores)
      
      mclapply(seq_along(aq_data_t_lists), function(ptss_i){
        ptss <- aq_data_t_lists[[ptss_i]]
        if(!file.exists(paste0(out_dir, i, '_subv3', ptss_i, "_", cityName, '.gpkg'))){
          source('src/fun_ptToNNLine3.R')
          snappedresult <- ptToNNLine3(ptss, roads_t_seg2, 30)
          st_write(snappedresult, paste0(out_dir, i, '_subv3', ptss_i, "_", cityName, '.gpkg')) 
        }
      }, mc.cores=n_cores)
    }
  }
}

