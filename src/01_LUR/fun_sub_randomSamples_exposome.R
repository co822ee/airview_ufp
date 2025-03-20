sub_randomSamples_exposome <- function(seed=123, ptsAll,
                              num_polygons_to_select){
  # Randomly shuffle the rows of the dataset
  set.seed(seed)
  shuffled_pts_sub <- ptsAll[sample(nrow(ptsAll)), ]
  
  ptsAll_sub <- shuffled_pts_sub[1:num_polygons_to_select, ]
  ptsAll_sub
  
}
