read_deconv <- function(city_sub, df_original=df_all2) {
  # Read deconvoluted data
  out_rollmin <- paste0("data/processed/deconv/", city_sub, ".Rdata")
  roll_df <- readRDS(out_rollmin)
  # Determine the time window
  cor_csvname <- paste0("results/output/deconv_cor_", city_sub, '.csv')
  cor_df <- read.csv(cor_csvname)
  # Determine the optimal roll_window for "UFP" based on traffic data correlation comparison
  ufp_cor_data <- cor_df %>% filter(poll == "UFP")
  aadt_1km_cor <- ufp_cor_data %>% filter(traf == "aadt_1km") %>% pull(cor)
  aadt_50m_cor <- ufp_cor_data %>% filter(traf == "aadt_50m") %>% pull(cor)
  
  if (any(aadt_1km_cor > aadt_50m_cor)) {
    optimal_roll_window <- ufp_cor_data$roll_window[aadt_1km_cor > aadt_50m_cor] %>% 
      min()
  } else{
    optimal_roll_window <- ufp_cor_data$roll_window[which.min(aadt_50m_cor - aadt_1km_cor)] 
  }
  
  # Log the determined optimal roll_window
  cat("Optimal roll_window for UFP based on traffic data correlation comparison: ", optimal_roll_window, ", city: ", city_sub, "\n")

  # Calculate the mean of means using the optimal_roll_window
  target_window <- paste0("UFP_min_", optimal_roll_window)
  means <- roll_df %>% 
    mutate(city=city_sub) %>% 
    dplyr::select(all_of(c("UID", "year", "month", "day", "city", 
                          target_window))) %>% 
    group_by(UID, year, month, day, city) %>% 
    summarise_at(target_window, mean, na.rm=T)

  meanOfmeans <- means %>% 
    group_by(city, UID) %>% 
    summarise_at(target_window, mean, na.rm=T) %>% 
    ungroup %>% 
    as.data.frame()

  # Filter data for the specific city
  city_data <- df_original %>% filter(city == city_sub)

  deconv_df <- inner_join(meanOfmeans, city_data, by=c("UID", "city"))
  # Compute the difference between deconvoluted background and mobile data
  deconv_df$UFP_diff <- deconv_df$UFPmean - deconv_df[[target_window]]

  # If the differences in concentrations are below zero, set the background concentration as zero
  # negative_diff_indices <- which(deconv_df$UFP_diff < 0)
  # deconv_df[[target_window]][negative_diff_indices] <- deconv_df$UFPmean[negative_diff_indices]
  # deconv_df$UFP_diff[negative_diff_indices] <- 0
  deconv_df <- deconv_df %>% rename(UFPdeconv = target_window)
  deconv_df
}

read_deconv_define_movingWindow <- function(city_sub, df_original=df_all2, optimal_roll_window=400) {
  # Read deconvoluted data
  out_rollmin <- paste0("data/processed/deconv/", city_sub, ".Rdata")
  roll_df <- readRDS(out_rollmin)
  
  # Calculate the mean of means using the optimal_roll_window
  target_window <- paste0("UFP_min_", optimal_roll_window)
  means <- roll_df %>% 
    mutate(city=city_sub) %>% 
    dplyr::select(all_of(c("UID", "year", "month", "day", "city", 
                          target_window))) %>% 
    group_by(UID, year, month, day, city) %>% 
    summarise_at(target_window, mean, na.rm=T)

  meanOfmeans <- means %>% 
    group_by(city, UID) %>% 
    summarise_at(target_window, mean, na.rm=T) %>% 
    ungroup %>% 
    as.data.frame()

  # Filter data for the specific city
  city_data <- df_original %>% filter(city == city_sub)

  deconv_df <- inner_join(meanOfmeans, city_data, by=c("UID", "city"))
  # Compute the difference between deconvoluted background and mobile data
  deconv_df$UFP_diff <- deconv_df$UFPmean - deconv_df[[target_window]]

  # If the differences in concentrations are below zero, set the background concentration as zero
  # negative_diff_indices <- which(deconv_df$UFP_diff < 0)
  # deconv_df[[target_window]][negative_diff_indices] <- deconv_df$UFPmean[negative_diff_indices]
  # deconv_df$UFP_diff[negative_diff_indices] <- 0
  deconv_df <- deconv_df %>% rename(UFPdeconv = target_window)
  deconv_df
}