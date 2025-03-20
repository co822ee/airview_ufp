

###### functions #####
standardize_coef <- function(pred_df, selectedPred_vars){ ## df with predictor variables' values
  q99 <- pred_df %>% ungroup %>% dplyr::select(selectedPred_vars)  %>%  sapply(., stats::quantile, probs=0.99, na.rm = T)
  q1 <- pred_df %>% ungroup %>% dplyr::select(selectedPred_vars) %>% sapply(., quantile, probs=0.01, na.rm = T)
  dfout <- q99-q1
  names(dfout) <- gsub('.99%', '', names(dfout))
  dfout <- data.frame(dfout)
  dfout$variables <- rownames(dfout)
  # dfout2 <- dfout[dfout$variables%in%neg_var,] %>% 
  #   mutate_at(., c('dfout'), function(vect) vect*(-1))
  # rbind(dfout[!(dfout$variables%in%neg_var),],
  #       dfout2)
  dfout
}
normalize_coef <- function(lm_model=localModel_df, df_pred=train_df, y_var="UFPmean"){
  # df_pred should be the pooled data, and lm_model can be either pooled model or city-specific model
  diff99_01 <- standardize_coef(df_pred, lm_model$variables[-1])
  norm_coef <- inner_join(lm_model, diff99_01, by=c( 'variables'))
  norm_coef <- norm_coef %>% mutate(beta_norm=beta*dfout)
  # norm_intercept <- lm_model[1,]
  # ufp_df <- quantile(df_pred[,y_var], probs=0.99, na.rm=T) - quantile(df_pred[,y_var], probs=0.01, na.rm=T)
  # norm_intercept <- norm_intercept %>% mutate(beta_norm=beta*ufp_df)
  if(any(grepl("nat_|ugr_|precip|wind|alt10_enh", norm_coef$variables))){
    norm_coef[grepl("nat_|ugr_|precip|wind|alt10_enh", norm_coef$variables), ]$beta <- norm_coef[grepl("nat_|ugr_|precip|wind|alt10_enh", norm_coef$variables), ]$beta*-1
    norm_coef[grepl("nat_|ugr_|precip|wind|alt10_enh", norm_coef$variables), ]$beta_norm <- norm_coef[grepl("nat_|ugr_|precip|wind|alt10_enh", norm_coef$variables), ]$beta_norm*-1
  }
  norm_coef
}