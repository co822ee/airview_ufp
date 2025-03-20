opt_rf <- function(df_train, df_test, y_varname, x_varname, csv_name, 
                   hyper_grid, seed=123, tuneRF=F, 
                   outputselect = c("station_european_code", "rf", "obs", "res", 
                                    "nfold", "df_type", "year", "index"), extValid=F,
                    num_trees=500, m_try=floor(sqrt(length(x_varname))), vi_importance = 'permutation',
                    min_node_size=2000, outDir="data/workingData/"
                    ){
   if(tuneRF){
      mtry <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$mtry
      ntrees <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$ntrees
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = ntrees,
         mtry = mtry,
         seed = seed,
         importance = 'impurity'          # 'permutation'
      )
   }else{
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
        formula = eq,
        data = df_train,
        num.trees = num_trees,
        mtry = m_try,
        seed = seed,
        local.importance = TRUE,
        scale.permutation.importance = TRUE,
        min.node.size = min_node_size,
        write.forest = TRUE,
        importance = vi_importance          # 'permutation', 'impurity'
      )
   }
   if(!extValid){
      df_all <- rbind(df_train %>% mutate(df_type = 'train'), 
                      df_test %>% mutate(df_type = 'test'))
      rf_result <- data.frame(rf = predict(rf_model, df_all) %>% predictions(),
                              obs = df_all[, y_varname]) %>% 
         mutate(res = obs - rf) %>% 
         cbind(df_all %>% dplyr::select(-all_of(y_varname))) 
      rf_poll_train <- rf_result[rf_result$df_type=='train', ]
      rf_poll_test <- rf_result[rf_result$df_type=='test', ]
      
      eval_train <- error_matrix(rf_poll_train$obs, rf_poll_train$rf)
      eval_test <- error_matrix(rf_poll_test$obs, rf_poll_test$rf)
      
      rf_result <- rf_result[, outputselect]
      
      write.csv(rf_result, 
                paste0(outDir, 'RF_result_all_', csv_name, '.csv'), 
                row.names = F)
   }else{
      df_all <- df_test
      rf_result <- data.frame(rf = predict(rf_model, df_all) %>% predictions(),
                              obs = df_all[, y_varname]) %>% 
         mutate(res = obs - rf) %>% 
         cbind(df_all %>% dplyr::select(-all_of(y_varname))) 
      eval_test <- error_matrix(rf_result$obs, rf_result$rf)
      
      rf_result <- rf_result[, outputselect]
      
      write.csv(rf_result, 
                paste0(outDir, 'RF_result_all_', csv_name, 'Ext_valid','.csv'), 
                row.names = F)
   }
   
   
   # Variable importance
   var_importance <- data.frame(var_name = rf_model$variable.importance %>% names, 
                                vi = rf_model$variable.importance %>% as.numeric())
   var_importance <- var_importance[with(var_importance, order(-vi)), ]
   
   
   write.csv(var_importance, paste0(outDir, 'RF_vi_', csv_name, '.csv'), 
             row.names = F)
   if(!extValid){
      return(list(rf_result=rf_result, eval_train=eval_train, eval_test=eval_test, rf_model=rf_model))
   }else{
      return(list(rf_result=rf_result, eval_train=0, eval_test=eval_test, rf_model=rf_model))
   }
   
}
