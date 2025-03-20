prep_deconv_variables <- function(allPredVars=allPred_vars, bufNo = 1000,
                                  background_varNoBuf=c("anhb", "CTMufp", "lcz", "pop2020", "precip", "pressure", "temp", "wind", "zoneID", "alt10_enh", "AADT_onRoad"),
                                  local_varNoBuf=c("roadtype")){
  source("src/01_LUR/fun_find_buf_varnames.R")
  # Prepare different variables for different models in the deconvolution methods
  ### 
  ### variables with buffer 
  varnames_prefix <- allPredVars[grepl("_\\d", allPredVars)] %>% 
    strsplit(., "_") %>% 
    lapply(., `[[`, 1) %>% 
    unique %>% unlist

  #### variables without buffer
  varnames_nobuf_prefix <- allPredVars[!grepl("_\\d", allPredVars)]

  
  allPredVars_background <- varnames_prefix %>% 
      paste0(., "_") %>% 
      lapply(find_buf_varnames, var_names=allPredVars, buf_no=bufNo, comp_operator=">=") %>% 
      unlist
  allPredVars_local <- varnames_prefix %>% 
      paste0(., "_") %>% 
      lapply(find_buf_varnames, var_names=allPredVars, buf_no=bufNo, comp_operator="<") %>% 
      unlist
  allPredVars_background <- c(allPredVars_background, background_varNoBuf)
  allPredVars_local <- c(allPredVars_local, local_varNoBuf)
  list(allPredVars_background=allPredVars_background, allPredVars_local=allPredVars_local)
}