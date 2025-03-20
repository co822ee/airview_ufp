
find_buf_varnames <- function(var_str, var_names, buf_no=1000, comp_operator=">="){
  ind_cols <- var_names[grepl(paste0("^", var_str), var_names)]
  out_cols <- NULL
  for (ind_col in ind_cols) {
    suffix <- gsub(var_str, "", ind_col)
    num <- as.numeric(suffix)
    if (!is.na(num) && eval(parse(text = paste("num", comp_operator, buf_no)))) {
      out_cols <- c(out_cols, ind_col)
    }
  }
  out_cols
}