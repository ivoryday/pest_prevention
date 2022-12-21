


reshape_by_var <- function(dat, prefix){
  # all cols that are variety specific
  cols_var_spec <- dat %>% dplyr::select(c("id_all", "year"), ends_with(varieties[1]) | ends_with(varieties[2]) | ends_with(varieties[3]) | 
                                           ends_with(varieties[4]) | ends_with(varieties[5]) |
                                           ends_with(varieties[6]) | ends_with(varieties[7]) | ends_with(varieties[8]) | ends_with(varieties[9]))
  
  vardf <- cols_var_spec %>% dplyr::select(c("id_all", "year"), starts_with(prefix))
  colnames(vardf) <- c(c("id_all", "year"), varieties)
  vardf_long <- vardf %>% gather(variety, value, -c(id_all, year))
  vardf_long <- vardf_long %>% ungroup() %>% arrange(id_all,variety, year)
  colnames(vardf_long)[4] <- prefix
  # vardf_long2 <- vardf_long[is.na(vardf_long[, prefix])==F,]  ## subset in combined df
  return(vardf_long)
}




vars_compile <- function(startdf, searchdf, varvec){
  for (i in varvec) {
    longdf <- reshape_by_var(searchdf, i)
    if(i != "ds" & i != "cl" & i!="area"){
      longdf[,4][is.na(longdf[,4])] <- 0
    }
    
    startdf <- cbind(startdf, longdf[4])
  }
  return(startdf)
}





mean_impute <- function(df, varvec){
  df_impu <- df
  for(var in varvec){
    df_impu[,var][is.na(df_impu[,var])==T] <- mean(df[,var], na.rm=T)
  }
  return(df_impu)
}


