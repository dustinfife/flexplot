return_factors_names = function(model) {
  
  terms = remove_interaction_terms(model)
  d = extract_data_from_fitted_object(model)
  d %>% select(-where(is.numeric)) %>%  names
}

#### average the ones that need to be averaged
return_levels_or_mean = function(x, factors=factors, d=d) { 
  
  if (x %in% factors) {
    return(unique(d[,x])[1])
  }  
  return(mean(d[,x], na.rm=T))
}

return_averages = function(model, not_included, shutup=FALSE) {
  
  if (length(not_included)==0 | is.null(not_included)) return(NA)
  
  # get necessary objects
  d = extract_data_from_fitted_object(model)
  factors = return_factors_names(model)
  
  if (!shutup){
    message(paste0("\nNote: You didn't specify predictions for:\n      ", 
                   paste0(not_included, collapse=","), 
                   "\nI'm going to predict the average for quantitative variables and take the first level of categorical predictors.\n\n"))
  }	  
  
  # return averages
  averages = lapply(not_included, return_levels_or_mean, factors=factors, d=d)
  return(setNames(as.list(averages), not_included))
}

return_factor_levels = function(factors, d) {
  if (length(factors)==0) return(NULL)
  
  if (length(factors) >1) return(lapply(d[, factors], unique))
  factor.levs = list(unique(d[, factors[1]]))
  names(factor.levs)[[1]] = factors[1]
  return(factor.levs)
}