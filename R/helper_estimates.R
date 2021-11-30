#### average the ones that need to be averaged
return_levels_or_mean = function(x, factors=factors, d=d) { 
  
  if (x %in% factors) {
    return(unique(d[,x])[1])
  }  
  return(mean(d[,x], na.rm=T))
}

return_averages = function(model, not_included, shutup=FALSE) {
  
  if (length(not_included)==0 | is.null(not_included)) return(NULL)
  
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

return_plus_minus_one_sd = function(x) {
  return(c(mean(x, na.rm=T) + sd(x, na.rm=T), mean(x, na.rm=T)-sd(x, na.rm=T)))
}

generate_numeric_predictions = function(numeric_variables, d) {
  
  if (length(numeric_variables)> 1) return(lapply(d[,numeric_variables], return_plus_minus_one_sd))
  if (length(numeric_variables)==1 
    & length(unique(d[,numeric_variables]))>5){
    numeric.preds = list(return_plus_minus_one_sd(d[,numeric_variables]))
    names(numeric.preds)[[1]] = numeric_variables
    return(numeric.preds)
  }
  return(NULL)
}

generate_grid_predictions = function(numeric.preds, factor.levs, average.predictions) {
  
  numeric_exist = length(numeric.preds)>0
  factor_exist  = length(factor.levs)  >0
  
  if (!(numeric_exist) & !(factor_exist)) return(NULL)
  
  # make a list of all things that need to be gridded
  items_to_grid = c(numeric.preds, factor.levs, average.predictions)
  final.prediction = expand.grid(items_to_grid)
  return(final.prediction)
}
