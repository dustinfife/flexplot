return_factors_names = function(model) {
  
  terms = remove_interaction_terms(model)
  d = extract_data_from_fitted_object(model)
  d %>% select(-where(is.numeric)) %>%  names
}

identify_method = function(data, outcome, axis, method) {
  # histograms/barcharts
  if (axis[1] == "1") return("loess")
  # association plot
  if (check.non.number(data[,axis[1]])) return("loess")
  # logistic
  if (length(unique(data[,outcome]))==2) return("logistic")
  if (!is.null(method)) return(method)  
  return("loess")
}