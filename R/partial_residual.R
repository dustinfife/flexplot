#model = lm(weight.loss~therapy.type + motivation, data=exercise_data)
#term = "motivation"
partial_residual = function(model, term=NULL) {

  # extract model residuals  
  res = residuals(model)
  
  # identify position of the term
  position = return_term_location(model, term)
  
  # get data
  data = extract_data_from_fitted_object(model)[,term]
  
  # get beta term
  beta = coef(model)[term]
  
  # return residuals
  return(res + beta*data)
  
}

return_term_location = function(model, term) {
  if (is.null(term)) stop("You must provide a term")
  variables = all.vars(formula(model))
  if (!(term %in% variables)) stop(paste0("The term ", term, " is not in the model"))
  return(which(variables %in% term))
}