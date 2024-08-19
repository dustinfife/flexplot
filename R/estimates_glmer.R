#' Report glmerMod object Estimates (effect sizes and parameters)
#'
#' Report glmerMod object Estimates
#' @param object a glmerMod object
#' @param mc Should model comparisons be performed? Currently not used
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.glmerMod = function(object, mc=FALSE){

  #### generate list of coefficients
  terms = remove_interaction_terms(object)
  
  #### get dataset
  d = extract_data_from_fitted_object(object)
  
  factor_or_number = which_terms_are_factors_or_numbers(d, terms)
  numbers = factor_or_number$numbers
  factors = factor_or_number$factors
  preds = output_glm_predictions(object, terms)
  
  #### output coefficients
  coef.matrix = generate_coef_matrix(object, preds, numbers, factors, d=d)
  return(coef.matrix)
}