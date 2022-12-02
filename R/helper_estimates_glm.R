output_glm_predictions = function(object, terms) {
  #### output predictions

  if (class(object)[1]=="glmerMod") return(NA)
  preds = lapply(terms, n.func, object=object); names(preds) = terms
  return(preds)
}

n.func = function(term, object){
  # this funciton returns predicted values at +/- 1 SD (for numeric)
  anchor.predictions(object, term, shutup=T)$prediction
}

output_coef_matrix_glm = function(object, preds=NULL, numbers=NULL) {
  
  #### <<<<<< for testing purposes
  if (is.null(preds)) {
    terms = remove_interaction_terms(object)
    preds = output_glm_predictions(object, terms)
  }
  
  if (is.null(numbers)) {
    d = extract_data_from_fitted_object(object)
    numbers = which_terms_are_factors_or_numbers(d, terms)$numbers
  }
  #### for testing purposes >>>>>>
  
  
  if (class(object)[1] == "glmerMod" & family(object)$link == "logit"){
    coef.matrix = data.frame(raw.coefficients = lme4::fixef(object), 
                             OR               = exp(lme4::fixef(object)), 
                             inverse.OR       = 1/exp(lme4::fixef(object)) 
                            )
    return(coef.matrix)
  } 
  
  if (family(object)$link=="logit"){
    coef.matrix = data.frame(raw.coefficients        = coef(object), 
                             OR                      = exp(coef(object)), 
                             inverse.OR              = 1/exp(coef(object)), 
                             standardized.OR         = exp(standardized.beta(object, sd.y=F)), 
                             inverse.standardized.OR = 1/exp(standardized.beta(object, sd.y=F)))
    return(coef.matrix)
  } 
  
  if (family(object)$link=="log"){
    coef.matrix = 
      data.frame(raw.coefficients                = coef(object), 
                             multiplicative.coef = exp(coef(object)), 
                             std.mult.coef       = exp(standardized.beta(object, sd.y=F)))
    return(coef.matrix)
  } 
  
  if (family(object)$link=="inverse"){
    coef.matrix = 
      data.frame(raw.coefficients          = coef(object), 
                             inverse.coef  = 1/(coef(object)), 
                             std.mult.coef = 1/(standardized.beta(object, sd.y=F)))
    return(coef.matrix)
  }
  
  coef.matrix = summary(object)$coefficients
  return(coef.matrix)
  

}

#### for those that are factors, put the first prediction in the -1 SD column
round_string = function(x, digits = 2){
  return.val = ifelse(round(x, digits)==0, 
                      paste0("<0.", rep(0, times=digits-1), "1"), 
                      round(x, digits=digits))
  return.val
}

round_coefficient_matrix = function(coef.matrix) {
  # clean up the output (round things)
  nms = row.names(coef.matrix); nms2 = names(coef.matrix)
  coef.matrix = data.frame(
    lapply(coef.matrix, function(y) if(is.numeric(y)) round(y, 3) else y), 
    row.names=nms) 
  names(coef.matrix) = nms2
  return(coef.matrix)
}