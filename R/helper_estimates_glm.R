compute_factor_differences = function(preds=NULL, factors=NULL, coef.matrix=NULL, d=NULL, object=NULL){

  if (is.null(preds)) {
    d = extract_data_from_fitted_object(object)
    terms = remove_interaction_terms(object)
    preds = output_glm_predictions(object, terms)
    factor_or_number = which_terms_are_factors_or_numbers(d, terms)
    numbers = factor_or_number$numbers
    factors = factor_or_number$factors
    coef.matrix = find_coef_matrix(object)
  }
  
  if (!("Prediction Difference (+/- 1 SD)" %in% names(coef.matrix))) {
    coef.matrix = coef.matrix %>%
      data.frame %>%
      mutate(`Prediction Difference (+/- 1 SD)` = NA)
  }
  
  if (!is.na(preds)[1] & length(numbers)>0){
    # input +/- 1 SD prediction for all numeric variables
    predicted_difference_of_one_SD = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
    coef.matrix[row.names(coef.matrix)%in%numbers,"Prediction Difference (+/- 1 SD)"] = predicted_difference_of_one_SD
    
    coef.matrix = round_coefficient_matrix(coef.matrix)
  }
  
  if (length(factors)==0) return(coef.matrix)
  
  # loop through all factors and input their predictions
  for (i in 1:length(factors)){
    
    current_factor_predictions = unlist(preds[factors[i]])
    levs = unique(d[,factors[i]]); levs = paste0(factors[i], levs)
    
    # find that level in the coef.matrix
    non_referent_groups = (levs %in% row.names(coef.matrix)); 
    referent_group = !non_referent_groups
    
    #if (length(which(non_referent_groups))>0) {
    
    #compute differences between referent group and other levels
    predicted_differences = round_string(unlist(current_factor_predictions)[which(non_referent_groups)] - 
                                           unlist(current_factor_predictions)[which(referent_group)], digits=2)
    labeled_predicted_differences = paste0(predicted_differences, " (relative to ", levs[referent_group], ")")
    
    # fill in for non-referent groups
    coef.matrix[levs[non_referent_groups], "Prediction Difference (+/- 1 SD)"] = labeled_predicted_differences
    #}
  }
  
  # give the referent group raw prediction if there's only one factor
  if (length(factors)==1){
    referent_group_prediction = round_string(unlist(current_factor_predictions)[referent_group], digits=2)
    referent_group_label = levs[referent_group]
    coef.matrix[1,"Prediction Difference (+/- 1 SD)"] = paste0(referent_group_prediction, " (", referent_group_label, " prediction)")
  }
  
  return(coef.matrix)
}

find_coef_matrix = function(object) {
  if (class(object)[1] == "zeroinfl") return(output_coef_matrix_zeroinf(object))
  if ("glm" %in% class(object)) return(output_coef_matrix_glm(object))
}

output_coef_matrix_zeroinf = function(object, return.others=FALSE) {
  
  #### get dataset
  d = object$model
  
  #### generate list of coefficients
  terms = attr(terms(object), "term.labels")
  
  #### identify factors
  if (length(terms)>1){
    factors = names(which(unlist(lapply(d[,terms], is.factor))));
    numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
  } else {
    factors = terms[which(is.factor(d[,terms]))]
    numbers = terms[which(is.numeric(d[,terms]))]
  }	
  #### output predictions
  n.func = function(term){anchor.predictions(object, term, shutup=T)$prediction}
  preds = lapply(terms, n.func); names(preds) = terms
  
  #### output coefficients
  coefficients_zi = coef(object)
  zeroinf_rows = grep("zero_", names(coefficients_zi), value=F)
  non_zero_inf_rows = which(!(1:length(coefficients_zi)%in%zeroinf_rows))
  coef.matrix = data.frame(A = coefficients_zi[non_zero_inf_rows], 
                           B = coefficients_zi[zeroinf_rows]) 
  names(coef.matrix) = c(object$dist, object$link)
  row.names(coef.matrix) = subsetString(row.names(coef.matrix), "_", 2)
  if (length(numbers)>0) {
    coef.matrix[numbers,"Prediction Difference (+/- 1 SD)"] = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
  }
  if (return.others) return(list(preds=preds, factors=factors, coef.matrix=coef.matrix))
  coef.matrix
}

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