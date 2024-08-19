
#generate_coef_matrix(mixed_logistic)
generate_coef_matrix = function(object=NULL, preds=NULL, numbers=NULL, factors, d) {

    ## shortcut for testing
  if (is.null(preds) | is.null(numbers)) {
    d = extract_data_from_fitted_object(object)
    terms = remove_interaction_terms(object)
    factor_or_number = which_terms_are_factors_or_numbers(d, terms)
    numbers = factor_or_number$numbers
    factors = factor_or_number$factors
    preds = output_glm_predictions(object, terms)
  }
  
  coef.matrix = output_coef_matrix_glm(object, preds, numbers) %>%
    sd_difference_numeric(numbers=numbers, preds=preds) %>%
    sd_difference_factors(factors=factors, preds=preds, d=d)
  
  return(coef.matrix)

  
}

# sd_difference_numeric(object = mixed_logistic)
# sd_difference_numeric(object = glmer(y_binary~a + b + (1|id), data=small_mixed, family="binomial"))
sd_difference_numeric = function(coef.matrix, object = NULL, numbers, preds) {
  
  ## shortcut for testing
  if (!is.null(object)) {
    d = extract_data_from_fitted_object(object)
    terms = remove_interaction_terms(object)
    factor_or_number = which_terms_are_factors_or_numbers(d, terms)
    numbers = factor_or_number$numbers
    factors = factor_or_number$factors
    preds = output_glm_predictions(object, terms)
    coef.matrix = output_coef_matrix_glm(object, preds, numbers)
  }
  
  if (length(numbers)<1 | is.na(preds)[1]) return(coef.matrix)
  
  # input +/- 1 SD prediction for all numeric variables
  predicted_difference_of_one_SD = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
  coef.matrix[numbers, "Prediction Difference (+/- 1 SD)"] = predicted_difference_of_one_SD
  return(coef.matrix)
}

# sd_difference_factors(object = glmer(y_binary~a + b + (1|id), data=small_mixed, family="binomial"))
#sd_difference_factors(object = mixed_logistic)
sd_difference_factors = function(coef.matrix, object = NULL, factors, preds, d) {
  
  # factros include the random effect!! Why?
  ## shortcut for testing
  if (!is.null(object)) {
    d = extract_data_from_fitted_object(object)
    terms = remove_interaction_terms(object)
    factor_or_number = which_terms_are_factors_or_numbers(d, terms)
    numbers = factor_or_number$numbers
    factors = factor_or_number$factors
    preds = output_glm_predictions(object, terms)
    coef.matrix = output_coef_matrix_glm(object, preds, numbers)
  }
  
  if (length(factors) == 0) return(coef.matrix)
  
  #referent_info = find_referent_group(object)
  
  referent_info = 1:length(factors) %>% purrr::map(find_referent_group, object=object,
                                            factors = factors, d=d, coef.matrix=coef.matrix,
                                            preds = preds)
  
  
  # give the referent group raw prediction if there's only one factor
  for (i in 1:length(referent_info)){
    non_referent_groups = referent_info[[i]]$non_referent_groups
    referent_group = referent_info[[i]]$referent_group
    levs = referent_info[[i]]$levs
    current_factor_predictions = referent_info[[i]]$current_factor_predictions
    
    #compute differences between referent group and other levels
    predicted_differences = round_string(unlist(current_factor_predictions)[which(non_referent_groups)] - 
                                           unlist(current_factor_predictions)[which(referent_group)], digits=2)
    labeled_predicted_differences = paste0(predicted_differences, " (relative to ", levs[referent_group], ")")
    
    # fill in for non-referent groups
    coef.matrix[levs[non_referent_groups], "Prediction Difference (+/- 1 SD)"] = labeled_predicted_differences
  }
  
  if (length(factors)==1){
    current_factor_predictions = unlist(preds[factors[1]])
    referent_group_prediction = round_string(unlist(current_factor_predictions)[referent_group], digits=2)
    referent_group_label = levs[referent_group]
    coef.matrix[1,"Prediction Difference (+/- 1 SD)"] = paste0(referent_group_prediction, " (", referent_group_label, " prediction)")
  }
  

  
  return(coef.matrix)  
  
  
}

#find_referent_group(i=1, mixed_logistic)
find_referent_group = function(i=1,object = NULL, factors, d, coef.matrix, preds) {
  
  # for testing purposes
  if (!is.null(object)) {
    d = extract_data_from_fitted_object(object)
    terms = remove_interaction_terms(object)
    factor_or_number = which_terms_are_factors_or_numbers(d, terms)
    numbers = factor_or_number$numbers
    factors = factor_or_number$factors
    preds = output_glm_predictions(object, terms)
    coef.matrix = output_coef_matrix_glm(object, preds, numbers)
  }
  
  current_factor_predictions = unlist(preds[factors[i]])
  levs = unique(d[,factors[i]]); levs = paste0(factors[i], levs)
  
  # find that level in the coef.matrix
  non_referent_groups = (levs %in% row.names(coef.matrix)); 
  referent_group = !non_referent_groups
  
  # return list of stuff
  data.frame(non_referent_groups=non_referent_groups, 
       referent_group=referent_group, 
       current_factor_predictions=current_factor_predictions, 
       levs=levs)
  
}