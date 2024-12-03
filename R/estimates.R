#' Report Estimates (effect sizes and parameters)
#'
#' Report object Estimates
#' @param object a object
#' @param mc Should model comparisons be performed?
#' @export
estimates = function(object, mc=TRUE){
  UseMethod("estimates")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @param object a object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.default = function(object, mc=TRUE){
	out = summary(object)
	class(out) = "estimates"
	out
}

#' Report lmerMod object Estimates (effect sizes and parameters)
#'
#' Report lmerMod object Estimates
#' @param object a lm object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.lmerMod = function(object, mc=TRUE){
  fixed = lme4::fixef(object)
  rand = lme4::VarCorr(object)

  
  # fit a baseline model
  baseline = fit_baseline_model(object)
  icc_stats = unlist(icc(baseline))
  
  # compute rsq 
  rsq = model.comparison(object, baseline)$r_squared_change
  
  # return the objects
  ret = list( fixed = fixed,
              r.squared=rsq,
              rand = rand, 
              icc = icc_stats
             )
  attr(ret, "class") = "lmer_estimates"
  return(ret)
}



#' Report glmerMod object Estimates (effect sizes and parameters)
#'
#' Report glmerMod object Estimates
#' @param object a glmerMod object
#' @param mc Should model comparisons be performed? Currently not used
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.glmerMod = function(object, mc=FALSE){
  fixed = lme4::fixef(object)
  rand = lme4::VarCorr(object)
  
  
  # fit a baseline model
  baseline = fit_baseline_model(object)
  icc_stats = unlist(icc(baseline))
  
  # compute rsq 
  rsq = model.comparison(object, baseline)$r_squared_change
  
  # return the objects
  ret = list( fixed = fixed,
              r.squared=rsq,
              rand = rand, 
              icc = icc_stats
  )
  attr(ret, "class") = "lmer_estimates"
  return(ret)
}



#' Report lm object Estimates (effect sizes and parameters)
#'
#' Report lm object Estimates
#' @param object a lm object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.lm = function(object, mc=TRUE){

	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	variables = as.character(attr(terms(object), "variables")); variables = variables[-1]
	outcome = variables[1]
	predictors = variables[-1]

	# for intercept only models, return the mean
	if (length(predictors) == 0 ) {
	  f = as.formula(paste0(outcome, "~1"))
	  est = compare.fits(formula = f, data=object$model, model1=object, model2=NULL, return.preds=T, report.se=T)[1,]
	  return = est[1:3]
	  names(return) = c("Mean", "Lower", "Upper")
	  return$d = coef(object)/summary(object)$sigma
	  return(return)
	}

  #### look for interaction terms
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = object$model
	
  #### identify factors
	variable_types = which_terms_are_factors_or_numbers(d, terms)
	factors = variable_types$factors
	numbers = variable_types$numbers
	
	# compute semi-partial
  semi.p = compute_semi_partial(object)
  
  # get summaries for factors
  factor_summaries = populate_estimates_factors(object, factors)
  coef.matrix = factor_summaries$coef.matrix
  difference.matrix = factor_summaries$difference.matrix
  
  # get summaries for numeric
  coef.matrix.numb = populate_estimates_numeric(object, numbers)
	
  # compute r squared and its CI
  r.squared = compute_r_squared(object)
	
	#### report correlation
  correlation = compute_correlation(object)
  
  # model comparison (I think this is only for JASP)
  dataset<<-object$model
  mod.comps = return_model_comparisons(object, terms, mc)

	ret = list(r.squared=r.squared, semi.p=semi.p, correlation = correlation, factor.summary = coef.matrix, 
	           difference.matrix=difference.matrix, factors=factors, numbers.summary=coef.matrix.numb, numbers=numbers, 
	           sigma=summary(object)$sigma,
	           model.comparison = mod.comps)
	attr(ret, "class") = "estimates"
	return(ret)
}


#' Report RandomForest object Estimates (effect sizes and parameters)
#'
#' Report RandomForest object Estimates
#' @param object a RandomForest object
#' @param mc Should model comparisons be performed? 
#' Currently not implemented for RandomForest objects
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.RandomForest = function(object, mc=TRUE) {
  
  y = unlist(attr(object, "data")@get("response"))
  ### compute OOB
  
  oob = predict(object, OOB=T, type="response")
  if (!is.numeric(y)){
    numeric=F
    oob = round(length(which(oob==y))/length(y), digits=3)
    rsq = NA
  } else {
    ### quantile of differences
    numeric = T
    rsq = cor(oob, y)[1,1]^2
    oob = round(quantile(abs(oob-y)), digits=3)
  }
  
  #### compute variable importance
  importance = party::varimp(object)
  if (!numeric){
    importance = round(sort(importance, decreasing=T), digits=4)
  } else {
    vals = sort(importance, decreasing=T)
    vals[vals<0] = 0
    importance = round(sqrt(vals), digits=3)
  }
  
  estimates = list(oob=oob, rsq = rsq, importance=importance)
  attr(estimates, "class") = "rf_estimates"
  attr(estimates, "numeric") = numeric
  return(estimates)
  
}



#' Report glm object Estimates (effect sizes and parameters)
#'
#' Report glm object Estimates
#' @param object a glm object
#' @param mc Should model comparisons be performed? Currently not used
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.glm = function(object, mc=FALSE){

	#### generate list of coefficients
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = extract_data_from_fitted_object(object)
	
	factor_or_number = which_terms_are_factors_or_numbers(d, terms)
	numbers = factor_or_number$numbers
	factors = factor_or_number$factors
	
  preds = output_glm_predictions(object, terms)
	
	#### output coefficients
  coef.matrix = compute_factor_differences(object=object)
  

  ##attr(coef.matrix, "class") = "glm.estimates"
	return(coef.matrix)
}



#' Report zeroinfl object Estimates (effect sizes and parameters)
#'
#' Report zeroinfl object Estimates
#' @param object a zeroinfl object
#' @param mc Should model comparisons be performed? Currently not used
#' @return Estimates for a zero inflated model
#' @export
estimates.zeroinfl = function(object, mc=FALSE){
  coef.matrix = compute_factor_differences(object=object)
  
  #### generate list of coefficients
  terms = remove_interaction_terms(object)
  
  #### get dataset
  d = extract_data_from_fitted_object(object)
  
  factor_or_number = which_terms_are_factors_or_numbers(d, terms)
  numbers = factor_or_number$numbers
  factors = factor_or_number$factors
  
  if (length(factors)==0) return(coef.matrix)
  factors_equation = make.formula(names(d)[1], factors)
  preds_factors = generate_predictors(d, factors_equation, object) %>%
    .[!duplicated(.),] %>%
    `rownames<-`( NULL ) 
  preds_factors$prediction = predict(object, preds_factors)
  preds_factors$logit      = predict(object, preds_factors, type="zero")
  
  return(list(coef.matrix=coef.matrix, means = preds_factors))
}



