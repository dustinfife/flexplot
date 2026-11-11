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
	  est = compare_fits(formula = f, data=object$model, model1=object, model2=NULL, return.preds=T, report.se=T)[1,]
	  return = est[c("prediction.fit", "prediction.lwr", "prediction.upr")]
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
#' @param object a zeroinfl object
#' @param mc Should model comparisons be performed? Currently not used
#' @return Estimates for a RandomForest model. For classification, it will return the following:
#' \itemize{
#' \item{OOB Accuracy in prediction: The total accuracy (correct classifications/incorrect classifications) for the OOB data}
#' \item{Variable importance: The difference in accuracy between the unshuffled and shuffled scores for the OOB data. Higher values indicate higher importance}
#' }
#' For regression, it will return the following:
#' \itemize{
#' \item{Quantiles of absolute value of OOB performance: This compares the OOB predicted values to the observed values, computing the difference between, and returns the quantiles.}
#' \item{Model R Squared: The squared correlation between the OOB predicted and the observed, just like in regular regression.}
#' \item{Variable importance: This is essentially the average difference in predictions between the original versus permutated values.}
#' }
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
    sds = 
    importance_sd = importance/sd(vals)
  }
  
  estimates = list(oob=oob, rsq = rsq, importance=importance)
  attr(estimates, "class") = "rf_estimates"
  attr(estimates, "numeric") = numeric
  return(estimates)
  
}



#' Interpretable estimates for generalized linear models
#'
#' Report coefficients and effect sizes from the original fitted model. For
#' binomial logit models, printing retains Raw Estimates, IRT Estimates, and
#' Standardized Estimates, followed by Marginal Predicted Probabilities when
#' categorical predictors are present. No models are refitted.
#'
#' @param object A fitted glm object.
#' @param mc Should model comparisons be performed? Currently not used.
#' @return The existing coefficient-level glm_estimates object. For binomial logit
#'   models, additional attributes are irt (predictor-level conditional estimates),
#'   conditioning (a one-row reference profile), and marginal_probabilities
#'   (Variable, Level, Probability; NULL if no categorical predictors exist).
#'   Coefficient rows, their names, and numeric missing IRT values are retained.
#' @details
#' The following definitions apply to binomial logistic regression. The modeled
#' success is the outcome recognized by the fitted binomial model: for a two-level
#' factor it is the second level. A proportion or two-column binomial response
#' retains the original model's success coding.
#'
#' \strong{Raw Estimates.} If eta = X beta is the logit of success probability,
#' raw.coefficients contains beta. A continuous main-effect coefficient measures
#' log-odds per predictor unit, conditional on the remaining covariates. In an
#' interaction model it is the effect at zero/reference moderator settings under
#' the fitted parameterization, not necessarily at the IRT conditioning settings.
#' OR = exp(beta) is the corresponding odds multiplier; inverse.OR = exp(-beta)
#' is its reciprocal. Treatment-coded factor coefficients compare a level with
#' its reference; sum, ordered-polynomial and other contrasts describe their
#' fitted contrast, not necessarily a pairwise level comparison. Interaction
#' coefficients describe changes in these effects.
#'
#' \strong{IRT Estimates.} Only continuous predictors are printed here; factors
#' remain in Raw and Standardized Estimates. Other continuous predictors are
#' held at their estimation-row means and categorical predictors at fitted
#' reference levels. Nondefault treatment references are respected. When the
#' fitted contrasts have no reference level (for example sum or polynomial
#' contrasts), the first fitted category is used. Inspect the conditioning
#' attribute for the actual settings. These are conditional estimates at specified
#' covariate values, not average marginal effects.
#'
#' The instantaneous_slope is the derivative of predicted success probability
#' with respect to the original predictor at target probability p. Its units are
#' probability per predictor unit. The intercept_threshold is the predictor value
#' where that same conditional curve reaches p. estimates uses p = 0.5;
#' [logistic_coefficients()] exposes location to request another p, changing both
#' calculations consistently. For a conditional affine logit a + b*x, these are
#' b*p*(1-p) and (qlogis(p)-a)/b. Interactions retain all fitted terms, evaluated
#' at the documented mean/reference settings, so results may change at other
#' moderator values. Log and square-root focal transformations use the chain
#' rule and original predictor units. General nonlinear transformations, including
#' quadratic terms, return numeric NA when uniqueness cannot be established.
#' Unreachable targets and nonfinite derivatives also yield NA. A zero conditional
#' slope is reported as zero with an NA threshold because no unique crossing exists.
#'
#' A threshold outside the observed range is an extrapolated crossing, not evidence
#' by itself of a poor predictor. Baseline risk may remain below or above 0.5 over
#' the entire observed range even when a predictor has a substantial effect.
#' Always interpret the slope, observed range, uncertainty and model adequacy
#' alongside the threshold; this table does not provide uncertainty intervals.
#'
#' \strong{Standardized Estimates.} Standardization multiplies each coefficient
#' by the standard deviation of its fitted model-matrix column. The displayed OR
#' is exp(beta*SD(column)) and inverse.OR is its reciprocal. For continuous main
#' effects this is the odds multiplier for one SD; factor contrasts and interaction
#' columns use their own coded-column SD and are not generic between-level effects.
#' The intercept has column SD zero and standardized OR one by convention.
#'
#' Prediction Difference (+/- 1 SD) retains the absolute difference between
#' response-scale predictions at a continuous main predictor's mean plus and minus
#' one SD, with other predictors at the same conditional settings. This spans two
#' SD and is not an odds ratio. Supported treatment-coded factor rows instead
#' display signed probability differences from the reference level; when there is
#' one unordered factor, the intercept row also displays its reference probability.
#' Contrasts without a single reference comparison, interaction coefficient rows,
#' and transformed coefficient rows have NA prediction differences rather than
#' inventing a discrete comparison or attaching a predictor result positionally.
#'
#' \strong{Marginal Predicted Probabilities.} For each categorical predictor and
#' each fitted level, set that predictor to the level for every eligible estimation
#' observation, retain all other observed covariates, predict with the original
#' model on the response scale, and take the arithmetic mean. Equivalently,
#' probability(level) = mean_i inverse_logit(eta_i with that level substituted).
#' This accounts for interactions and contrasts without interpreting coefficient
#' names. It is observation-weighted standardization over estimation rows, not a
#' prediction at mean covariates, equal-cluster weighting, a pairwise difference,
#' or automatically a causal effect. Fitting weights do not become averaging
#' weights. Missing rows excluded during fitting do not reenter this population.
#'
#' Existing output for other GLM families/links is unchanged. For transformed
#' predictors, original estimation-row predictor values must be recoverable from
#' the stored frame or original data call. Coefficient-level values retain existing
#' display rounding; the summary attributes retain full numerical precision.
#' @examples
#' model = glm(injury ~ safety + attention + gender,
#'             data=tablesaw.injury, family=binomial)
#' result = estimates(model)
#' result
#' # Raw OR: conditional odds multiplier for one original safety unit.
#' exp(coef(model)["safety"])
#' # IRT: probability slope and 50 percent crossing with attention at its mean
#' # and gender at its fitted reference category. Check those actual values:
#' attr(result, "conditioning")
#' # The 70 percent threshold and tangent slope describe a different point:
#' logistic_coefficients(model, location=.7)
#' # Standardized OR uses one SD; prediction difference spans mean +/- one SD.
#' # Gender probabilities average over observed safety and attention instead:
#' attr(result, "marginal_probabilities")
#' interacting = glm(injury ~ safety * gender + attention,
#'                   data=tablesaw.injury, family=binomial)
#' estimates(interacting)
#' # The safety IRT values are conditional on reference gender; the marginal
#' # gender probabilities retain each observation's actual safety and attention.
#' @seealso [logistic_coefficients()]
#' @export
estimates.glm = function(object, mc=FALSE){
  if (identical(stats::family(object)$family, "binomial") &&
      identical(stats::family(object)$link, "logit")) {
    result = logistic_estimates_table(object)
    attr(result, "class") = "glm_estimates"
    return(result)
  }

	#### generate list of coefficients
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = extract_data_from_fitted_object(object) %>% data.frame
	factor_or_number = which_terms_are_factors_or_numbers(d, terms)
	numbers = factor_or_number$numbers
	factors = factor_or_number$factors
  preds = output_glm_predictions(object, terms)
	
	#### output coefficients
  coef.matrix = compute_factor_differences(object=object)
  

  attr(coef.matrix, "class") = "glm_estimates"
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

