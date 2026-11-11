#' Conditional calculus-based logistic regression estimates
#'
#' Report instantaneous probability slopes and target-probability thresholds for
#' continuous predictors in a fitted logit-link model. These are conditional
#' estimates, not average marginal effects. No model is refitted.
#'
#' @param model A fitted logit-link glm or glmerMod model. For glmerMod models,
#'   predictions exclude random effects.
#' @param location Target probability strictly between zero and one, default 0.5.
#'   Both the slope and threshold refer to this same probability.
#' @details
#' Other continuous predictors are held at their estimation-row means and factors
#' at their fitted reference levels. A treatment-contrast reference is the level
#' whose contrast row is zero, including a non-first reference. Contrasts such as
#' sum and polynomial coding have no unique reference category; their first
#' fitted level is used instead. Interactions are evaluated at these same settings
#' for both the threshold and slope. Estimates can differ at other moderator values.
#'
#' If the conditional logit is a + b*x, the threshold is
#' (qlogis(location) - a)/b and the instantaneous slope is
#' b*location*(1-location), in probability units per unit of x. The original fitted
#' model supplies the link predictions, retaining its contrasts and interactions.
#' Thresholds may lie outside the observed range and then describe extrapolation.
#'
#' Single log(x) and sqrt(x) focal transformations, including their interactions,
#' are supported when the conditional link is affine in that transformation.
#' Thresholds and derivatives are expressed in original x units using the chain
#' rule. General nonlinear or mixed transformations (including quadratic and
#' polynomial terms) return numeric NA: a unique threshold is not established.
#' Nonfinite thresholds, unreachable targets, and singular derivatives also return
#' NA. A constant conditional curve has slope zero and no unique threshold (NA).
#' Categorical predictors retain numeric NA values in this helper's output.
#' Categorical type is determined from fitted expressions: factor(x),
#' as.factor(x), ordered(x) and as.ordered(x) with a direct source column are
#' excluded even if the source column is numeric. Their conditioning levels use
#' valid original inputs corresponding to the fitted factor levels. Ambiguous
#' or unsupported categorical transformations require an explicit factor column;
#' see \code{\link{estimates.glm}} for the supported mappings and restrictions.
#'
#' @returns A tibble with one row per underlying predictor and columns variable,
#'   instantaneous_slope and intercept_threshold. Missing estimates are numeric NA.
#' @seealso \code{\link{estimates.glm}}
#' @export
#' @examples
#' logistic_model = glm(injury ~ safety * gender + attention,
#'                      data=tablesaw.injury, family=binomial)
#' logistic_coefficients(logistic_model)
#' logistic_coefficients(logistic_model, location=.7)
logistic_coefficients = function(model, location = .5) {
  if (length(location) != 1L || !is.numeric(location) || is.na(location) ||
      !is.finite(location) || location <= 0 || location >= 1) {
    stop("location must be a probability strictly between 0 and 1.", call.=FALSE)
  }
  info = gather_model_info(model)
  summaries = lapply(info$variables, compute_logistic_summary_for_variable,
                      model_info=info, location=location)
  if (!length(summaries)) {
    return(tibble::tibble(variable=character(), instantaneous_slope=double(),
                           intercept_threshold=double()))
  }
  dplyr::bind_rows(summaries)
}

gather_model_info = function(model) {
  coefs = return_logistic_coefficients(model)
  model_data = logistic_estimation_data(model)
  mm = stats::model.matrix(model)
  variables = all.vars(stats::delete.response(stats::terms(model)))
  # Grouping columns are not fixed-effect predictors in glmer models.
  if (inherits(model, "glmerMod")) {
    variables = all.vars(reformulas::nobars(stats::formula(model))[[3]])
  }
  predictors = logistic_predictor_info(model, model_data, variables)
  continuous = variables[!vapply(predictors, function(x) x$categorical, logical(1))]
  list(model=model, data=model_data, coefs=coefs, coef_names=names(coefs),
       term_names=attr(stats::terms(model), "term.labels"),
       term_labels=attr(mm, "assign"), mm=mm, variables=variables,
       predictors=predictors, continuous=continuous,
       reference=logistic_reference_data(model, model_data, variables, predictors))
}

compute_logistic_summary_for_variable = function(var, model, model_info=NULL, location=.5) {
  if (is.null(model_info)) model_info = gather_model_info(model)
  values = logistic_conditional_summary(var, model_info, location)
  tibble::tibble(variable=var, instantaneous_slope=unname(values[1]),
                 intercept_threshold=unname(values[2]))
}




match_term_names = function(name, model, model_info=NULL) {
  
  if (is.null(model_info)) model_info = gather_model_info(model)
  
  idx = match(name, colnames(model_info$mm))
  if (is.na(idx)) return("unknown")
  
  term_index = model_info$term_labels[idx]
  matched_term = model_info$term_names[term_index]
  
  if (is.na(matched_term)) return("unknown")
  return(matched_term)
}


return_logistic_coefficients = function(model) {
  # Determine if it's a glm or glmer model
  mod_class = class(model)
  link      = family(model)$link
  if (!("logit" %in% link)) stop("Model must be a logistic regression model")
  if ("glmerMod" %in% mod_class) return (lme4::fixef(model))
  if ("glm"      %in% mod_class) return (coef(model))
}


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
  d = extract_data_from_fitted_object(model) %>% data.frame
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
  if (length(numeric_variables)<1) return(NULL)
  if (length(numeric_variables)> 1) return(lapply(d[,numeric_variables], return_plus_minus_one_sd))
  if (length(numeric_variables)==1){
    numeric.preds = list(return_plus_minus_one_sd(d[[numeric_variables]]))
    names(numeric.preds)[[1]] = numeric_variables
    return(numeric.preds)
  }
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

##' Generate Predictions for a Model
##'
##' Generate Predictions for a Model
##'
##' This function is simply an easy-to-use wrapper for the predict function.
##' With some models (e.g., logistic regression), the metrics are not very intuitive.
##' The anchor.predictions model generates the actual predicted values, depending on what the user specifies.
##' @param model The fitted model object to generate predictions
##' @param reference A vector (or single value) containing the name of the variable(s) the user wishes to generate predictions for. For
##' categorical variables, the function will generate predictions for every level of the categorical variable. For numeric variables, the function
##' will generate predictions at +1 and -1 standard deviations away from the mean.
##' @param shutup The function will give a notice if you don't specify predictions for variables that are in the model. This argument tell it to shut up.
##' @return A data frame containing the predicted values (along with the values of the predictor variables used to estimate the prediction)
##' @author Dustin Fife
##' @export
##' @return A data frame containing predictions
##' @examples
##' data(exercise_data)
##' linear.model = lm(weight.loss~health + gender, data= exercise_data)
##' # generate predictions for males/females
##' anchor.predictions(linear.model, "gender")
##' # generate predictions for health (+/- 1 sd from the mean)
##' anchor.predictions(linear.model, "health")
##' # fit a logistic regression model
##' data(tablesaw.injury)
##' glm.mod = glm(injury~safety + attention + gender, data= tablesaw.injury, family=binomial)
##' anchor.predictions(glm.mod, "attention")
##' anchor.predictions(glm.mod, c("safety", "gender"))
anchor.predictions = function(model, reference, shutup=F){

  # extract dataset/terms
  d = extract_data_from_fitted_object(model) %>% data.frame
  terms = remove_interaction_terms(model)

  # figure out which terms need to be aggregated across
  included = terms[which(terms %in% reference)]
  not.included = terms[which(!(terms %in% reference))]

  # figure out which are categorical/numeric
  factors = return_factors_names(model)
  factors.included = factors[factors%in%included]
  numeric = terms[!(terms %in% factors)]; numeric.included = numeric[numeric%in%included]

  average.predictions = return_averages(model, not.included, shutup)

  # predict the ones that need to be predicted
  factor.levs = return_factor_levels(factors.included, d)

  # create function to return +/- 1 standard deviation
  numeric.preds = generate_numeric_predictions(numeric.included, d)
  
  # generate predictors for final prediction
  final.prediction = generate_grid_predictions(numeric.preds, factor.levs, average.predictions)

  # now predict
  final.prediction$prediction = predict(model, final.prediction, type="response")

  # rename predictions
  final.prediction[,numeric.included] = paste0(
    round(final.prediction[,numeric.included], digits=2),
    " (", c("+", "-"), "1 SD)", sep="")
  if (length(which(names(final.prediction)%in%not.included))>0){
    final.prediction = final.prediction[,-which(names(final.prediction)%in%not.included)]
  }

  final.prediction
}

##' Compute Bayes Factor from BICs
##'
##' Compute Bayes Factor from BICs
##'
##' Given two models, the bf.bic function calculates the approximate bayes factor from the two models' BIC values
##' @param model1 The first model to be compared
##' @param model2 The second model to be compared
##' @param invert Should the BF be inverted?
##' @return The approximate Bayes Factor of the model comparison
##' @author Dustin Fife
##' @export
bf.bic = bf_bic = function(model1, model2, invert=F){
  bf = exp((BIC(model2) - BIC(model1))/2)
  if (invert){
    1/bf
  } else {
    bf
  }
}

which_terms_are_factors_or_numbers = function(d, terms) {
  
  chars              = unlist(lapply(d[,terms, drop=F], is.character))
  chars              = names(chars)[chars]
  d[,chars]          = lapply(d[,chars, drop=F], as.factor)
  factors            = names(which(unlist(lapply(d[,terms, drop=F], is.factor))));
  numbers            = names(which(unlist(lapply(d[,terms, drop=F], is.numeric))));
  return(list(factors=factors, numbers=numbers))
}

compute_semi_partial = function(object) {
  #### compute change in r squared
  ssr = drop1(aov(object))[-1,"Sum of Sq"]
  ssr2 = aov(object)$effects
  if (length(ssr)<(nrow(anova(object))-1)){
    message("Note: I am not reporting the semi-partial R squared for the main effects because an interaction is present. 
            To obtain main effect sizes, drop the interaction from your model. \n\n")
  }
  sst = sum(anova(object)[,"Sum Sq"])
  sse = anova(object)[,"Sum Sq"]
  semi.p = (sse[1:(length(sse)-1)]/sst)
  max = nrow(anova(object))-1
  min = max-length(semi.p)+1
  nms = row.names(anova(object))[min:max]	
  names(semi.p) = nms
  return(semi.p)
}

populate_estimates_factors = function(object, factors=NULL) {
  
  if (is.null(factors)) factors = which_terms_are_factors_or_numbers(object$model, 
                                                                     attr(terms(object), "term.labels"))$factors
  if (length(factors)==0) return(list(coef.matrix=NA, difference.matrix=NA))
  
  d = object$model
  outcome = as.character(attr(terms(object), "variables"))[-1][1]
  
  #### generate table with names
  factor.names = unlist(lapply(d[,factors, drop=F], unique))
  num.rows     = sum(unlist(apply(d[,factors, drop=F], 2, function(x) { length(unique(x))})))			
  num.rows2    = sum(apply(d[,factors, drop=F], 2, function(x){ a = length(unique(x)); (a*(a-1))/2}))
  
  #### create empty matrix with variable names
  coef.matrix = data.frame(variables = rep("", num.rows), levels=NA, estimate=NA, lower=NA, upper=NA)
  coef.matrix$variables = factor(coef.matrix$variables, levels=c("", factors))		
  
  #### create empty difference.matrix
  difference.matrix = data.frame(variables = NA, comparison = 1:num.rows2, difference=NA, 
                                 lower=NA, upper=NA, cohens.d=NA)
  
  p = 1; p2=1; i=1
  
  for (i in 1:length(factors)){
    
    #### populate df based on levels
    levs = length(unique(d[,factors[i]]))
    levs2 = (levs*(levs-1))/2
    current.rows = p:(p+levs-1)
    current.rows2 = p2:(p2 + levs2-1)
    
    #### populate variable names
    coef.matrix$variables[p] = factors[i]
    
    #### populate the estimates/lower/upper
    f = as.formula(paste0(outcome, "~", factors[i]))
    est = compare_fits(formula = f, data=d, model1=object, model2=NULL, return.preds=T, report.se=T, prediction_method="reference") %>%
      group_by_at(factors[i]) %>%
      summarize(across(prediction.fit:prediction.upr, ~mean(.x))) %>%
      data.frame
    
    coef.matrix$levels[current.rows] = unique(as.character(est[,1]))
    coef.matrix$estimate[current.rows] = est$prediction.fit
    coef.matrix$lower[current.rows] = est$prediction.lwr
    coef.matrix$upper[current.rows] = est$prediction.upr
    
    #### fill in the difference matrix
    difference.matrix$variables[p2] = factors[i]
    center = outer(est$prediction.fit, est$prediction.fit, "-")
    keep = lower.tri(center)
    center = center[keep]
    nn = table(d[,factors[i]])
    df = nrow(d) - length(coef(object))
    width = qtukey(.95, levs, df) *
      summary(object)$sigma * 
      sqrt(outer(1/nn, 1/nn, "+"))[keep]
    difference.names = outer(as.character(est[[factors[i]]]), 
                             as.character(est[[factors[i]]]), 
                             paste, sep = "-")[keep]
    
    difference.matrix$comparison[current.rows2] = difference.names
    difference.matrix[current.rows2,c("difference", "lower", "upper")] = 
      c(center, center-width, center+width)				
    difference.matrix$cohens.d[current.rows2] = difference.matrix$difference[current.rows2]/summary(object)$sigma
    
    #### increment the counter
    p = p + levs
    p2 = p2+levs2
    
  }
  return(list(coef.matrix=coef.matrix,
              difference.matrix = difference.matrix))
  
} 


fit_baseline_model = function(object) {
  dv = get_terms(object)$response
  re = get_re(object)
  form = as.formula(
    paste0(dv, "~1+(1|", re, ")")
  )
  return(update(object, formula=form))
}


remove_interaction_terms = function(object) {
  #### generate list of coefficients
  terms = attr(terms(object), "term.labels")
  interaction = length(grep(":", terms))>0
  if (interaction){
    terms = terms[-grep(":", terms)]
  }
  return(terms)
}

populate_estimates_numeric = function(object, numbers=NULL) {
  
  if (is.null(numbers)) numbers = which_terms_are_factors_or_numbers(object$model, 
                                                                     attr(terms(object), "term.labels"))$numbers
  if (length(numbers)==0) return(NA)
  
  vars = c("(Intercept)", numbers)
  coef.matrix.numb = data.frame(variables=vars, estimate=NA, lower=NA, upper=NA, 
                                std.estimate=NA, std.lower=NA, std.upper=NA)
  coef.matrix.numb$estimate = coef(object)[vars]
  
  # identify non-numeric coefficients
  # if this is NA (e.g., if all values of a predictor are zero), this will throw an error. 
  # must get rid of NA values
  numeric_coefficient = vars[!(is.na(coef(object)[vars]))]
  coef.matrix.numb = coef.matrix.numb[coef.matrix.numb$variables %in% numeric_coefficient,]
  
  # give an error message if they're not the same
  if (length(vars) != length(numeric_coefficient)) {
    stop("Hmmm. It looks like one of your coefficients is NA. Did you include a predictor with no variability?")
  }
  
  #### get upper and lower using matrix multiplication
  all_coefficients = coef(object)[numeric_coefficient]
  matrix_of_coefficients = matrix(all_coefficients, ncol=2, nrow=length(numeric_coefficient), byrow=F)
  multiplier = t(t(c(1.96, -1.96)))
  standard_errors = t(summary(object)$coef[numeric_coefficient,2])
  upper.lower = t(matrix_of_coefficients + 
                    t(multiplier %*% standard_errors))
  coef.matrix.numb$lower = (upper.lower)[2,]
  coef.matrix.numb$upper = (upper.lower)[1,]
  
  
  ##### standardized estimates
  coef.std = standardized.beta(object, se=T)
  
  #### remove those that are numeric
  num = which(names(coef.std$beta) %in% numbers)	
  coef.std$beta = coef.std$beta[num]
  coef.std$se = coef.std$se[num]					
  coef.matrix.numb$std.estimate = c(0, coef.std$beta)
  upper.lower = t(matrix(c(0, coef.std$beta), ncol=2, nrow=length(vars), byrow=F) + t(t(t(c(1.96,-1.96)))%*%t(c(0, coef.std$se))))
  coef.matrix.numb[,c("std.upper", "std.lower")] = t(upper.lower)	
  return(coef.matrix.numb)
  
}

compute_r_squared = function(object) {
  #### report R squared
  r.squared = summary(object)$r.squared
  n = nrow(object$model)
  t.crit = qt(.975, df=n-2)	
  se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
  r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
  r.squared = round(r.squared, digits=4)
  return(r.squared)
}

compute_correlation = function(object) {
  
  #### get dataset
  d = object$model
  terms = attr(terms(object), "term.labels")
  terms = remove_interaction_terms(object)
  
  #### identify factors
  variable_types = which_terms_are_factors_or_numbers(d, terms)
  factors = variable_types$factors
  numbers = variable_types$numbers
  
  if (length(numbers)==1 & length(factors)==0) return(cor(d)[1,2])
  return(NA)
}

removed.one.at.a.time = function(i, terms, object){
  new.f = as.formula(paste0(". ~ . -", terms[i]))
  new.object = update(object, new.f)
  list(
    rsq = summary(object)$r.squared - summary(new.object)$r.squared,
    bayes.factor = bf.bic(object, new.object, invert=FALSE)
  )
}

return_model_comparisons = function(object, terms, mc) {
  
  if (length(terms)<2 | !mc) {
    return(NULL)
  }

  ### do nested model comparisons
  ### this requires superassignment to work with JASP
  #dataset<<-object$model
  dataset = object$model
  all.terms = attr(terms(object), "term.labels")
  mc = t(sapply(1:length(all.terms), removed.one.at.a.time, terms=all.terms, object=object))
  mc = data.frame(cbind(all.terms,mc), stringsAsFactors = FALSE)
  mod.comps = mc
  mod.comps = rbind(c("Full Model", summary(object)$r.squared, NA), mod.comps)
  mod.comps$rsq = as.numeric(mod.comps$rsq); mod.comps$bayes.factor = as.numeric(unlist(mod.comps$bayes.factor))
  return(mod.comps)
}
