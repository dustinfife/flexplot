#' Output calculus-based logistic regression coefficients
#' 
#' This function takes a logistic regression model and returns more intuitive slope/intercept
#' parameters. The slope (discrimination) is interpreted as the instantaneous (tangent) slope of the ogive
#' curve at p =  0.5. The intercept (difficulty) is interpreted as the value of X where
#' the logistic curve crosses the 50% threshold
#'
#' @param model A fitted model (glm or glmerMod)
#'
#' @returns Discrimination and difficulty estimates for a regression model
#' @export
#'
#' @examples
#' logistic_model = glm(y_bin~x + a, data=small, family=binomial)
#' logistic_coefficients(logistic_model)
logistic_coefficients = function(model) {

  model_info = gather_model_info(model)
  coefs = model_info$coefs
  coef_names = model_info$coef_names
  
  coef_terms = sapply(coef_names[-1], match_term_names, model_info = model_info)
  
  vars = unique(coef_terms[coef_terms != "unknown"])
  b0 = coefs[1]
  
  default_means = model_info$term_names %>%
    purrr::set_names() %>%
    purrr::map(~ if (is.numeric(model_info$data[[.x]]))
      mean(model_info$data[[.x]], na.rm = TRUE)
      else NA)
  
  result = purrr::map_dfr(vars, compute_logistic_summary_for_variable,
                          model_info = model_info)
  
  return(result)
}

gather_model_info = function(model) {
  coefs = return_logistic_coefficients(model)
  model_data = extract_data_from_fitted_object(model)
  mm = model.matrix(model, data = model_data)
  term_labels = attr(mm, "assign")
  term_names = attr(terms(model), "term.labels")
  coef_names = names(coefs)
  
  model_info = list(
    model = model,
    data = model_data,
    coefs = coefs,
    coef_names = coef_names,
    term_names = term_names,
    term_labels = term_labels,
    mm = mm
  )
  
  return(model_info)
}

compute_logistic_summary_for_variable = function(var,
                                                 model, 
                                                 model_info = NULL) {
  # Derive model_info if not supplied
  if (is.null(model_info)) model_info = gather_model_info(model)
  
  b0 = model_info$coefs[1]
  
  # Compute default means 
  default_means = model_info$term_names %>%
    purrr::set_names() %>%
    purrr::map(~ if (is.numeric(model_info$data[[.x]]))
      mean(model_info$data[[.x]], na.rm = TRUE)
      else NA)
  
  related_coefs = model_info$coef_names[stringr::str_starts(model_info$coef_names, var)]
  b_j = sum(model_info$coefs[related_coefs], na.rm = TRUE)
  
  other_vars = setdiff(model_info$term_names, var)
  
  offset = b0 + sum(purrr::map_dbl(other_vars, function(other_var) {
    val = default_means[[other_var]]
    if (is.na(val)) return(0)
    other_related = model_info$coef_names[stringr::str_starts(model_info$coef_names, other_var)]
    sum(model_info$coefs[other_related], na.rm = TRUE) * val
  }))
  
  # Only compute slope and threshold if the predictor is numeric
  if (is.numeric(model_info$data[[var]])) {
    slope = b_j / 4
    threshold = -offset / b_j
  } else {
    slope = NA
    threshold = NA
  }
  
  dplyr::tibble(
    variable = var,
    instantaneous_slope = slope,
    intercept_threshold = threshold
  )
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


##' Compute the ICC from a lmer (package lme4) model
##'
##' This function will extract the variances from a mixed model and output the value of the ICC
##'	
##' Recall that the equation for the ICC is var(school)/[var(school)+ var(person)]. 
##' This forumla extracts the necessary variances from the mixed model and computes it. 
##' @param model a lmer model
##' @return the ICC
##' @author Dustin Fife
##' @export
##' @examples
##' data(alcuse)
##' require(lme4)
##' model = lmer(ALCUSE~1|ID, data=alcuse)
##' icc(model)
icc = function(model){
  #### compute ICC
  var.components = as.data.frame(VarCorr(model))$vcov
  ICC = var.components[1]/sum(var.components)
  
  #### find out average cluster size
  id.name = names(coef(model))
  clusters = nrow(matrix(unlist((coef(model)[id.name]))))
  n = length(residuals(model))
  average.cluster.size = n/clusters
  
  #### compute design effects
  design.effect = 1+ICC*(average.cluster.size-1)
  
  #### return stuff
  list(icc=ICC, design.effect=design.effect)
  
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
    est = compare.fits(formula = f, data=d, model1=object, model2=NULL, return.preds=T, report.se=T) %>% 
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
