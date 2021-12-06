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
  d = extract_data_from_fitted_object(model)
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
    numeric.preds = list(return_plus_minus_one_sd(d[,numeric_variables]))
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
  d = extract_data_from_fitted_object(model)
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
  numeric.preds = generate_numeric_predictions(numeric, d)

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

return_mean_for_intercept_models = function(model, outcome=NULL, data=NULL) {
  if (is.null(outcome)) outcome    = get_terms(model)$response
  if (is.null(data))    data = extract_data_from_fitted_object(model)
  
  f = as.formula(paste0(outcome, "~1"))
  est = compare.fits(formula = f, data=data, model1=model, model2=NULL, return.preds=T, report.se=T)
  return = est[2:4]
  names(return) = c("Mean", "Lower", "Upper")
  return$d = coef(model)/summary(model)$sigma
  return(return)
}

get_factor_numeric_names = function(data, predictors) {
  
  if (length(predictors)==0) return(list(cat=character(0), numb=character(0)))
  #### get variable types
  cat      = names(which(unlist(lapply(data[,predictors, drop=FALSE], function(x) (!is.numeric(x))))))
  numb     = predictors[predictors %!in% cat]
  list(cat=cat, numb=numb)
}

delta_rsquare = function(object) {
  
  ssr = drop1(aov(object))[-1,"Sum of Sq"]
  ssr2 = aov(object)$effects
  if (length(ssr)<(nrow(anova(object))-1)){
    message("Note: I am not reporting the semi-partial R squared for the main effects because an interaction is present. To obtain main effect sizes, drop the interaction from your model. \n\n")
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

create_empty_estimates_matrices = function(d, factors) {
  factor_levels    =     unlist(lapply(d[,factors, drop=FALSE], levels))
  estimates_rows   = sum(unlist( apply(d[,factors, drop=FALSE], 2, function(x) { length(unique(x))})))			
  differences_rows = sum(apply(d[,factors, drop=FALSE], 2, function(x){ a = length(unique(x)); (a*(a-1))/2}))
  
  #### create empty matrix with variable names
  coef.matrix = data.frame(variables = rep("", estimates_rows), levels=NA, estimate=NA, lower=NA, upper=NA)
  coef.matrix$variables = factor(coef.matrix$variables, levels=c("", factors))		
  
  #### difference.matrix
  difference.matrix = data.frame(variables = NA, comparison = 1:differences_rows, difference=NA, 
                                 lower=NA, upper=NA, cohens.d=NA)
  return(list(coef.matrix=coef.matrix, difference.matrix=difference.matrix))
}

populate_estimates_matrix = function(object, 
                                     d=NULL, factors=NULL,outcome=NULL){

  # get null object (for easy testing)
  if (is.null(d))       d = extract_data_from_fitted_object(object)
  if (is.null(factors)) factors = get_factor_numeric_names(d,
                                                           get_terms(object, nonlinear_terms = T)$predictors)$cat
  if (is.null(outcome)) outcome = get_terms(object, nonlinear_terms = T)$response
  coef.matrix=create_empty_estimates_matrices(d, factors)$coef.matrix
  difference.matrix=create_empty_estimates_matrices(d, factors)$difference.matrix
  
  
  p = 1; p2=1; i=1
  for (i in 1:length(factors)){
    
    #### populate df based on levels
    levs = length(levels(d[,factors[i]]))
    levs2 = (levs*(levs-1))/2
    current.rows = p:(p+levs-1)
    current.rows2 = p2:(p2 + levs2-1)

    #### populate variable names
    coef.matrix$variables[p] = factors[i]
    
    #### populate the estimates/lower/upper
    f = as.formula(paste0(outcome, "~", factors[i]))
    est = compare.fits(formula = f, data=d, model1=object, model2=NULL, return.preds=T, report.se=T)
    
    
    coef.matrix$levels[current.rows] = as.character(est[,1])
    coef.matrix$estimate[current.rows] = est$prediction.fit
    coef.matrix$lower[current.rows] = est$prediction.lwr
    coef.matrix$upper[current.rows] = est$prediction.upr
    
    
    #### fill in the difference matrix
    difference.matrix$variables[p2] = factors[i]
    center = outer(est$prediction.fit, est$prediction.fit, "-")
    keep <- lower.tri(center)
    center <- center[keep]
    nn = table(d[,factors[i]])
    df = nrow(d) - length(coef(object))
    width = qtukey(.95, levs, df) *
      summary(object)$sigma * 
      sqrt(outer(1/nn, 1/nn, "+"))[keep]
    difference.names = outer(as.character(est[,1]), 
                             as.character(est[,1]), 
                             paste, sep = "-")[keep]
    difference.matrix$comparison[current.rows2] = difference.names
    difference.matrix[current.rows2,c("difference", "lower", "upper")] = 
      c(center, center-width, center+width)				
    difference.matrix$cohens.d[current.rows2] = difference.matrix$difference[current.rows2]/summary(object)$sigma
    
    #### increment the counter
    p = p + levs
    p2 = p2+levs2
    
    
  }
  
  return(list(difference.matrix = difference.matrix, coef.matrix = coef.matrix))
  
}
  






#

