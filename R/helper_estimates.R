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
  
  if (length(numeric_variables)> 1) return(lapply(d[,numeric_variables], return_plus_minus_one_sd))
  if (length(numeric_variables)==1 
    & length(unique(d[,numeric_variables]))>5){
    numeric.preds = list(return_plus_minus_one_sd(d[,numeric_variables]))
    names(numeric.preds)[[1]] = numeric_variables
    return(numeric.preds)
  }
  return(NULL)
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
