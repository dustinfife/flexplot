##' Compare the fits of two models
##'
##' This function takes two fitted models as input and plots them to visually compare how the two differ in terms of fit.
##' It can take a \code{glm}, \code{rlm}, \code{lm}, and \code{randomForest} model (and maybe others as well). The function takes
##' a \code{\link{flexplot}}-like formula as input.  
##'	
##' @param formula A formula that can be used in flexplot. The variables inside must not include variables outside the fitted models. 
##' @param data The dataset containing the variables in formula
##' @param model1 The fitted model object (e.g., lm) containing the variables specified in the formula
##' @param model2 The second fitted model object (e.g., lm) containing the variables specified in the formula
##' @param return.preds Should the function return the predictions instead of a graphic? Defaults to F
##' @param report.se Should standard errors be reported alongside the estimates? Defaults to F. 
##' @param re Should random effects be predicted? Only applies to mixed models. Defaults to F. 
##' @param pred.type What type of predictions should be outputted? This is mostly for \code{glm} models. Defaults to "response." 
##' @param num_points Number of points used for predictions. Larger numbers = slower algorithm, but smoother predictions. 
##' @param clusters For visualizing mixed models, this specifies the number of clusters to display
##' @param ... Other parameters passed to flexplot
##' @author Dustin Fife
##' @return Either a graphic or the predictions for the specified model(s)
##' @export
##' @examples 
##' data(exercise_data)
##' mod1 = lm(weight.loss~therapy.type + motivation, data=exercise_data)
##' mod2 = lm(weight.loss~therapy.type * motivation, data=exercise_data)
##' compare.fits(weight.loss~therapy.type | motivation, data=exercise_data, mod1, mod2)
compare_fits = function(formula, data, model1, model2=NULL, 
                        return.preds=F, report.se=F, re=F, 
                        pred.type="response", num_points = 50,
                        clusters=3,...){
  if (is.null(model2)) runme = "yes"

#### if mod2 is null..
  if (is.null(model2)) model2 = model1
  
  #### get type of model
  model1.type = class(model1)[1]
  model2.type = class(model2)[1]	

  #### get all variables
  variables_mod1 = get_terms(model1)
  variables_mod2 = get_terms(model2)
  testme = unique(c(variables_mod1$predictors, variables_mod2$predictors))
  all_variables = unique(c(variables_mod1$predictors, variables_mod2$predictors, variables_mod1$response, variables_mod2$response))
  
  if (tibble::is_tibble(data)){
    data = as.data.frame(data)
  }

  #### for the rare occasion where deleting missing data changes the levels...
  k = check_missing(model1, model2, data, all_variables)
  
  #### make sure, if they have lme4, both models are lme4 objects
  test_same_class(model1, model2)
  
  #### convert random effects to factors for mixed models
  k = prepare_data_for_compare_fits(k, model1, formula, clusters)

  ### make sure they have the same outcome
  if (variables_mod1$response != variables_mod2$response) {
    stop("It looks like your two models have different outcome variables. That's not permitted, my friend!")
  }
  
  # check for errors
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  compare_fits_errors(k, outcome, predictors, testme)
  
  # generate predictions for the entire dataset (before aggregating) 
  predictions = get_fitted(model1, re=re, pred.type=pred.type, report.se=report.se)
  
  prediction.model = post_prediction_process_cf(model1, model2, predictions, formula, re, k, pred.type)

  #### eliminate those predictions that are higher than the range of the data
  if (!is.factor(data[,outcome])){
    min.dat = min(data[,outcome], na.rm=T); max.dat = max(data[,outcome], na.rm=T)
    too_high = length(which(prediction.model$prediction>(max.dat))>0)
    too_low  = length(which(prediction.model$prediction<(min.dat))>0)
    if ( too_high | too_low ){
      warning("Some of the model's predicted values are beyond the range of the original y-values. 
              I'm truncating the y-axis to preserve the original scale.")
    }
  } else {
    # if they supply a factor, convert it to a number
    prediction.model$prediction = round(as.numeric(as.character(prediction.model$prediction)), digits=3)
    
    # if the original outcome is an ordered factor, convert it to a number
    if (class(data[,outcome])[1] == "ordered") data[,outcome] = as.numeric(as.character(data[,outcome]))
  }
  
  #### create flexplot
  if (return.preds) return(prediction.model)
    
  # for logistic and factor outcome variable, add one to the predictions
  # (otherwise the fitted line falls below the range of y values)
  if (should_shift_predictions(model1.type, model1, outcome, prediction.model$prediction, data)) {
    prediction.model$prediction = prediction.model$prediction + 1
  }
  

  final_geom = return_lims_geom(outcome, data, model1)
  
  # remove duplicate rows
  prediction.model = prediction.model[!duplicated(prediction.model),]
  
  #when we have an intercept only model
  if (nrow(prediction.model)==1) { prediction.model = NULL; final_geom = theme_bw() }
  
  flexplot(formula, data=data, prediction=as.data.frame(prediction.model), suppress_smooth=T, se=F, ...) +
    final_geom
}	

return_lims_geom = function(outcome, data, model1) {
  if (!(class(model1)[1] == "lm" | class(model1)[1] == "glm" | class(model1)[1] == "lmerMod")) return(theme_bw())
  if (family(model1)$link=="logit" & !is.numeric(data[,outcome[1]])) return(theme_bw())
  if (is.factor(data[,outcome]) | is.character(data[,outcome])) return(theme_bw())
  return(coord_cartesian(ylim=c(min(data[,outcome]), max(data[,outcome]))))
}