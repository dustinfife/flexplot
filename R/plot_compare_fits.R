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
compare.fits = function(formula, data, model1, model2=NULL, 
                        return.preds=F, report.se=F, re=F, 
                        pred.type="response", num_points = 50,
                        clusters=3,...){
  
  if (is.null(model2)) runme = "yes"
  
  #### if mod2 is null..
  if (is.null(model2)){
    model2 = model1
  } 
  
  #### get type of model
  model1.type = class(model1)[1]
  model2.type = class(model2)[1]	

  #### get all variables
  variables_mod1 = get_terms(model1)
  variables_mod2 = get_terms(model2)
  model_terms   = unique(c(variables_mod1$predictors, variables_mod2$predictors))
  all_variables = unique(c(variables_mod1$predictors, variables_mod2$predictors, variables_mod1$response, variables_mod2$response))
  
  # do all the checks/manipulations for the data
  data = prepare_data_for_compare.fits(data, model1, model2, all_variables)
  # put in a check to take data from the larger of the two models
  # if they don't provide a formula
  if (is.null(formula)) formula = make_flexplot_formula(variables_mod1$predictors, variables_mod1$response, data, drop_second_slot=T)
  
  ##### extract variable names from FORMULA
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  
  #### convert random effects to factors for mixed models
  data = subset_random_model(model1, formula, d=data, samp.size = clusters)
  
  check_errors_compare_fits(model1, model2, data, formula)
  

  # generate predictor values
  pred.values = generate_predictors(data, formula, model1, ...)
  pred.mod1 = generate_predictions(model1, re, pred.values, pred.type, report.se)
  
  ### there's no fixed effect if we don't have these lines
  model1.type = class(model1)[1]
  if (model1.type == "lmerMod" | model1.type == "glmerMod"){
    pred.mod1 = data.frame(prediction = predict(model1, pred.values, type="response", re.form=NA), model= "fixed effects")		
  }
  
  if (!exists("runme")) {
    pred.mod2 = generate_predictions(model2, re, pred.values, pred.type, report.se)
  } else {
    pred.mod2 = pred.mod1
  }
  if ((model2.type == "lmerMod" | model2.type == "glmerMod") & re){
    pred.mod2 = data.frame(prediction = predict(model2, pred.values, type="response"), model= "random effects")	
    old.mod=0	
  }
  
  #### convert polyr back to numeric (if applicable)
  if (model1.type == "polr" | model2.type == "polr"){
    data[,outcome] = as.numeric(as.character(data[,outcome]))		
    pred.mod1$prediction = as.numeric(as.character(pred.mod1$prediction))
    pred.mod2$prediction = as.numeric(as.character(pred.mod2$prediction))		
  }
  
  # set the names of the models
  pred.mod1$model = return_model_labels(model1, deparse(substitute(model1)), pred.mod1$model, re=re)
  pred.mod2$model = return_model_labels(model2, deparse(substitute(model2)), pred.mod2$model, re=re)
  pred.mod2$model = change_model_names_if_same(pred.mod1$model, pred.mod2$model)
  
  #### report one or two coefficients, depending on if they supplied it
  if (!exists("runme") | exists("old.mod")){
    prediction.model = rbind(pred.mod1, pred.mod2)
    prediction.model = cbind(pred.values, prediction.model)
  } else {
    prediction.model = pred.mod1
    prediction.model = cbind(pred.values, prediction.model)
  }

  # remove duplicate rows
  prediction.model = prediction.model[!duplicated(prediction.model),]  
  #### return the dataset
  if (return.preds) return(prediction.model)
  
  #when we have an intercept only model
  final_geom = return_lims_geom(outcome, data, model1)

  #when we have an intercept only model
  if (nrow(prediction.model)==1) { prediction.model = NULL; final_geom = theme_bw() }
  flexplot(formula, data=data, prediction=prediction.model, suppress_smooth=T, se=F, ...) +
    final_geom
  
}	

return_lims_geom = function(outcome, data, model1) {
  if (!(class(model1)[1] == "lm" | class(model1)[1] == "glm")) return(theme_bw())
  if (family(model1)$link=="logit" & !is.numeric(data[,outcome[1]])) return(theme_bw())
  if (is.factor(data[,outcome]) | is.character(data[,outcome])) return(theme_bw())
  return(coord_cartesian(ylim=c(min(data[,outcome]), max(data[,outcome]))))
}