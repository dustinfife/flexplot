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
  testme = unique(c(variables_mod1$predictors, variables_mod2$predictors))
  all_variables = unique(c(variables_mod1$predictors, variables_mod2$predictors, variables_mod1$response, variables_mod2$response))
  
  if (tibble::is_tibble(data)){
    data = as.data.frame(data)
  }
  
  #### for the rare occasion where deleting missing data changes the levels...
  data = check_missing(model1, model2, data, all_variables)
  
  #### make sure, if they have lme4, both models are lme4 objects
  test_same_class(model1, model2)
  
  #### convert random effects to factors for mixed models
  
  data = subset_random_model(model1, formula, d=data, samp.size = clusters)

  ### make sure they have the same outcome
  if (variables_mod1$response != variables_mod2$response) {
    stop("It looks like your two models have different outcome variables. That's not permitted, my friend!")
  }
  
  ##### extract variable names from FORMULA
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  
  # check for errors
  compare_fits_errors(data, outcome, predictors, testme)

  # generate predictor values
  pred.values = generate_predictors(data, formula, model1, ...)

  # ensure pred.values have same class as original data
  # but don't change RE; because prior to this there's been sampling of the data and this would revert that
  randef = extract_random_term(model1)
  
  all_predictors_minus_re = ifelse(length(randef)>0, predictors[!(predictors==randef)], predictors)
  # when we have a mean model, this fails without the if statement
  if (!is.na(all_predictors_minus_re)) {
    a = all_predictors_minus_re %>% purrr::map(make_data_types_the_same, pred.values, extract_data_from_fitted_object(model1))
    pred.values[,all_predictors_minus_re] = a
  }
  
  # for intercept only models
  if (nrow(pred.values)==0) {
    pred.values = data.frame("(Intercept)" = 1)
  }

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
  
  #### if they have the same name, just call them model1 and model2
  
  if (!re){
    pred.mod1$model = paste0(deparse(substitute(model1)), " (", model1.type, ")", collapse="")
    if (pred.mod1$model[1] == pred.mod2$model[1]){
      pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, " 2)", collapse="")
    } else {
      pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, ")", collapse="")
    }
  }
  
  #### report one or two coefficients, depending on if they supplied it
  if (!exists("runme") | exists("old.mod")){
    prediction.model = rbind(pred.mod1, pred.mod2)
    prediction.model = suppressWarnings(cbind(pred.values, prediction.model))
  } else {
    prediction.model = pred.mod1
    prediction.model = suppressWarnings(cbind(pred.values, prediction.model))
  }
  
  #### eliminate those predictions that are higher than the range of the data
  if (!is.factor(data[,outcome])){
    min.dat = min(data[,outcome], na.rm=T); max.dat = max(data[,outcome], na.rm=T)
    if (length(which(
        prediction.model$prediction>(max.dat))>0 |
          length(which(prediction.model$prediction<(min.dat))))){
      #prediction.model  = prediction.model[-which(prediction.model$prediction>max.dat | prediction.model$prediction<min.dat), ]
      warning("Some of the model's predicted values are beyond the range of the original y-values. 
              I'm truncating the y-axis to preserve the original scale.")
      
    }
  } else {
    #### if they supply a factor, convert it to a number!!!!!
    prediction.model$prediction = round(as.numeric(as.character(prediction.model$prediction)), digits=3)
  }
  
  #### create flexplot
  if (return.preds){
    prediction.model
  } else {
    # at one time, I was adding one to the predictions. WHY??????
    # ### for logistic, add one to the predictions
    # if (model1.type == "glm" ) {
    #   if (family(model1)$link=="logit" & !is.numeric(data[,outcome[1]])){
    #     prediction.model$prediction = prediction.model$prediction + 1
    #   }
    # } 

    final_geom = return_lims_geom(outcome, data, model1)
    #when we have an intercept only model
    if (nrow(prediction.model)==1) { prediction.model = NULL; final_geom = theme_bw() }
    # remove duplicate rows
    prediction.model = prediction.model[!duplicated(prediction.model),]
    flexplot(formula, data=data, prediction=prediction.model, suppress_smooth=T, se=F, ...) +
      final_geom
  }	
  
}	

return_lims_geom = function(outcome, data, model1) {
  if (!(class(model1)[1] == "lm" | class(model1)[1] == "glm")) return(theme_bw())
  if (family(model1)$link=="logit" & !is.numeric(data[,outcome[1]])) return(theme_bw())
  if (is.factor(data[,outcome]) | is.character(data[,outcome])) return(theme_bw())
  return(coord_cartesian(ylim=c(min(data[,outcome]), max(data[,outcome]))))
}