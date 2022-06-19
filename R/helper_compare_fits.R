get_re = function(model) {
  if (!(class(model)[1] %in% c("lmerMod", "glmerMod"))) {
    return(NULL)
  }
  text_formula = paste0(formula(model))[3]
  re = gsub(".* | [.+]", "\\1", text_formula)
  re = gsub(")", "", re)
  return(re)
}

whats_model2 = function(model1,model2=NULL) {
  if (is.null(model2)){
    return(model1)
  } 
  return(model2)
}

compare_fits_errors = function(data, outcome, predictors, testme=NULL) {
  
  ## see if all predictors are categorical
  dv_is_factor = length(unique(data[,outcome]))<3
  axis_is_factor = ifelse(length(predictors)>0,
                          check.non.number(data[,predictors[1]]),
                          FALSE)
  if (dv_is_factor & axis_is_factor) {
    stop("Well, darn. You've found a limitation of flexplot. Flexplot cannot use the compare.fits function when
         both your outcome variable and your x-axis variable are categorical. Maybe try putting a numeric variable on the x-axis. ")
  }
  
  ##### make sure they're putting the same variables from formula in terms
  if (!(all(predictors %in% testme))){
    stop(paste0("Sorry, but some variables in formula don't match what's in the model. Specifically: ", paste0(variables[!(variables%in%testme)], collapse=",")))
  }
  
  ##### make sure they're using the right dataset
  if (!(all(predictors %in% names(data)))){
    stop(paste0("Sorry, but some variables in formula don't match what's in the dataset. Specifically: ", paste0(variables[!(variables%in%data)], collapse=","), ". Did you input the wrong dataset?"))
  }	
  return(NULL)
}

# function that extracts variables from cforest model
get_cforest_variables = function(model, return.type=c("all", "predictors", "response")) {
  
  return.type = match.arg(return.type)
  
  ## get all variables 
  vars = attr(model, "data")@formula
  
  if (return.type == "all") {
    response = unlist(strsplit(as.character(vars$response)[2], " + ", fixed=T))
    input = unlist(strsplit(as.character(vars$input)[2], " + ", fixed=T))
    all_vars = c(response, input)
    return(all_vars)
  }
  
  if (return.type == "predictors") {
    input = unlist(strsplit(as.character(vars$input)[2], " + ", fixed=T))
    return(input)
  }
  
  response = unlist(strsplit(as.character(vars$response)[2], " + ", fixed=T))
  return(response)
}

get_terms = function(model) {
  
  model.type = class(model)[1]
  
  #### extract the terms from each MODEL
  if (model.type == "RandomForest") {
    predictors = get_cforest_variables(model, "predictors");
    response = get_cforest_variables(model, "response");
    return(list(predictors = predictors, response=response))
  } 
    
  form = formula(model) 
  predictors=all.vars(form)[-1]  
  response = all.vars(form)[1]
  return(list(predictors = predictors, response=response))
}

check_missing = function(model1, model2=NULL, data, variables) {

  ### if they haven't supplied model 2, no need to check
  if (is.null(model2)) return(data)
  
  n1 = get_model_n(model1)
  n2 = get_model_n(model2)
  
  if (n1<nrow(data) | n2<nrow(data)){
    data = na.omit(data[,variables])
  }
  
  return(data)
}

get_model_n = function(model) {

  mod_class = class(model)[1]
  if (mod_class == "RandomForest") return(attr(model, "responses")@nobs)
  if (mod_class == "randomForest.formula") return(length(model$predicted))
  if (mod_class == "lmerMod" | mod_class == "glmerMod") return(nobs(model))
  if (mod_class == "rpart") return(length(model$y))
  
  return(nrow(model$model))
  
}

check_errors_compare_fits = function(model1, model2, data, formula=NULL) {
  
  if (is.null(formula)) formula = formula(model1)
  variables_mod1 = get_terms(model1)
  variables_mod2 = get_terms(model2)
  model_terms   = unique(c(variables_mod1$predictors, variables_mod2$predictors))
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  
  ### make sure they have the same outcome
  if (variables_mod1$response != variables_mod2$response) {
    stop("It looks like your two models have different outcome variables. That's not permitted, my friend!")
  }
  
  ##### make sure they're putting the same variables from formula in terms
  if (!(all(predictors %in% model_terms))){
    stop(paste0("Sorry, but some variables in formula don't match what's in the model. Specifically: ", 
                paste0(predictors[!(predictors%in%model_terms)], collapse=",")))
  }
  
  ##### make sure they're using the right dataset
  if (!(all(predictors %in% names(data)))){
    stop(paste0("Sorry, but some variables in formula don't match what's in the dataset. Specifically: ", 
                paste0(variables[!(variables%in%data)], collapse=","), ".\nDid you input the wrong dataset?\n\nMaybe you should take a nap."))
  }
}

modify_for_mean_model = function(model1, predictors) {
  # ensure pred.values have same class as original data
  # but don't change RE; because prior to this there's been sampling of the data and this would revert that
  randef = extract_random_term(model1)
  
  all_predictors_minus_re = ifelse(length(randef)>0, predictors[!(predictors==randef)], predictors)
  
  # when we have a mean model, this fails without this if statement
  if (!is.na(all_predictors_minus_re)) {
    a = all_predictors_minus_re %>% purrr::map(make_data_types_the_same, pred.values, extract_data_from_fitted_object(model1))
    pred.values[,all_predictors_minus_re] = a
  }
  
  # for intercept only models
  if (nrow(pred.values)==0) {
    pred.values = data.frame("(Intercept)" = 1)
  }
  
}

prepare_data_for_compare.fits = function(data=NULL, model1, model2=NULL, all_variables=NULL) {
  
  # identify which is the bigger model
  
  if (is.null(data))    data = extract_data_compare_fits(model1, model2)
  if (tibble::is_tibble(data)) data = as.data.frame(data)
  if (is.null(all_variables)) all_variables = all.vars(formula(model1))
  #### for the rare occasion where deleting missing data changes the levels...
  data = check_missing(model1, model2, data, all_variables)
  return(data)
  
}

make_data_types_the_same = function(variable, predicted_data, model_data) {
  
  class_model = class(model_data[,variable])
  class_prediction = class(predicted_data[,variable])
  if (identical(class_model, class_prediction)) return(predicted_data[,variable])
  
  # if it's an ordered factor
  if (class_model[1] == "ordered") {
    old_levels = levels(model_data[,variable])
    new_x = factor(predicted_data[,variable], levels=old_levels, ordered=T)
    return(new_x)
  } 
  # if it's a regular factor
  if (class_model[1] == "factor") {
    old_levels = levels(model_data[,variable])
    new_x = factor(predicted_data[,variable], levels=old_levels)
    return(new_x)
  }
  
  if (class(model_data[,variable])[1] == "numeric") return(as.numeric(as.character(predicted_data[,variable])))
  if (class(model_data[,variable])[1] == "integer") return(as.integer(predicted_data[,variable]))
  
}

### function to generate prediction matrix spanning the range of the data
generate_predictors = function(data, formula, model, ...) {
  
  ## extract variable slots
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis

  # reproduce breaks from flexplot in the dataset
  list_values = list(...)
  
  binned_data = reproduce_breaks(data, formula, list_values)
  k=binned_data$binned_data
  breaks=binned_data$breaks
  
  # for all binned variables, average within bins 
  a = names(breaks) %>% purrr::map(replace_numeric_with_average, data=k, breaks=breaks)
  k[,names(breaks)] = a

  # identify those variables in the model that are not plotted
  # (If I don't do this, we'll get a jagged line in the visuals)
  vars_in_model = get_predictors(model)
  which_are_missing = remove_nonlinear_terms(vars_in_model[!(vars_in_model %in% variables)])
  
  # replace the missing variables with mean (numeric) or a level
  new_values = which_are_missing %>% purrr::map(return_constant_for_predicted_data, data=k, model=model)
  if (length(new_values)>0) k[,which_are_missing] = new_values
  
  # remove the outcome variable (because it's replaced with "prediction" now)
  k[,outcome] = NULL
  # remove variables not in there
  #find all variables in either formula or model
  
  all_variables_in_either = remove_nonlinear_terms(unique(c(predictors, vars_in_model)))
  return(k[,all_variables_in_either, drop=FALSE])
}

return_constant_for_predicted_data = function(missing_variable, data, model) {
  
  if (length(missing_variable)==0) return(NA)
  if (is.numeric(data[,missing_variable])) {
    message(paste0("Note: You didn't choose to plot ", missing_variable, " so I am inputting the median\n"))
    data[,missing_variable] = median(data[,missing_variable], na.rm=T)
    return(data[,missing_variable])
  }
  
  # this had issues when a random effect (as factor)
  val = unique(as.character(data[[missing_variable]]))[1]
  #only display the message if it's not a glmer mod
  if (!(class(model) %in% c("lmerMod", "glmerMod"))) {
    message(paste0("Note: You didn't choose to plot ",
                   missing_variable, " so I am inputting '", val, "'\n"))
  }
  data[,missing_variable] = val
  return(data[,missing_variable])
  
}

replace_numeric_with_average = function(variable, breaks, data) {
  
  binned_name = paste0(variable, "_binned")
  f = make.formula(variable, binned_name)
  means_by_bin = aggregate(f, FUN=mean, data=data)
  
  data[,variable] = round(as.numeric(as.character(
                        cut(data[,variable],
                             breaks[[variable]],
                             labels = means_by_bin[,2]))), digits=3)
  return(data[,variable])
}

reproduce_breaks = function(data, formula, list_values) {
  # check if they supplied bins, breaks, or labels arguments in ....
  # arguments = c("bins", "breaks", "labels")
  # are_arguments_in_dotdotdot = any(arguments %in% names(list_values))
  # if (!(are_arguments_in_dotdotdot)) return(list(binned_data=data, breaks=NULL))
  
  ## extract variable slots
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  
  # find bins/breaks/labels
  bins  =  if("bins"  %in% names(list_values))  unlist(list_values["bins"]) else 3
  breaks = if("breaks" %in% names(list_values)) (list_values["breaks"]$breaks) else NULL
  labels = if("labels" %in% names(list_values)) (list_values["labels"]$labels) else NULL
  
  
  break.me = flexplot_break_me(data, predictors, given, axis, bins)
  breaks = flexplot_create_breaks(break.me = break.me, breaks, data, labels, bins=bins)

  # now make the binned columns in the dataset
  binned_data = bin_variables(data=data, bins=bins, labels=labels, break.me=break.me, breaks=breaks)
  
  return(list(binned_data=binned_data, breaks=breaks))
 
}

generate_quadriture = function(x, number_points = 15) {
  seq(from=min(x), to=max(x), length.out=15)
}

generate_predictions = function(model, re, pred.values, pred.type, report.se) {

  model.type = class(model)[1]
  if ((model.type == "lmerMod" | model.type == "glmerMod") & !re){
    return(data.frame(prediction = 
                 predict(model, pred.values, type="response"), model="fixed effects"))
  }  
  
  if ((model.type == "lmerMod" | model.type == "glmerMod") & re){
    return(data.frame(
      prediction = 
                  predict(model, pred.values, type="response", re.form=NA), model="random effects"))
  }  
  
  if (model.type == "polr"){
      return(
        data.frame(prediction = predict(model, pred.values, type="class", re.form=NA), model= model.type)		
      )
  }

  if (model.type=="RandomForest") {
    
    ## get dataset to test that classes are all the same
    response = attr(model, "data")@get("response")
    outcome = attr(model, "data")@get("input")
    data = cbind(response, outcome)
    # check if classes differ from old to new, and correct if they are
    class_preds = lapply(pred.values, class)
    class_data = lapply(data[names(pred.values)], class)
    if (!identical(class_preds, class_data)) {
      for (i in 1:length(class_preds)) {
        if (class(data[,names(pred.values[i])])== "factor") 
          pred.values[,i] = factor(pred.values[,i], levels=levels(data[,names(pred.values[i])]))
        else 
          class(pred.values[,i]) = class(data[,names(pred.values[i])])
      }
    }
    
    prediction = predict(model, newdata=pred.values, OOB = TRUE)
    d = data.frame(prediction = prediction, model=model.type)
    names(d)[1] = "prediction"
    return(d
    )    
  }
  
  if (model.type == "rpart") {
    return(
      data.frame(prediction = predict(model, pred.values), model= model.type)		
    )
  }
  
  int = ifelse(report.se, "confidence", "none")
  return(
    data.frame(prediction = predict(model, pred.values, interval=int, type=pred.type), model=model.type)
  )
}


