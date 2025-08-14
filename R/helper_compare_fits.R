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

is_binary_01 = function(predictions) {
  all(unique(predictions) %in% c(0, 1))
}

should_shift_predictions = function(model_type, model, outcome, predictions, data) {
  if (model_type == "glm") {
    return(
      family(model)$link == "logit" &&
        !is.numeric(data[[outcome[1]]])
    )
  } else if (model_type == "RandomForest") {
    return(is_binary_01(predictions))
  }
  return(FALSE)
}

compare_fits_errors = function(data, outcome, predictors, testme=NULL) {
  
  ## see if all predictors are categorical
  dv_is_factor = length(unique(data[,outcome]))<3
  axis_is_factor = ifelse(length(predictors)>0,
                          check.non.number(data[,predictors[1]]),
                          FALSE)
  # if (dv_is_factor & axis_is_factor) {
  #   stop("Well, darn. You've found a limitation of flexplot. Flexplot cannot use the compare.fits function when
  #        both your outcome variable and your x-axis variable are categorical. Maybe try putting a numeric variable on the x-axis. ")
  # }
  
  ##### make sure they're putting the same variables from formula in terms
  if (!(all(predictors %in% testme))){
    missing_vars = paste0(predictors[!(predictors%in%testme)], collapse=", ")
    stop(paste0("Sorry, but some variables in formula don't match what's in the model. Specifically, these variables are your in your formula, but not in the model:\n    ", missing_vars))
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

#' Extract Variable Names from Model Objects
#'
#' This function extracts predictor and response variable names from various
#' types of fitted model objects. It is used internally by flexplot functions
#' to determine which variables are available in a model.
#'
#' @param model A fitted model object (e.g., lm, glm, RandomForest, keras model, etc.)
#'
#' @return A list with two elements:
#'   \item{predictors}{Character vector of predictor variable names}
#'   \item{response}{Character string of the response variable name}
#'
#' @details
#' This generic function provides a consistent interface for extracting variable
#' names across different model types. Each model class should have its own method
#' that handles the specifics of variable extraction for that model type.
#'
#' @seealso \code{\link{compare.fits}} for the main function that uses this generic
#'
#' @examples
#' # Linear model example
#' data(mtcars)
#' model = lm(mpg ~ hp + wt, data = mtcars)
#' terms_info = get_terms(model)
#' terms_info$predictors  # "hp" "wt"
#' terms_info$response    # "mpg"
#'
#' @export
get_terms = function(model) {
  UseMethod("get_terms")
}

#' Extract Variable Names from RandomForest Models
#'
#' S3 method for extracting variable names from RandomForest objects.
#'
#' @param model A fitted RandomForest object from the party package
#' @return A list with elements "predictors" and "response"
#'
#' @method get_terms RandomForest
#' @export
get_terms.RandomForest = function(model) {
  predictors = get_cforest_variables(model, "predictors")
  response = get_cforest_variables(model, "response")
  return(list(predictors = predictors, response = response))
}


#' Extract Variable Names from General Model Objects
#'
#' Default S3 method for extracting variable names from model objects that
#' have a formula() method.
#'
#' @param model A fitted model object with a formula() method
#' @return A list with elements "predictors" and "response"
#'
#' @details This default method works for most standard R model objects including
#'   lm, glm, and other models that store their formula and support the formula() function.
#'
#' @method get_terms default
#' @export
get_terms.default = function(model) {
  form = formula(model) 
  predictors = all.vars(form)[-1]  
  response = all.vars(form)[1]
  return(list(predictors = predictors, response = response))
}

check_missing = function(model1, model2=NULL, data, variables) {

  ### if they haven't supplied model 2, no need to check
  if (is.null(model2)) return(data)
  
  n1 = get_model_n(model1)
  n2 = get_model_n(model2)
  
  # Only check if both are not NULL
  if (!is.null(n1) && !is.null(n2) && (n1 < nrow(data) | n2 < nrow(data))) {
    data = na.omit(data[,variables])
  }
  
  return(data)
}

#' Get Number of Observations from Model Objects
#'
#' Extract the number of observations used to fit a model object.
#'
#' @param model A fitted model object
#' @return Integer representing the number of observations, or NULL if unknown
#' @export
get_model_n = function(model) {
  UseMethod("get_model_n")
}

#' @method get_model_n default
#' @export
get_model_n.default = function(model) {
  mod_class = class(model)[1]
  if (mod_class == "RandomForest") return(attr(model, "responses")@nobs)
  if (mod_class == "randomForest.formula") return(length(model$predicted))
  if (mod_class == "lmerMod" | mod_class == "glmerMod") return(nobs(model))
  if (mod_class == "rpart") return(length(model$y))
  
  return(nrow(model$model))
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

#not_binned_but_plotted(c("a", "b", "c"), c("b_binned"))
#not_binned_but_plotted(c("a", "b"), c(""))
#not_binned_but_plotted(c("a", "b"), c("a_binned", "b_binned"))
not_binned_but_plotted = function(plot_variables, binned_variables) {
  fake_bin_names = paste0(plot_variables, "_binned")
  actually_binned = fake_bin_names %in% binned_variables
  c(plot_variables[!actually_binned], fake_bin_names[actually_binned])
}

bin_if_theres_a_flexplot_formula = function(formula, data, ...) {
  
  # extract given/axis
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  
  # reproduce breaks from flexplot in the dataset (if they supply them)
  list_values = list(...)
  
  # see if they didn't give a flexplot formula
  given_length = length(given)
  axis_length  = length(axis)
  if (axis_length>2 & is.na(given[1])) return(data) 
    
  # if they have a flexplot formula, bin things
  binned_data = reproduce_breaks(data, formula, list_values)
  k=binned_data$binned_data
  breaks=binned_data$breaks
  
  # # for all binned variables, average within bins
  a = names(breaks) %>% purrr::map(replace_numeric_with_average, data=k, breaks=breaks)
  k[,names(breaks)] = a
  return(k)
}

bin_if_theres_a_flexplot_formula2 = function(data, formula,  ...) {
  
  # extract given/axis
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  
  # reproduce breaks from flexplot in the dataset (if they supply them)
  list_values = list(...)
  
  # see if they didn't give a flexplot formula
  given_length = length(given)
  axis_length  = length(axis)
  if (axis_length>2 & is.na(given[1])) return(data) 
  
  # if they have a flexplot formula, bin things
  binned_data = reproduce_breaks(data, formula, list_values)
  k=binned_data$binned_data
  return(k)
}


return_constant_for_predicted_data = function(missing_variable, data, model) {
  
  if (length(missing_variable)==0) return(NA)
  if (is.numeric(data[,missing_variable])) {
    message(paste0("Note: You didn't choose to plot ", missing_variable, " so I am inputting the mean\n"))
    data[,missing_variable] = mean(data[,missing_variable], na.rm=T)
    return(data[,missing_variable])
  }
  
  # this had issues when a random effect (as factor)
  val = unique(as.character(data[[missing_variable]]))[1]
  #only display the message if it's not a glmer mod
  if (!(class(model)[1] %in% c("lmerMod", "glmerMod"))) {
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
  bins  =  if("bins"   %in% names(list_values))  unlist(list_values["bins"]) else 3
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

### function to generate prediction matrix spanning the range of the data
generate_predictors = function(data, formula, model, ...) {
  
  ## extract variable slots
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  
  k = bin_if_theres_a_flexplot_formula(formula, data, ...)
  
  # identify those variables in the model that are not plotted
  # (If I don't do this, we'll get a jagged line in the visuals)
  vars_in_model = get_predictors(model)
  which_are_missing = remove_nonlinear_terms(vars_in_model[!(vars_in_model %in% variables)])
  
  # replace the missing variables with mean (numeric) or a level
  new_values = which_are_missing %>% purrr::map(return_constant_for_predicted_data, data=k, model=model)
  if (length(which_are_missing)>0) k[,which_are_missing] = new_values
  
  # remove the outcome variable (because it's replaced with "prediction" now)
  k[,outcome] = NULL
  # remove variables not in there
  #find all variables in either formula or model
  
  all_variables_in_either = remove_nonlinear_terms(unique(c(predictors, vars_in_model)))
  return(k[,all_variables_in_either, drop=FALSE])
}






