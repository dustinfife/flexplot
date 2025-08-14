#' Generate Predictions from Model Objects
#'
#' This is a generic function for generating predictions from various types of
#' model objects. It is primarily used internally by \code{\link{compare.fits}}
#' to create consistent prediction outputs across different modeling frameworks.
#'
#' @param model A fitted model object (e.g., lm, glm, lmerMod, keras model, etc.)
#' @param re Logical. Should random effects be predicted? Only applies to mixed models.
#'   For most model types, this parameter is ignored. Default is FALSE.
#' @param pred.values A data frame containing predictor values for which predictions
#'   should be generated. Variable names should match those used in model fitting.
#' @param pred.type Character string specifying the type of predictions to generate.
#'   Options depend on the model type (e.g., "response", "link" for glm models).
#'   Default is "response".
#' @param report.se Logical. Should standard errors be reported alongside predictions?
#'   Not all model types support this feature. Default is FALSE.
#'
#' @return A data frame with at least two columns:
#'   \item{prediction}{The predicted values}
#'   \item{model}{A character string identifying the model type or name}
#'
#'   For mixed models when \code{re = TRUE}, may return both fixed and random
#'   effect predictions. Some methods may include additional columns such as
#'   standard errors when \code{report.se = TRUE}.
#'
#' @details
#' This generic function provides a consistent interface for generating predictions
#' across different model types. Each model class should have its own method
#' that handles the specifics of prediction for that model type.
#'
#' Currently supported model types include:
#' \itemize{
#'   \item Linear models (lm)
#'   \item Generalized linear models (glm) 
#'   \item Mixed-effects models (lmerMod, glmerMod from lme4)
#'   \item Ordinal regression models (polr from MASS)
#'   \item Random forest models (RandomForest from party)
#'   \item Regression trees (rpart)
#'   \item Neural networks (keras models via flex_nn package)
#' }
#'
#' @seealso \code{\link{compare.fits}} for the main function that uses this generic
#'
#' @examples
#' # Basic usage with linear model
#' data(mtcars)
#' model = lm(mpg ~ hp + wt, data = mtcars)
#' pred_data = data.frame(hp = c(100, 150, 200), wt = c(2.5, 3.0, 3.5))
#' predictions = generate_predictions(model, re = FALSE, pred_data, "response", FALSE)
#' head(predictions)
#'
#' # With mixed model (if lme4 available)
#' \dontrun{
#' library(lme4)
#' data(sleepstudy)
#' mixed_model = lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
#' pred_data = data.frame(Days = c(0, 5, 9), Subject = c("308", "309", "310"))
#' 
#' # Fixed effects only
#' fixed_preds = generate_predictions(mixed_model, re = FALSE, pred_data, 
#'                                   "response", FALSE)
#' 
#' # Both fixed and random effects
#' both_preds = generate_predictions(mixed_model, re = TRUE, pred_data, 
#'                                  "response", FALSE)
#' }
#'
#' @export
generate_predictions = function(model, re, pred.values, pred.type, report.se) {
  UseMethod("generate_predictions")
}
#' Generate Predictions for Mixed-Effects Models
#'
#' S3 method for generating predictions from lmerMod objects (linear mixed-effects models).
#' Handles both fixed-effects-only and combined fixed/random effects predictions.
#'
#' @param model A fitted lmerMod object from the lme4 package
#' @param re Logical. If FALSE, returns only fixed effects predictions. If TRUE,
#'   returns both fixed and random effects predictions.
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (ignored for lmerMod)
#' @param report.se Logical. Whether to report standard errors (currently not implemented)
#'
#' @return Data frame with columns "prediction" and "model". When re=TRUE, returns
#'   both random and fixed effects predictions with appropriate model labels.
#'
#' @method generate_predictions lmerMod
#' @export
generate_predictions.lmerMod = function(model, re, pred.values, pred.type, report.se) {
  if (!re) {
    return(data.frame(prediction = 
                        predict(model, pred.values, type="response", re.form=NA), 
                      model="fixed effects"))
  } else {
    random_effects = data.frame(prediction = predict(model, pred.values, type="response", re.form=NULL), 
                                model="random effects")
    fixed_effects = data.frame(prediction = predict(model, pred.values, type="response", re.form=NA), 
                               model="fixed effects")
    return(rbind(random_effects, fixed_effects))
  }
}

#' Generate Predictions for Generalized Linear Mixed-Effects Models
#'
#' S3 method for generating predictions from glmerMod objects. Uses the same logic
#' as lmerMod models for handling fixed and random effects.
#'
#' @param model A fitted glmerMod object from the lme4 package
#' @param re Logical. If FALSE, returns only fixed effects predictions. If TRUE,
#'   returns both fixed and random effects predictions.
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (ignored for glmerMod)
#' @param report.se Logical. Whether to report standard errors (currently not implemented)
#'
#' @return Data frame with columns "prediction" and "model". When re=TRUE, returns
#'   both random and fixed effects predictions with appropriate model labels.
#'
#' @method generate_predictions glmerMod
#' @export
generate_predictions.glmerMod = generate_predictions.lmerMod  # Same logic

#' Generate Predictions for Ordinal Regression Models
#'
#' S3 method for generating predictions from polr objects (proportional odds
#' logistic regression models from the MASS package).
#'
#' @param model A fitted polr object from the MASS package
#' @param re Logical. Random effects parameter (ignored for polr models)
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (ignored, uses "class")
#' @param report.se Logical. Whether to report standard errors (currently not implemented)
#'
#' @return Data frame with columns "prediction" and "model", where predictions
#'   are the predicted classes from the ordinal regression model.
#'
#' @method generate_predictions polr
#' @export
generate_predictions.polr = function(model, re, pred.values, pred.type, report.se) {
  return(data.frame(prediction = predict(model, pred.values, type="class"), 
                    model = "polr"))
}

#' Generate Predictions for Random Forest Models
#'
#' S3 method for generating predictions from RandomForest objects (from the party package).
#' Handles class consistency between training data and prediction data.
#'
#' @param model A fitted RandomForest object from the party package
#' @param re Logical. Random effects parameter (ignored for RandomForest models)
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (ignored for RandomForest)
#' @param report.se Logical. Whether to report standard errors (currently not supported)
#'
#' @return Data frame with columns "prediction" and "model" containing the
#'   RandomForest predictions.
#'
#' @details This method ensures that factor variables in the prediction data
#'   have the same levels and ordering as those used during model training.
#'
#' @method generate_predictions RandomForest
#' @export
generate_predictions.RandomForest = function(model, re, pred.values, pred.type, report.se) {
  response = attr(model, "data")@get("response")
  outcome = attr(model, "data")@get("input")
  data = cbind(response, outcome)
  # check if classes differ from old to new, and correct if they are
  class_preds = lapply(pred.values, class)
  class_data = lapply(data[names(pred.values)], class)
  if (!identical(class_preds, class_data)) {
    for (i in 1:length(class_preds)) {
      
      if ("factor" %in% (class(data[,names(pred.values[i])]))) {
        
        ordered = ifelse("ordered" %in% (class(data[,names(pred.values[i])])), T, F) 
        pred.values[,i] = factor(pred.values[,i], levels=levels(data[,names(pred.values[i])]), ordered=ordered)
      } else {
        class(pred.values[,i]) = class(data[,names(pred.values[i])])
      }  
    }
  }
  
  prediction = predict(model, newdata=pred.values, OOB = TRUE)
  d = data.frame(prediction = prediction, model="RandomForest")
  names(d)[1] = "prediction"
  return(d)  
}

#' Generate Predictions for Regression Tree Models
#'
#' S3 method for generating predictions from rpart objects (recursive partitioning
#' and regression trees).
#'
#' @param model A fitted rpart object
#' @param re Logical. Random effects parameter (ignored for rpart models)
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (ignored for rpart)
#' @param report.se Logical. Whether to report standard errors (currently not supported)
#'
#' @return Data frame with columns "prediction" and "model" containing the
#'   regression tree predictions.
#'
#' @method generate_predictions rpart
#' @export
generate_predictions.rpart = function(model, re, pred.values, pred.type, report.se) {
  return(data.frame(prediction = predict(model, pred.values), 
                    model = "rpart"))
}

#' Generate Predictions for General Model Objects
#'
#' Default S3 method for generating predictions from model objects. This method
#' handles standard model types like lm and glm that have consistent predict() interfaces.
#'
#' @param model A fitted model object with a predict() method
#' @param re Logical. Random effects parameter (ignored for most model types)
#' @param pred.values Data frame containing predictor values for prediction
#' @param pred.type Character string specifying prediction type (e.g., "response", "link")
#' @param report.se Logical. Whether to report standard errors via confidence intervals
#'
#' @return Data frame with columns "prediction" and "model". When report.se=TRUE,
#'   may include additional columns for confidence intervals (depending on the model type).
#'
#' @details This default method works for most standard R model objects including
#'   lm, glm, and other models that follow standard predict() conventions.
#'
#' @method generate_predictions default
#' @export
generate_predictions.default = function(model, re, pred.values, pred.type, report.se) {
  int = ifelse(report.se, "confidence", "none")
  return(data.frame(prediction = predict(model, pred.values, interval=int, type=pred.type),
                    model = class(model)[1]))
}
# get_fitted ----------------------------------------------------


#' @export
get_fitted = function(model, re, pred.values=NULL, pred.type, report.se) {
  UseMethod("get_fitted")
}


#' @method get_fitted default
#' @export
get_fitted.default = function(model, re=NULL, pred.values=NULL, pred.type="response", report.se=FALSE) {
  
  if (is.null(pred.values)) pred.values = extract_data_from_fitted_object(model)
  int = ifelse(report.se, "confidence", "none")
  return(data.frame(prediction = predict(model, pred.values, interval=int, type=pred.type),
                    model = class(model)[1]))
}

#' @method get_fitted lmerMod
#' @export
get_fitted.lmerMod = function(model, re=FALSE, pred.values, pred.type="response", report.se=FALSE) {
  if (!re) {
    return(data.frame(prediction = 
                        predict(model, type="response", re.form=NA), 
                      model="fixed effects"))
  } else {
    random_effects = data.frame(prediction = predict(model, type="response", re.form=NULL), 
                                model="random effects")
    fixed_effects = data.frame(prediction = predict(model, type="response", re.form=NA), 
                               model="fixed effects")
    return(rbind(random_effects, fixed_effects))
  }
}

#' @method get_fitted glmerMod
#' @export
get_fitted.glmerMod = get_fitted.lmerMod  # Same logic

#' @method get_fitted polr
#' @export
get_fitted.polr = function(model, re=NULL, pred.values, pred.type="class", report.se=F) {
  return(data.frame(prediction = predict(model, pred.values, type="class"), 
                    model = "polr"))
}

#' @method get_fitted RandomForest
#' @export
get_fitted.RandomForest = function(model, re=NULL, pred.values, pred.type=NULL, report.se=F) {
  response = attr(model, "data")@get("response")
  outcome = attr(model, "data")@get("input")
  data = cbind(response, outcome)
  # check if classes differ from old to new, and correct if they are
  class_preds = lapply(pred.values, class)
  class_data = lapply(data[names(pred.values)], class)
  if (!identical(class_preds, class_data)) {
    for (i in 1:length(class_preds)) {
      
      if ("factor" %in% (class(data[,names(pred.values[i])]))) {
        
        ordered = ifelse("ordered" %in% (class(data[,names(pred.values[i])])), T, F) 
        pred.values[,i] = factor(pred.values[,i], levels=levels(data[,names(pred.values[i])]), ordered=ordered)
      } else {
        class(pred.values[,i]) = class(data[,names(pred.values[i])])
      }  
    }
  }
  
  prediction = predict(model, newdata=pred.values, OOB = TRUE)
  d = data.frame(prediction = prediction, model="RandomForest")
  names(d)[1] = "prediction"
  return(d)  
}

#' @method get_fitted rpart
#' @export
get_fitted.rpart = function(model, re, pred.values, pred.type, report.se) {
  return(data.frame(prediction = predict(model, pred.values), 
                    model = "rpart"))
}



# prepare_data_for_compare_fits -------------------------------------------


#' @export
prepare_data_for_compare_fits = function(data, model1, formula, all_variables) {
  UseMethod("prepare_data_for_compare_fits")
}

#' @method prepare_data_for_compare_fits default
#' @export
prepare_data_for_compare_fits.default = function(data, model1, formula, all_variables) {
  return(data)
}

#' @method prepare_data_for_compare_fits lmerMod
#' @export
prepare_data_for_compare_fits.lmerMod = function(data, model1, formula, clusters=NULL) {
  #### convert random effects to factors for mixed models
  data = subset_random_model(d=data, object = model1, formula=formula, samp.size = clusters)
  return(data)
}



# get_plotted_line ---------------------------------------------------------

#' @export
get_plotted_line = function(model, data, formula, ...) {
  UseMethod("get_plotted_line")
}


#' @method get_plotted_line default
#' @export
get_plotted_line.default = function(model, data, formula,  ...) {

  ## extract variable slots
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  x_variable_named = predictors[1]
  
  # 1. Generate predictions for the entire dataset
  k = data %>%
    bin_if_theres_a_flexplot_formula(formula=formula, ...) %>%
    # 2. Generate "bins" for both x-axis variable and already-binned variables
    get_sequence_of_target_variable(x_label=x_variable_named)
  
  # collect variables that are "binned"
  binned_vars = grep("_binned", names(k), value=T)
  x_var       = grep("_sequence", names(k), value=T)
  
  # aggregate across those variables
  plotted_variables = c(not_binned_but_plotted(predictors, binned_vars), "model")
  n = k %>%
    # replace x variable with _sequence 
    dplyr::mutate(!!sym(x_variable_named) := .data[[x_var]]) %>%
    dplyr::select(-all_of(x_var)) %>%
    group_by(across(all_of(plotted_variables))) %>%
    summarize(prediction = mean(prediction, na.rm=T), .groups = "drop") %>%
    #convert all *_binned factors like "4.2-4.9" to numeric midpoints
    mutate(across(ends_with("_binned"), ~ {
      x = as.character(.x)
      # remove parentheses if present
      x = gsub("[()]", "", x)
      low  = as.numeric(sub("^(-?[0-9.]+)-.*$", "\\1", x))
      high = as.numeric(sub("^.*-(-?[0-9.]+)$", "\\1", x))
      (low + high) / 2
    })) %>%
      # strip the "_binned" suffix from their names
      rename_with(~ sub("_binned$", "", .x), ends_with("_binned"))
  n
}

#' @method get_plotted_line polr
#' @export
get_plotted_line.polr = function(model, data, formula,  ...) {
  
  # convert prediction to numeric
  data$prediction = as.numeric(as.character(data$prediction))
  
  get_plotted_line.default(model, data, formula, ...)
}

get_sequence_of_target_variable = function(data, x_label) {
  
  data[,paste0(x_label, "_sequence")] = data[,x_label]
  x_variable = data[,x_label]
  if (is.factor(x_variable)) return(data)
  if (!is.numeric(x_variable)) return(data)
  
  unique_values = length(unique(x_variable))
  if (unique_values < 10) return(data)
  
  bins = quantile(x_variable, probs = seq(from=0, to=1, length.out=10)) %>% unique
  new_labels = (diff(bins)/2)+bins[-length(bins)]
  new_labels = new_labels
  data[,paste0(x_label, "_sequence")] = cut(x_variable, breaks=bins, include.lowest=T, new_labels) %>% as.character %>% as.numeric
  return(data)
  
}

# post_prediction_process_cf


# post_prediction_process_cf ----------------------------------------------

#' @export
post_prediction_process_cf        = function(model1, model2=NULL, predictions, formula, re=FALSE, k, pred.type,  ...) {
  UseMethod("post_prediction_process_cf")
}

#' @method post_prediction_process_cf default
#' @export
post_prediction_process_cf.default = function(model1, model2=NULL, predictions, formula, re=FALSE, k, pred.type, ...) {
  predictions$model = deparse(substitute(model1))
  both_models_identical = identical(model1, model2)
  if (!both_models_identical) { 
    predictions2 = get_fitted(model2, re=re, pred.values=k, pred.type=pred.type, report.se=F) 
    predictions2$model = deparse(substitute(model2))
    predictions  = setNames(predictions , c("prediction", "model"))
    predictions2 = setNames(predictions2, c("prediction", "model"))
    predictions = rbind(predictions, predictions2)
    k = rbind(k, k)
  }
  k$prediction = predictions$prediction
  k$model      = predictions$model
  
  # get predicted values
  pred.values = get_plotted_line(data=k, formula=formula, model=model1, ...)
  
  # for intercept only models
  if (nrow(pred.values)==0) pred.values = data.frame("(Intercept)" = 1)
  
  # if they provide two models AND re=T, return just the random effects
  # if (re & !exists("runme")) {
  #   pred.mod1 = pred.mod1[pred.mod1$model == "random effects",]
  #   pred.mod2 = pred.mod2[pred.mod2$model == "random effects",]
  #   pred.mod1$model = deparse(substitute(model1))
  #   pred.mod2$model = deparse(substitute(model2))
  # }
  
  prediction.model = pred.values
}

#' @method post_prediction_process_cf lmerMod 
#' @export
post_prediction_process_cf.lmerMod  = function(model1, model2=NULL, predictions, formula, re=FALSE, k, pred.type, ...) {
  
  # generate predictions for other model
  both_models_identical = identical(model1, model2)
  if (!both_models_identical) { 
    predictions2 = get_fitted(model2, re=re, pred.type=pred.type, report.se=report.se) 
  } else {
    predictions2 = predictions
  }
  
  # if they provide two models AND re=T, return just the random effects
  if (re & !both_models_identical) {
    predictions  =  predictions[ predictions$model == "random effects",]
    predictions2 = predictions2[predictions2$model == "random effects",]
  } 
  
  # rename model if they don't have re
  if (!re | (re & !both_models_identical)) {
    predictions$model = deparse(substitute(model1))
    predictions2$model = deparse(substitute(model2))
  }
  
  
  predictions = rbind(predictions, predictions2)
  k = rbind(k, k)
  k$prediction = predictions$prediction
  k$model      = predictions$model
  # get rid of duplicates (happens when they don't provide a second model)
  k = k[!duplicated(k),]
  # get predicted values
  pred.values = get_plotted_line(data=k, formula=formula, model=model1, ...)
  
  # for intercept only models
  if (nrow(pred.values)==0) pred.values = data.frame("(Intercept)" = 1)
  
  return(pred.values)
}




