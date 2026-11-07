#' Generate Predictions from Model Objects
#'
#' This is a generic function for generating predictions from various types of
#' model objects. It is primarily used internally by \code{\link{compare_fits}}
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
#'   \item Neural-network models require a compatible adapter (not supplied here)
#' }
#'
#' @seealso \code{\link{compare_fits}} for the main function that uses this generic
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
  # Observed prediction frames may contain columns not used by this forest.
  variables = intersect(names(pred.values), names(data))
  for (variable in variables) {
    if (is.factor(data[[variable]])) {
      pred.values[[variable]] = factor(pred.values[[variable]],
                                       levels=levels(data[[variable]]),
                                       ordered=is.ordered(data[[variable]]))
    } else if (identical(class(data[[variable]]), "integer") &&
               is.numeric(pred.values[[variable]])) {
      # modeltools checks declared classes, although party uses double-valued
      # inputs internally. Restore the class without coercing fractional profiles.
      attr(pred.values[[variable]], "class") = "integer"
    }
  }
  
  response = response[[1]]
  if (is.factor(response) && nlevels(droplevels(response)) == 2 && pred.type == "response") {
    probabilities = predict(model, newdata=pred.values, type="prob", OOB=FALSE)
    positive = match(levels(droplevels(response))[2], levels(response))
    prediction = if (is.list(probabilities)) {
      vapply(probabilities, `[`, numeric(1), positive)
    } else {
      probabilities[, positive]
    }
  } else {
    prediction = predict(model, newdata=pred.values, OOB=FALSE)
  }
  d = data.frame(prediction = prediction, model="RandomForest")
  names(d)[1] = "prediction"
  return(d)  
}

#' @rdname generate_predictions
#' @method generate_predictions randomForest
#' @export
generate_predictions.randomForest = function(model, re, pred.values, pred.type, report.se) {
  if (model$type == "classification") {
    prediction = predict(model, newdata=pred.values, type="prob")
    if (ncol(prediction) != 2) {
      stop("Multicategory forest probabilities are not supported by compare_fits().", call.=FALSE)
    }
    prediction = prediction[, 2]
  } else {
    prediction = predict(model, newdata=pred.values, type="response")
  }
  data.frame(prediction=prediction, model=class(model)[1])
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
  type = if (model$method == "class") "prob" else "vector"
  prediction = predict(model, newdata=pred.values, type=type)
  if (is.matrix(prediction)) {
    if (ncol(prediction) != 2) {
      stop("Multicategory tree probabilities are not supported by compare_fits().", call.=FALSE)
    }
    prediction = prediction[, 2]
  }
  data.frame(prediction=prediction, model="rpart")
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


#' Extract predictions from a fitted model
#'
#' Recover the model's estimation data when prediction data are not supplied,
#' then dispatch to \code{\link{generate_predictions}}. No model is refitted.
#' @param model A fitted model supported by the prediction adapters.
#' @param re Include random-effects predictions for mixed models?
#' @param pred.values Optional data frame of prediction coordinates. If
#'   \code{NULL}, recover the original estimation data from the model.
#' @param pred.type Prediction scale forwarded to the model's prediction method.
#' @param report.se Request uncertainty information from the prediction adapter?
#' @return A data frame with \code{prediction} and \code{model} columns, and
#'   additional uncertainty columns when supported and requested.
#' @seealso \code{\link{compare_fits}}
#' @export
get_fitted = function(model, re, pred.values=NULL, pred.type, report.se) {
  UseMethod("get_fitted")
}


#' @method get_fitted default
#' @export
get_fitted.default = function(model, re=FALSE, pred.values=NULL, pred.type="response", report.se=FALSE) {
  if (is.null(pred.values)) pred.values = extract_data_from_fitted_object(model)
  generate_predictions(model, re, pred.values, pred.type, report.se)
}

#' @method get_fitted lmerMod
#' @export
get_fitted.lmerMod = get_fitted.default

#' @method get_fitted glmerMod
#' @export
get_fitted.glmerMod = get_fitted.default

#' @method get_fitted polr
#' @export
get_fitted.polr = get_fitted.default

#' @method get_fitted RandomForest
#' @export
get_fitted.RandomForest = get_fitted.default

#' @method get_fitted rpart
#' @export
get_fitted.rpart = get_fitted.default


# prepare_data_for_compare_fits -------------------------------------------


#' Prepare comparison data for a fitted model
#'
#' The default method returns the input data. Mixed-model methods select
#' clusters and prepare their grouping variable using the existing stratified
#' cluster-sampling convention.
#' @param data Data used for plotting and prediction.
#' @param model1 A fitted model; S3 dispatch is on this argument.
#' @param formula A flexplot formula specifying the displayed variables.
#' @param all_variables Retained for compatibility; currently unused.
#' @param clusters Number of mixed-model clusters to display.
#' @return The prepared data frame.
#' @seealso \code{\link{compare_fits}}
#' @export
prepare_data_for_compare_fits = function(data, model1, formula, all_variables=NULL, clusters=3) {
  UseMethod("prepare_data_for_compare_fits", model1)
}

#' @method prepare_data_for_compare_fits default
#' @export
prepare_data_for_compare_fits.default = function(data, model1, formula, all_variables=NULL, clusters=3) {
  return(data)
}

#' @method prepare_data_for_compare_fits lmerMod
#' @export
prepare_data_for_compare_fits.lmerMod = function(data, model1, formula, all_variables=NULL, clusters=3) {
  #### convert random effects to factors for mixed models
  data = subset_random_model(d=data, object = model1, formula=formula, samp.size = clusters)
  return(data)
}



#' @method prepare_data_for_compare_fits glmerMod
#' @export
prepare_data_for_compare_fits.glmerMod = prepare_data_for_compare_fits.lmerMod

# get_plotted_line ---------------------------------------------------------

#' Summarize predictions at displayed coordinates
#'
#' Average predictions within displayed groups, models, and conditioning bins.
#' Numeric x values with at least ten distinct values use quantile-bin midpoints.
#' This helper supplies observed conditional summaries, rather than marginal
#' standardization.
#' @param model A fitted model used for S3 dispatch.
#' @param data Data containing displayed variables, \code{prediction}, and
#'   \code{model} columns.
#' @param formula A flexplot formula specifying displayed variables.
#' @param ... Display-binning arguments forwarded to the binning helpers.
#' @return A data frame of plotted coordinates and averaged predictions, with
#'   an \code{observation_groups} attribute identifying the contributing rows.
#' @seealso \code{\link{compare_fits}}
#' @export
get_plotted_line = function(model, data, formula, ...) {
  UseMethod("get_plotted_line")
}


#' @method get_plotted_line default
#' @export
get_plotted_line.default = function(model, data, formula, ...) {
  variables = flexplot_axis_given(formula)
  x = variables$axis[1]
  displayed = all.vars(formula)[-1]
  binned = reproduce_breaks(data, formula, list(...))
  frame = binned$binned_data
  bin_variables = names(binned$breaks)
  bin_variables = bin_variables[paste0(bin_variables, "_binned") %in% names(frame)]
  keys = if (length(displayed)) {
    not_binned_but_plotted(displayed, paste0(bin_variables, "_binned"))
  } else character(0)
  if (is.numeric(frame[[x]])) {
    frame = get_sequence_of_target_variable(frame, x)
    frame[[x]] = frame[[paste0(x, "_sequence")]]
  }
  keys = c(keys, "model")
  grouped = dplyr::group_by(frame, dplyr::across(dplyr::all_of(keys)), .drop=TRUE)
  result = dplyr::summarise(
    grouped, prediction=mean(.data$prediction),
    dplyr::across(dplyr::all_of(bin_variables), mean), .groups="drop"
  )
  result = as.data.frame(result)
  attr(result, "observation_groups") = dplyr::group_rows(grouped)
  result
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

#' Combine prediction streams for a comparison
#'
#' A low-level compatibility helper for combining model prediction streams and
#' reducing them to plotted coordinates. Use \code{\link{compare_fits}} for
#' ordinary model comparisons.
#' @param model1 A fitted model used for S3 dispatch.
#' @param model2 An optional second fitted model.
#' @param predictions Prediction data for the first model.
#' @param formula A flexplot formula specifying displayed variables.
#' @param re Include random-effects predictions?
#' @param k Prediction coordinates aligned with the prediction rows.
#' @param pred.type Prediction scale forwarded to prediction helpers.
#' @param ... Arguments forwarded to prediction-summary helpers.
#' @return A data frame of plotted prediction summaries.
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
    predictions2 = get_fitted(model2, re=re, pred.values=k, pred.type=pred.type)
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

# Marginal standardization: one shared reference distribution for all curves.
# The same reference rows are used at every plotted coordinate and for both models.
compare_fits_marginal = function(models, data, formula, re, pred.type, model_labels,
                                 single_model, report.se, num_points=50, ...) {
  if (length(num_points) != 1 || !is.finite(num_points) || num_points < 2 ||
      num_points != as.integer(num_points)) {
    stop("num_points must be an integer >= 2.", call.=FALSE)
  }
  displayed = all.vars(formula)[-1]
  # No displayed predictors means one marginal estimate, with no axis columns.
  grid = data.frame(row.names=1L)
  if (length(displayed)) {
    # Existing grid generator establishes conditioning/moderator values and levels.
    grid = generate_predictors(data, formula, models[[1]], ...)
    if (!all(displayed %in% names(grid))) {
      stop("Prediction grid is missing displayed variables.", call.=FALSE)
    }
    grid = unique(grid[displayed])
  }
  axis = flexplot_axis_given(formula)$axis[1]
  if (axis %in% displayed && is.numeric(data[[axis]]) &&
      length(unique(data[[axis]])) >= 10) {
    x_values = seq(min(data[[axis]]), max(data[[axis]]), length.out=num_points)
    moderators = setdiff(displayed, axis)
    if (length(moderators)) {
      grid = unique(grid[moderators])
      grid = merge(data.frame(.marginal_x=x_values), grid, by=NULL)
    } else {
      grid = data.frame(.marginal_x=x_values)
    }
    grid[[axis]] = grid$.marginal_x
    grid$.marginal_x = NULL
    grid = grid[displayed]
  }

  # Randomly sample the reference distribution
  n_ref = min(300L, nrow(data))
  ref_rows = sample.int(nrow(data), size=n_ref, replace=FALSE)
  reference = data[ref_rows, , drop=FALSE]
  outputs = list()
  for (i in seq_along(models)) {
    model = models[[i]]
    if (report.se && (!inherits(model, "lm") || inherits(model, "rlm"))) {
      stop("Marginal report.se=TRUE currently supports lm/glm only.", call.=FALSE)
    }
    for (j in seq_len(nrow(grid))) {
      newdata = reference
      for (variable in displayed) newdata[[variable]] = rep(grid[[variable]][j], nrow(reference))
      rownames(newdata) = as.character(seq_len(nrow(newdata)))
      frame = compare_fits_prediction_frame(model, newdata, re, pred.type,
                                            model_labels[i], single_model, "reference")
      for (type in unique(frame$model)) {
        values = frame$prediction[frame$model == type]
        result = grid[j, , drop=FALSE]
        result$model = type
        result$prediction = mean(values)
        if (report.se) {
          gradient = compare_fits_gradient(model, newdata, pred.type)
          avg = colMeans(gradient)
          result$se = sqrt(drop(avg %*% stats::vcov(model) %*% avg))
          result = compare_fits_intervals(result, model)
        }
        outputs[[length(outputs)+1L]] = result
      }
    }
  }
  result = do.call(rbind, outputs)
  rownames(result) = NULL
  result
}
