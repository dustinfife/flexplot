
# get_fitted ----------------------------------------------------


#' @export
get_fitted = function(model, re, pred.values, pred.type, report.se) {
  UseMethod("get_fitted")
}


#' @method get_fitted default
#' @export
get_fitted.default = function(model, re=NULL, pred.values, pred.type="response", report.se=FALSE) {
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
  new_labels = (diff(bins)/2)+bins
  new_labels = new_labels[-length(new_labels)]
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
    predictions2 = get_fitted(model2, re=re, pred.type=pred.type, report.se=F) 
    predictions2$model = deparse(substitute(model2))
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




