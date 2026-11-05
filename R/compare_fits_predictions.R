# Inspect expressions, rather than variable names, to establish response scales.
compare_fits_model_response = function(model, model_name) {
  # party forests store separate one-sided response and input formulas.
  response_formula = tryCatch(
    if (inherits(model, "RandomForest")) attr(model, "data")@formula$response else
      stats::formula(model),
    error=identity
  )
  expected_length = if (inherits(model, "RandomForest")) 2L else 3L
  if (!is.call(response_formula) || length(response_formula) != expected_length ||
      !identical(response_formula[[1]], as.name("~"))) {
    stop("Cannot inspect the fitted response expression for ", model_name,
         " (", class(model)[1], "); its response scale cannot be verified.", call.=FALSE)
  }
  response = response_formula[[2]]
  if (!is.symbol(response)) {
    stop("Transformed model responses are unsupported in compare_fits(): ",
         model_name, " uses response '", paste(deparse(response), collapse=" "),
         "'. Predictions may be on a different scale from the plotted outcome. ",
         "Use models with the same untransformed response variable.", call.=FALSE)
  }
  as.character(response)
}

# Identify estimation rows before generating predictions. Row names identify the
# observations; stored model-frame values verify that those identities match.
compare_fits_model_terms = function(model) {
  terms = tryCatch(get_terms(model), error=identity)
  if (inherits(terms, "error") || length(terms$response) != 1) {
    stop("Cannot recover model variables for class '", class(model)[1],
         "'. A compatible model/formula adapter is required.", call.=FALSE)
  }
  terms
}

compare_fits_plain_expression = function(expression) {
  if (!is.call(expression)) return(TRUE)
  if (!as.character(expression[[1]])[1] %in% c("~", "|", "+", "(")) return(FALSE)
  all(vapply(as.list(expression)[-1], compare_fits_plain_expression, logical(1)))
}

compare_fits_model_frame = function(model) {
  if (inherits(model, "rpart")) {
    frame = stats::model.frame(stats::terms(model),
                                data=extract_data_from_fitted_object(model),
                                na.action=stats::na.pass)
    return(frame[match(names(model$where), rownames(frame)), , drop=FALSE])
  }
  if (inherits(model, "RandomForest")) {
    return(cbind(attr(model, "data")@get("response"), attr(model, "data")@get("input")))
  }
  if (inherits(model, "randomForest")) {
    frame = extract_data_from_fitted_object(model)
    ids = names(model$predicted)
    if (is.null(ids)) stop("Cannot identify the random forest estimation rows.", call.=FALSE)
    return(frame[match(ids, rownames(frame)), , drop=FALSE])
  }
  frame = tryCatch(stats::model.frame(model), error=identity)
  if (inherits(frame, "error") || !is.data.frame(frame)) {
    stop("Cannot reliably identify estimation observations for model class '",
         class(model)[1], "'. Supply a model with a recoverable model frame.", call.=FALSE)
  }
  frame
}

compare_fits_matching_rows = function(model, data) {
  frame = compare_fits_model_frame(model)
  ids = rownames(frame)
  if (is.null(ids) || anyDuplicated(ids)) {
    stop("Cannot identify unique estimation observations for this model.", call.=FALSE)
  }
  matched = match(rownames(data), ids)
  present = which(!is.na(matched))
  if (length(present) == 0) return(rep(FALSE, nrow(data)))

  if (inherits(model, c("RandomForest", "randomForest", "merMod"))) {
    candidate = data
  } else {
    candidate = stats::model.frame(stats::terms(model), data=data, na.action=stats::na.pass)
  }
  shared = intersect(names(frame), names(candidate))
  if (length(shared) == 0) {
    stop("Cannot verify observation identities against the model's estimation data.", call.=FALSE)
  }
  for (variable in shared) {
    original = frame[[variable]][matched[present]]
    supplied = candidate[[variable]][present]
    if (is.matrix(frame[[variable]])) {
      original = frame[[variable]][matched[present], , drop=FALSE]
      supplied = candidate[[variable]][present, , drop=FALSE]
    }
    if (!isTRUE(all.equal(as.character(original), as.character(supplied)))) {
      stop("Cannot align model observations with 'data': row identities or values differ. ",
           "Supply the original observations with their original row names.", call.=FALSE)
    }
  }
  !is.na(matched)
}

compare_fits_sample = function(data, models, variables) {
  missing = setdiff(variables, names(data))
  if (length(missing)) stop("Variables missing from 'data': ", paste(missing, collapse=", "), call.=FALSE)
  eligible = stats::complete.cases(data[variables])
  for (model in models) eligible = eligible & compare_fits_matching_rows(model, data)
  data = data[eligible, , drop=FALSE]
  if (nrow(data) == 0) {
    stop("The models have no common eligible estimation observations in 'data'.", call.=FALSE)
  }
  data
}

compare_fits_numeric_predictions = function(values) {
  if (is.factor(values) || is.character(values)) {
    converted = suppressWarnings(as.numeric(as.character(values)))
    if (anyNA(converted)) {
      stop("Averaging predicted category labels is unsupported. Use a model/prediction type ",
           "that returns numeric responses or binary probabilities.", call.=FALSE)
    }
    values = converted
  }
  if (!is.numeric(values) || is.matrix(values) || any(!is.finite(values))) {
    stop("The prediction method must return one finite numeric prediction per observation.", call.=FALSE)
  }
  unname(values)
}

compare_fits_prediction_frame = function(model, data, re, pred.type, label, single_model, method) {
  predicted = if (method == "observed") {
    get_fitted(model, re=re, pred.values=data, pred.type=pred.type, report.se=FALSE)
  } else {
    generate_predictions(model, re=re, pred.values=data, pred.type=pred.type, report.se=FALSE)
  }
  if (!all(c("prediction", "model") %in% names(predicted))) {
    stop("The model's prediction method did not return scalar 'prediction' and 'model' columns.", call.=FALSE)
  }
  if (!inherits(model, "merMod") && .row_names_info(predicted, type=1) > 0 &&
      !identical(rownames(predicted), rownames(data))) {
    stop("Prediction row identities do not match the supplied observations; refusing to reorder rows.", call.=FALSE)
  }
  if (re && !single_model && inherits(model, "merMod")) {
    predicted = predicted[predicted$model == "random effects", , drop=FALSE]
  }
  frames = list()
  for (type in unique(as.character(predicted$model))) {
    block = predicted[as.character(predicted$model) == type, , drop=FALSE]
    if (nrow(block) != nrow(data)) {
      stop("Predictions do not align with the supplied observations; refusing to recycle rows.", call.=FALSE)
    }
    frame = data
    frame$prediction = compare_fits_numeric_predictions(block$prediction)
    frame$model = if (re && single_model && inherits(model, "merMod")) type else label
    frames[[type]] = frame
  }
  do.call(rbind, frames)
}

compare_fits_observed = function(model, data, formula, re, pred.type, label,
                                  single_model, report.se, ...) {
  frame = compare_fits_prediction_frame(model, data, re, pred.type, label, single_model, "observed")
  result = get_plotted_line(model, frame, formula, ...)
  if (!report.se) return(result)
  compare_fits_summary_se(result, model, data, pred.type)
}

compare_fits_reference = function(models, data, formula, re, pred.type, labels,
                                   single_model, report.se, ...) {
  grid = generate_predictors(data, formula, models[[1]], ...)
  for (model in models) {
    missing = setdiff(get_terms(model)$predictors, names(grid))
    for (variable in missing) {
      grid[[variable]] = return_constant_for_predicted_data(variable, data, model)
    }
  }
  grid = unique(grid)
  frames = list()
  for (i in seq_along(models)) {
    frame = compare_fits_prediction_frame(models[[i]], grid, re, pred.type,
                                          labels[i], single_model, "reference")
    if (report.se) frame = compare_fits_reference_se(frame, models[[i]], pred.type)
    frames[[i]] = frame
  }
  do.call(rbind, frames)
}

# Delta-method uncertainty for an average fitted response, using coefficient
# covariance rather than averaging individual prediction standard errors.
compare_fits_gradient = function(model, data, pred.type) {
  if (!inherits(model, "lm") || inherits(model, "rlm") ||
      !pred.type %in% c("response", "link")) {
    stop("report.se=TRUE is supported for lm and glm response/link predictions only.", call.=FALSE)
  }
  matrix = stats::model.matrix(stats::delete.response(stats::terms(model)), data,
                                contrasts.arg=model$contrasts, xlev=model$xlevels)
  if (inherits(model, "glm") && pred.type == "response") {
    eta = stats::predict(model, newdata=data, type="link")
    matrix = matrix * model$family$mu.eta(eta)
  }
  matrix
}

compare_fits_summary_se = function(result, model, data, pred.type) {
  gradient = compare_fits_gradient(model, data, pred.type)
  groups = attr(result, "observation_groups")
  variance = stats::vcov(model)
  result$se = numeric(nrow(result))
  for (i in seq_along(groups)) {
    rows = groups[[i]]
    # Each mixed prediction stream would repeat the observation frame, but
    # uncertainty is explicitly restricted to lm/glm above.
    average_gradient = colMeans(gradient[rows, , drop=FALSE])
    result$se[i] = sqrt(drop(average_gradient %*% variance %*% average_gradient))
  }
  compare_fits_intervals(result, model)
}

compare_fits_reference_se = function(frame, model, pred.type) {
  gradient = compare_fits_gradient(model, frame, pred.type)
  frame$se = sqrt(rowSums((gradient %*% stats::vcov(model)) * gradient))
  compare_fits_intervals(frame, model)
}

compare_fits_intervals = function(prediction, model) {
  critical = if (inherits(model, "glm")) stats::qnorm(.975) else
    stats::qt(.975, df=stats::df.residual(model))
  prediction$prediction.fit = prediction$prediction
  prediction$prediction.lwr = prediction$prediction - critical * prediction$se
  prediction$prediction.upr = prediction$prediction + critical * prediction$se
  prediction
}
