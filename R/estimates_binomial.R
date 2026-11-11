# Estimation rows in original predictor coordinates, including transformed terms.
logistic_estimation_data = function(model) {
  data = stats::model.frame(model)
  variables = all.vars(stats::delete.response(stats::terms(model)))
  if (all(variables %in% names(data))) return(data)
  source = tryCatch(eval(stats::getCall(model)$data,
                         envir=environment(stats::formula(model))), error=function(e) NULL)
  rows = match(rownames(data), rownames(source))
  if (is.null(source) || anyNA(rows) || !all(variables %in% names(source))) {
    stop("Cannot recover original predictor values for transformed model terms.", call.=FALSE)
  }
  source[rows, , drop=FALSE]
}

logistic_factor_contrasts = function(model, variable, values) {
  fitted_levels = model$xlevels[[variable]]
  if (is.null(fitted_levels)) fitted_levels = levels(factor(values))
  coding = model$contrasts[[variable]]
  if (is.null(coding)) return(stats::contrasts(factor(values, levels=fitted_levels)))
  if (is.matrix(coding)) return(coding)
  contrast_function = get(coding, envir=environment(stats::formula(model)), mode="function")
  contrast_function(fitted_levels)
}

# Classify fitted expressions, not the storage type of their source columns.
logistic_predictor_info = function(model, data, variables) {
  terms = stats::terms(model)
  expressions = as.list(attr(stats::delete.response(terms), "variables"))[-1]
  frame = stats::model.frame(model)
  keys = names(frame)[seq_along(expressions) + attr(terms, "response")]
  result = setNames(vector("list", length(variables)), variables)
  for (variable in variables) {
    indices = which(vapply(expressions, function(x) variable %in% all.vars(x), logical(1)))
    categorical = vapply(frame[keys[indices]], function(x) is.factor(x) ||
                            is.character(x) || is.logical(x), logical(1))
    if (!any(categorical)) {
      if (!is.numeric(data[[variable]])) {
        stop("Cannot construct continuous prediction profiles for '", variable,
             "'. Use an explicit numeric predictor column and refit the model.", call.=FALSE)
      }
      result[[variable]] = list(categorical=FALSE)
      next
    }
    if (length(indices) != 1L) {
      stop("Predictor '", variable, "' has multiple fitted representations. ",
           "Use separate explicit predictor columns before calling estimates().", call.=FALSE)
    }
    expression = expressions[[indices]]
    key = keys[indices]
    if (!identical(expression, as.name(variable))) {
      head = if (is.call(expression)) expression[[1]] else NULL
      if (is.call(head) && identical(head[[1]], as.name("::")) &&
          identical(head[[2]], as.name("base"))) head = head[[3]]
      if (!is.call(expression) || length(expression) < 2L || !is.symbol(head) ||
          !as.character(head) %in% c("factor", "as.factor", "ordered", "as.ordered") ||
          !identical(expression[[2]], as.name(variable))) {
        stop("Unsupported categorical transformation '", key, "' in estimates(). ",
             "Create an explicit factor column and refit the model.", call.=FALSE)
      }
    }
    fitted = frame[[key]]
    fitted_levels = if (inherits(model, "glm")) model$xlevels[[key]] else NULL
    if (is.null(fitted_levels)) fitted_levels = levels(factor(fitted))
    rows = match(fitted_levels, as.character(fitted))
    unique_inputs = vapply(fitted_levels, function(level) {
      length(unique(data[[variable]][as.character(fitted) == level])) == 1L
    }, logical(1))
    if (anyNA(rows) || !all(unique_inputs)) {
      stop("Cannot map fitted levels of '", key, "' to unique original inputs. ",
           "Create an explicit factor column and refit the model.", call.=FALSE)
    }
    result[[variable]] = list(categorical=TRUE, variable=variable, key=key,
                               expression=expression, levels=fitted_levels,
                               values=data[[variable]][rows], fitted=fitted,
                               ordered=is.ordered(fitted),
                               environment=environment(stats::formula(model)))
  }
  result
}

logistic_set_factor = function(data, predictor, level) {
  value = predictor$values[match(level, predictor$levels)]
  data[[predictor$variable]] = rep(value, nrow(data))
  # Verify that reevaluating the original expression retains its fitted meaning.
  formula = structure(call("~", predictor$expression), class="formula",
                        .Environment=predictor$environment)
  fitted = tryCatch(stats::model.frame(formula, data=data, na.action=stats::na.pass)[[1]],
                     error=function(e) NULL)
  if (is.null(fitted) || anyNA(fitted) || !all(as.character(fitted) == level)) {
    stop("Cannot safely construct predictions for categorical transformation '",
         predictor$key, "'. Use an explicit factor column and refit the model.", call.=FALSE)
  }
  data
}

logistic_reference_data = function(model, data, variables, predictors) {
  reference = data[1, , drop=FALSE]
  for (variable in variables) {
    predictor = predictors[[variable]]
    if (!predictor$categorical) {
      reference[[variable]] = mean(data[[variable]])
      next
    }
    levels = predictor$levels
    baseline = 1L
    if (length(levels) > 1L && inherits(model, "glm")) {
      coding = logistic_factor_contrasts(model, predictor$key, predictor$fitted)
      zero = which(rowSums(abs(coding)) == 0)
      if (length(zero) == 1L) baseline = zero
    }
    reference = logistic_set_factor(reference, predictor, levels[baseline])
  }
  reference
}

logistic_predict = function(model, data, type="link") {
  if (inherits(model, "glmerMod")) {
    return(as.numeric(stats::predict(model, newdata=data, type=type, re.form=NA)))
  }
  as.numeric(stats::predict(model, newdata=data, type=type))
}

# Certify a single focal coordinate before solving. General nonlinear formulas
# may have multiple roots; a finite numerical search cannot establish uniqueness.
logistic_focal_transform = function(variable, terms) {
  expressions = as.list(attr(stats::delete.response(terms), "variables"))[-1]
  transforms = character()
  for (expression in expressions) {
    if (!variable %in% all.vars(expression)) next
    if (is.call(expression) && identical(expression[[1]], as.name("I"))) expression = expression[[2]]
    if (identical(expression, as.name(variable))) {
      transforms = c(transforms, "identity")
      next
    }
    if (!is.call(expression) || length(expression) != 2L ||
        !as.character(expression[[1]]) %in% c("log", "sqrt") ||
        !identical(expression[[2]], as.name(variable))) return(NULL)
    transforms = c(transforms, as.character(expression[[1]]))
  }
  transforms = unique(transforms)
  if (length(transforms) != 1L) return(NULL)
  transforms
}

logistic_conditional_summary = function(variable, model_info, location) {
  missing = c(instantaneous_slope=NA_real_, intercept_threshold=NA_real_)
  if (!variable %in% model_info$continuous) return(missing)
  transform = logistic_focal_transform(variable, stats::terms(model_info$model))
  if (is.null(transform)) return(missing)
  # Four coordinates check the fitted conditional link for an affine dependence
  # on the certified focal transform, including interactions and offsets.
  coordinate = if (transform == "sqrt") c(0, 1, 2, 3) else c(-1, 0, 1, 2)
  x = switch(transform, identity=coordinate, log=exp(coordinate), sqrt=coordinate^2)
  profile = model_info$reference[rep(1, length(x)), , drop=FALSE]
  profile[[variable]] = x
  eta = tryCatch(logistic_predict(model_info$model, profile), error=function(e) rep(NA_real_, 4))
  if (any(!is.finite(eta))) return(missing)
  slope = eta[2] - eta[1]
  intercept = eta[1] - slope * coordinate[1]
  if (max(abs(eta - (intercept + slope * coordinate))) > 1e-8 * max(1, abs(eta))) return(missing)
  if (abs(slope) < 1e-12) return(c(instantaneous_slope=0, intercept_threshold=NA_real_))
  target = (stats::qlogis(location) - intercept) / slope
  if (transform == "sqrt" && target < 0) return(missing)
  threshold = switch(transform, identity=target, log=exp(target), sqrt=target^2)
  if (!is.finite(threshold) || (transform != "identity" && threshold <= 0)) return(missing)
  derivative = switch(transform, identity=1, log=1/threshold, sqrt=1/(2*sqrt(threshold)))
  c(instantaneous_slope=slope * derivative * location * (1-location),
    intercept_threshold=threshold)
}

logistic_irt_rows = function(model, summary) {
  matrix = stats::model.matrix(model)
  terms = stats::delete.response(stats::terms(model))
  factors = attr(terms, "factors")
  expressions = as.list(attr(terms, "variables"))[-1]
  rows = rep(NA_integer_, ncol(matrix))
  for (i in seq_along(attr(terms, "term.labels"))) {
    variables = unique(unlist(lapply(expressions[factors[, i] != 0], all.vars)))
    columns = which(attr(matrix, "assign") == i)
    if (length(variables) != 1L || length(columns) != 1L) next
    rows[columns] = match(variables, summary$variable)
  }
  rows[match(names(stats::coef(model)), colnames(matrix))]
}

logistic_coefficient_table = function(model) {
  summary = logistic_coefficients(model)
  rows = logistic_irt_rows(model, summary)
  standardized = standardized.beta(model, sd.y=FALSE)[names(stats::coef(model))]
  result = data.frame(raw.coefficients=stats::coef(model),
                       OR=exp(stats::coef(model)), inverse.OR=exp(-stats::coef(model)),
                       standardized.OR=exp(standardized),
                       inverse.standardized.OR=exp(-standardized),
                       instantaneous_slope=summary$instantaneous_slope[rows],
                       intercept_threshold=summary$intercept_threshold[rows])
  result
}

logistic_marginal_probabilities = function(model, info) {
  summaries = list()
  for (variable in info$variables) {
    predictor = info$predictors[[variable]]
    if (!predictor$categorical) next
    for (level in predictor$levels) {
      profile = logistic_set_factor(info$data, predictor, level)
      summaries[[length(summaries)+1L]] = data.frame(
        Variable=variable, Level=level,
        Probability=mean(logistic_predict(model, profile, type="response")))
    }
  }
  if (!length(summaries)) return(NULL)
  do.call(rbind, summaries)
}

logistic_estimates_table = function(model) {
  info = gather_model_info(model)
  result = logistic_coefficient_table(model)
  difference = rep(NA_real_, nrow(result))
  names(difference) = rownames(result)
  for (variable in info$continuous) {
    profile = info$reference[c(1, 1), , drop=FALSE]
    profile[[variable]] = mean(info$data[[variable]]) + c(1, -1)*stats::sd(info$data[[variable]])
    columns = which(rownames(result) == variable)
    if (length(columns)) difference[columns] = abs(round(diff(logistic_predict(model, profile, "response")), 2))
  }
  factors = setdiff(info$variables, info$continuous)
  for (variable in factors) {
    predictor = info$predictors[[variable]]
    levels = predictor$levels
    coding = logistic_factor_contrasts(model, predictor$key, predictor$fitted)
    baseline = which(rowSums(abs(coding)) == 0)
    if (length(baseline) != 1L) next
    profiles = lapply(levels, logistic_set_factor, data=info$reference, predictor=predictor)
    profile = do.call(rbind, profiles)
    predictions = logistic_predict(model, profile, "response")
    matrix = stats::model.matrix(stats::delete.response(stats::terms(model)), profile,
                                  contrasts.arg=model$contrasts, xlev=model$xlevels)
    # Only treatment-coded main effects correspond to discrete level changes.
    terms = stats::delete.response(stats::terms(model))
    expressions = as.list(attr(terms, "variables"))[-1]
    factor_terms = attr(terms, "factors")
    main_row = which(vapply(expressions, identical, logical(1), y=predictor$expression))
    term = if (length(main_row)) which(factor_terms[main_row, ] != 0 &
                                        colSums(factor_terms != 0) == 1L) else integer()
    columns = which(attr(matrix, "assign") %in% term)
    for (column in columns) {
      changed = which(matrix[, column] != matrix[baseline, column])
      if (length(changed) != 1L) next
      name = colnames(matrix)[column]
      if (!name %in% names(difference)) next
      difference[name] = paste0(round_string(predictions[changed] - predictions[baseline]),
                                " (relative to ", variable, levels[baseline], ")")
    }
    if (length(factors) == 1L && !predictor$ordered && "(Intercept)" %in% names(difference)) {
      difference["(Intercept)"] = paste0(round_string(predictions[baseline]), " (", variable,
                                         levels[baseline], " prediction)")
    }
  }
  result[["Prediction Difference (+/- 1 SD)"]] = unname(difference)
  # Preserve the existing display precision while retaining numeric NA values.
  if (length(info$continuous)) result = round_coefficient_matrix(result)
  attr(result, "irt") = logistic_coefficients(model)
  attr(result, "marginal_probabilities") = logistic_marginal_probabilities(model, info)
  attr(result, "conditioning") = info$reference[info$variables]
  attr(result, "continuous") = info$continuous
  result
}
