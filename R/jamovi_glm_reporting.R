# Reset existing result objects rather than removing them from the result tree.
jamovi_glm_reporting_reset = function(results) {
  for (item in results$items) {
    item$setVisible(FALSE)
    item$setError("")
    item$setStatus("complete")
    if (inherits(item, "Image")) item$.setPath(NULL)
    if (inherits(item, "Table")) {
      item$deleteRows()
      for (key in names(item$notes)) item$setNote(key, NULL)
      if (item$name == "coefficients") {
        for (column in item$columns) column$visible = identical(column$name, "term")
      }
    }
    if (inherits(item, "Html")) item$setContent("")
  }
  results$plot$setState(NULL)
  results$comparisonplot$setState(NULL)
  results$waiting$setContent("<p>Select an outcome and predictors, and specify at least one model term.</p>")
  results$waiting$setVisible(TRUE)
  invisible(NULL)
}

# Omit undefined cells so the results renderer shows blanks, not NaN/NA text.
jamovi_glm_table_rows = function(table, values) {
  for (i in seq_len(nrow(values))) {
    row = as.list(values[i, , drop=FALSE])
    for (name in names(row)) {
      value = row[[name]]
      if (is.na(value) || (is.numeric(value) && !is.finite(value)) ||
          (is.character(value) && !name %in% c("term", "Variable", "Level", "variable") &&
           value %in% c("NA", "NaN"))) row[[name]] = NULL
    }
    table$addRow(rowKey=i, values=row)
  }
}

# Coefficient-level statistics come from estimates.glm; uncertainty from the fitted model.
jamovi_glm_estimates = function(results, state, includeP=FALSE) {
  model = state$model
  coefficients = summary(model)$coefficients
  values = tryCatch(estimates(model), error=identity)
  unavailable = inherits(values, "error")
  if (unavailable) {
    values = data.frame(raw.coefficients=stats::coef(model), check.names=FALSE)
  } else {
    class(values) = "data.frame"
  }
  t_intervals = colnames(coefficients)[3] == "t value"
  interval_title = if (t_intervals) "95% t CI" else "95% normal Wald CI"
  results$coefficients$setNote("intervals", if (t_intervals)
    paste0("95% t confidence intervals for raw coefficients; residual df = ", stats::df.residual(model), ".") else NULL)
  description = ""
  if (unavailable) description = paste0(description,
    " Supplementary effect summaries are unavailable for this model specification. Raw coefficients, standard errors, confidence intervals and predictions remain available.")
  irt = attr(values, "irt")
  probabilities = attr(values, "marginal_probabilities")
  # IRT and marginal probabilities have predictor-level rows, not coefficient rows.
  columns = setdiff(names(values), c("instantaneous_slope", "intercept_threshold"))
  p_names = c(colnames(coefficients)[4], make.names(colnames(coefficients)[4]))
  if (!includeP) columns = setdiff(columns, p_names)
  reported = values[, columns, drop=FALSE]
  indices = match(rownames(values), rownames(coefficients))
  if (!any(c("Std. Error", "Std..Error") %in% names(reported))) {
    reported$SE = coefficients[indices, 2]
  }
  critical = if (colnames(coefficients)[3] == "t value")
    stats::qt(.975, stats::df.residual(model)) else stats::qnorm(.975)
  reported$lower = coefficients[indices, 1] - critical*coefficients[indices, 2]
  reported$upper = coefficients[indices, 1] + critical*coefficients[indices, 2]
  if (includeP && !any(p_names %in% names(reported))) reported$p = coefficients[indices, 4]
  reported = cbind(data.frame(term=rownames(values)), reported)
  for (name in setdiff(names(reported), "term")) {
    numeric = is.numeric(reported[[name]])
    if (!name %in% names(results$coefficients$columns)) {
      results$coefficients$addColumn(name, title=name, type=if (numeric) "number" else "text",
        superTitle=if (name %in% c("lower", "upper")) interval_title else NULL,
        format=if (name %in% c(p_names, "p")) "zto,pvalue" else "")
    }
    column = results$coefficients$getColumn(name)
    column$visible = TRUE
    if (name %in% c("lower", "upper")) column$setSuperTitle(interval_title)
  }
  jamovi_glm_table_rows(results$coefficients, reported)
  results$coefficients$setNote("uncertainty", "SE describes uncertainty in raw coefficients; confidence limits use the same coefficient scale.")
  if (any(grepl("Prediction Difference", names(reported), fixed=TRUE)))
    results$coefficients$setNote("differences", "Prediction differences compare expected outcomes at mean +/- one SD for numeric predictors or fitted factor contrasts, with other predictors at their reference profile.")
  results$coefficients$setNote("model", paste0("Family: ", model$family$family, "; Link: ", model$family$link))
  if (state$omitted > 0L) results$coefficients$setNote("missing",
    paste(state$omitted, "observations omitted because model variables were missing."))
  if (length(state$warnings)) results$coefficients$setNote("warnings", paste(state$warnings, collapse="; "))
  if (model$family$family != "binomial") {
    interpretation = switch(model$family$link,
      log="Log-link multiplicative coefficients describe changes in the response mean. Standardized multipliers use a one-SD predictor change.",
      inverse="Reciprocal coefficients describe the inverse-mean scale; they are not response ratios.",
      identity="Raw coefficients are in outcome units.")
    description = paste0(description, " ", interpretation,
      " Interaction coefficients are conditional on the other predictors.")
    if (inherits(model, "negbin")) description = paste0(description,
      " Negative Binomial uncertainty conditions on estimated size (theta).")
    results$coefficients$setNote("interpretation", trimws(description))
    return(invisible(NULL))
  }
  response = stats::model.response(stats::model.frame(model))
  success = if (is.factor(response)) levels(response)[2] else "1"
  results$coefficients$setNote("success", paste("Success event:", success))
  if (!is.null(irt)) {
    irt = irt[irt$variable %in% attr(values, "continuous"), , drop=FALSE]
    jamovi_glm_table_rows(results$irt, irt)
    results$irt$setVisible(nrow(irt) > 0L)
    if (length(attr(values, "conditioning")) > 1L) results$irt$setNote("conditioning", paste(
      "IRT estimates are conditional on other continuous predictors being held at their means and categorical predictors at their reference levels.",
      "For predictors involved in interactions, estimates may differ at other moderator values."))
    results$irt$setNote("interpretation", paste(
      "Instantaneous slopes are probability change per predictor unit at probability 0.5; thresholds are predictor values at probability 0.5.",
      "Only continuous predictors are included. Undefined or nonunique thresholds are blank. These are conditional estimates, not average marginal effects."))
  }
  if (!is.null(probabilities)) {
    jamovi_glm_table_rows(results$probabilities, probabilities)
    results$probabilities$setVisible(nrow(probabilities) > 0L)
    results$probabilities$setNote("averaging", "Each category is set across eligible observations; response-scale probabilities are averaged over other observed predictors.")
  }
  description = paste0(description,
    "Raw coefficients are on the log-odds scale. OR gives the odds multiplier for the fitted contrast; inverse.OR gives its reciprocal.",
    " Standardized odds ratios use a one-SD predictor change; inverse standardized odds ratios reverse that contrast.")
  results$coefficients$setNote("interpretation", trimws(description))
  invisible(NULL)
}

jamovi_glm_plot_guide = function(state, x=NULL, panels=NULL, group=NULL, style="auto", scale="probability") {
  models = if (is.null(state$models)) list(state$model) else state$models
  formula = jamovi_model_layout(models, state$data, x, panels, group)
  predictors = unique(unlist(lapply(models, function(model) all.vars(stats::formula(model))[-1])))
  hidden = setdiff(predictors, all.vars(formula)[-1])
  x = flexplot_axis_given(formula)$axis[1]
  display = jamovi_glm_visualization(state$data, x, state$model$family$family == "binomial", style, scale)
  guide = display$note
  if (display$overlay) {
    guide = paste(guide, if (display$scale == "logit")
      "Dots show observed-bin logits (half-observation bounds); curves show logits of fitted probabilities." else
      "Dots show observed-bin success proportions; curves show fitted-model probabilities.")
    if (display$scale == "logit") guide = paste(guide,
      "Exact zero/one probabilities and confidence limits have infinite logits; model probabilities are not clipped.")
  }
  if (length(hidden)) guide = paste(guide, "Unshown predictors are averaged over eligible observations.",
    if (display$overlay) "Curves are adjusted marginal predictions, not bin-matched predictions." else "")
  if (display$overlay && display$scale == "logit" && length(hidden)) guide = paste(guide,
    "Curves transform averaged probabilities, not average linear predictors.")
  guide = trimws(guide)
  if (!nzchar(guide)) return("")
  paste0("<p>", guide, "</p>")
}
