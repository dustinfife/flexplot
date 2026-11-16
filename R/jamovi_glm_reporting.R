# Reset existing result objects rather than removing them from the result tree.
jamovi_glm_reporting_reset = function(results) {
  for (item in results$items) {
    item$setVisible(FALSE)
    item$setError("")
    item$setStatus("complete")
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
  description = "<p>Estimates follow estimates.glm(). Confidence intervals are 95% Wald intervals for raw coefficients.</p>"
  if (unavailable) description = paste0(description,
    "<p>Additional estimates.glm summaries are unavailable for this model specification. Raw coefficients, standard errors, confidence intervals and predictions remain available.</p>")
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
        superTitle=if (name %in% c("lower", "upper")) "95% Wald CI" else NULL,
        format=if (name %in% c(p_names, "p")) "zto,pvalue" else "")
    }
    column = results$coefficients$getColumn(name)
    column$visible = TRUE
  }
  jamovi_glm_table_rows(results$coefficients, reported)
  results$coefficients$setNote("model", paste("Family:", model$family$family, " Link:", model$family$link))
  if (state$omitted > 0L) results$coefficients$setNote("missing",
    paste(state$omitted, "observations omitted because model variables were missing."))
  if (length(state$warnings)) results$coefficients$setNote("warnings", paste(state$warnings, collapse="; "))
  if (model$family$family != "binomial") {
    interpretation = switch(model$family$link,
      log="Log-link multiplicative coefficients describe changes in the response mean. Standardized multipliers use a one-SD predictor change.",
      inverse="Reciprocal coefficients describe the inverse-mean scale; they are not response ratios.",
      identity="Raw coefficients are in outcome units.")
    description = paste0(description, "<p>", interpretation,
      " Prediction differences follow the existing conditional mean +/- one-SD or categorical-contrast convention. Interaction coefficients are conditional.</p>")
    if (inherits(model, "negbin")) description = paste0(description,
      "<p>Negative Binomial uncertainty conditions on estimated size (theta).</p>")
    results$explanation$setContent(description)
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
  description = paste0(description, "<p>OR and inverse.OR describe fitted coefficient contrasts. Standardized odds ratios use predictor SDs without standardizing the binary outcome. Estimates retain estimates.glm() display precision.</p>")
  results$explanation$setContent(description)
  invisible(NULL)
}

jamovi_glm_plot_guide = function(state, x=NULL, panels=NULL, group=NULL, style="auto") {
  formula = jamovi_model_layout(list(state$model), state$data, x, panels, group)
  hidden = setdiff(all.vars(stats::formula(state$model))[-1], all.vars(formula)[-1])
  x = flexplot_axis_given(formula)$axis[1]
  overlay = state$model$family$family == "binomial" && style == "auto" &&
    length(x) && x %in% names(state$data) && is.numeric(state$data[[x]]) &&
    length(unique(state$data[[x]])) >= 10L
  guide = if (overlay) "<p>Dots show observed bin proportions; curves show fitted-model probabilities.</p>" else
    "<p>Model predictions are on the response scale.</p>"
  if (length(hidden)) guide = paste0(guide,
    "<p>Unshown predictors are averaged over eligible observations.",
    if (overlay) " Curves are adjusted marginal predictions, not bin-matched predictions." else "", "</p>")
  guide
}
