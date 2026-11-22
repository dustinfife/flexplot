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

# Omit undefined values, preserving literal text and displayed factor labels.
jamovi_glm_table_rows = function(table, values) {
  for (i in seq_len(nrow(values))) {
    row = as.list(values[i, , drop=FALSE])
    for (name in names(row)) {
      value = row[[name]]
      if (is.factor(value)) row[[name]] = value = as.character(value)
      if (is.na(value) || (is.numeric(value) && !is.finite(value))) row[[name]] = NULL
    }
    table$addRow(rowKey=i, values=row)
  }
}

# Display labels only; stable column identifiers continue to hold the values.
jamovi_glm_column_title = function(name, link) {
  titles = c(raw.coefficients="Coefficient", Estimate="Coefficient", OR="OR",
    inverse.OR="1/OR", standardized.OR="Std. OR", inverse.standardized.OR="1/Std. OR",
    multiplicative.coef="Multiplier", inverse.coef="Reciprocal",
    std.mult.coef=if (link == "inverse") "Std. reciprocal" else "Std. multiplier",
    lower="Lower", upper="Upper", Std..Error="SE", `Std. Error`="SE",
    t.value="t", z.value="z", Pr...t..="p", Pr...z..="p",
    `Prediction Difference (+/- 1 SD)`="Prediction difference")
  if (name %in% names(titles)) unname(titles[[name]]) else name
}

# Hidden tables should not retain orphaned explanatory notes.
jamovi_glm_visible_notes = function(results) {
  context = results$coefficients$notes[intersect(c("model","missing"),names(results$coefficients$notes))]
  for (name in c("coefficients","irt","probabilities")) {
    table = results[[name]]
    if (!table$visible || table$rowCount == 0L)
      for (key in names(table$notes)) table$setNote(key,NULL)
  }
  if (results$coefficients$visible) return(invisible(NULL))
  for (name in c("irt","probabilities")) {
    table = results[[name]]
    if (!table$visible || table$rowCount == 0L) next
    for (key in names(context)) if (!is.null(context[[key]]))
      table$setNote(key,context[[key]]$note)
    break
  }
  invisible(NULL)
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
    paste0("Residual df: ", stats::df.residual(model), ".") else NULL)
  description = ""
  if (unavailable) description = paste0(description,
    "Supplementary effect summaries are unavailable for this specification.")
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
    column$setTitle(jamovi_glm_column_title(name,model$family$link))
    column$visible = TRUE
    if (name %in% c("lower", "upper")) column$setSuperTitle(interval_title)
  }
  jamovi_glm_table_rows(results$coefficients, reported)
  shown = function(name) {
    if (!name %in% names(reported)) return(FALSE)
    value = reported[[name]]
    if (is.numeric(value)) return(any(is.finite(value)))
    any(!is.na(value) & !value %in% c("", "NA", "NaN"))
  }
  if (any(vapply(c("OR","multiplicative.coef","inverse.coef"),shown,logical(1))))
    results$coefficients$setNote("uncertainty", "SE and confidence limits refer to raw coefficients.")
  if (shown("Prediction Difference (+/- 1 SD)"))
    results$coefficients$setNote("differences", "Prediction differences compare mean - 1 SD with mean + 1 SD, or factor levels, at the reference profile.")
  model_note = paste0("Family: ", model$family$family, "; Link: ", model$family$link)
  if (model$family$family == "binomial") {
    response = stats::model.response(stats::model.frame(model))
    success = if (is.factor(response)) levels(response)[2] else "1"
    model_note = paste0(model_note, ". Success event: ", success)
  }
  results$coefficients$setNote("model", model_note)
  if (state$omitted > 0L) results$coefficients$setNote("missing",
    paste(state$omitted, "observations omitted because model variables were missing."))
  if (any(vapply(stats::model.frame(model)[-1],is.factor,logical(1))))
    description = paste(description,"Factor coefficients follow the fitted contrasts.")
  if (any(attr(stats::terms(model),"order") > 1L))
    description = paste(description,"Interaction effects depend on the other interacting predictors.")
  if (model$family$family != "binomial") {
    interpretation = switch(model$family$link,
      log=paste("Coefficients are on the log-mean scale.",
        if (shown("multiplicative.coef")) "Multipliers act on the expected outcome." else "",
        if (shown("std.mult.coef")) "Standardized multipliers use a one-SD predictor change." else ""),
      inverse=paste("Coefficients are on the inverse-mean scale.",
        if (shown("inverse.coef")) "Reciprocals are not response ratios." else "",
        if (shown("std.mult.coef")) "Standardized reciprocals use a one-SD predictor change." else ""),
      identity="Coefficients are in outcome units.")
    description = paste(description,interpretation)
    if (inherits(model, "negbin")) description = paste(description,
      "Uncertainty conditions on estimated size (theta).")
    results$coefficients$setNote("interpretation", trimws(description))
    return(invisible(NULL))
  }
  if (!is.null(irt)) {
    irt = irt[irt$variable %in% attr(values, "continuous"), , drop=FALSE]
    jamovi_glm_table_rows(results$irt, irt)
    results$irt$setVisible(nrow(irt) > 0L)
    if (nrow(irt) && length(attr(values, "conditioning")) > 1L) results$irt$setNote("conditioning", paste(
      "Estimates are conditional on other numeric predictors at their means and factors at fitted reference levels.",
      if (any(attr(stats::terms(model),"order") > 1L)) "Interaction results depend on these settings." else ""))
    if (nrow(irt)) results$irt$setNote("interpretation", paste(
      "Slopes describe probability change per predictor unit, not average marginal effects. Thresholds are in predictor units.",
      if (any(!is.finite(irt$intercept_threshold))) "Blank thresholds are undefined or nonunique." else ""))
  }
  if (!is.null(probabilities)) {
    jamovi_glm_table_rows(results$probabilities, probabilities)
    results$probabilities$setVisible(nrow(probabilities) > 0L)
    if (nrow(probabilities)) results$probabilities$setNote("averaging", "Each level is set across eligible observations; probabilities are averaged over the other observed predictors.")
  }
  description = paste(description, "Coefficients are log odds.",
    if (shown("OR")) "OR multiplies the odds." else "",
    if (shown("inverse.OR") || shown("inverse.standardized.OR")) "Inverse ratios are reciprocals." else "",
    if (shown("standardized.OR")) "Std. OR uses a one-SD change in the fitted predictor contrast." else "")
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
