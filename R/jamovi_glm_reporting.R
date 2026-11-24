# Reset existing result objects rather than removing them from the result tree.
jamovi_glm_reporting_reset = function(results) {
  for (item in results$items) {
    item$setVisible(FALSE)
    jamovi_result_clear(item)
    if (inherits(item, "Image")) item$.setPath(NULL)
    if (inherits(item, "Table")) {
      if (item$name == "coefficients") {
        for (column in item$columns) column$visible = identical(column$name, "term")
      }
    }
  }
  jamovi_plot_clear_states(results, c("plot", "comparisonplot"))
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
    if (!table$visible || table$rowCount == 0L) {
      for (key in names(table$notes)) table$setNote(key,NULL)
      jamovi_table_clear_footnotes(table)
    }
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
  for (name in c("coefficients","irt","probabilities")) jamovi_result_clear(results[[name]])
  for (name in c("irt","probabilities")) results[[name]]$setVisible(FALSE)
  for (column in results$coefficients$columns) column$visible = identical(column$name,"term")
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
  model_note = paste0("Family: ", model$family$family, "; Link: ", model$family$link)
  if (model$family$family == "binomial") {
    response = stats::model.response(stats::model.frame(model))
    success = if (is.factor(response)) levels(response)[2] else "1"
    model_note = paste0(model_note, ". Success event: ", success)
  }
  results$coefficients$setNote("model", model_note)
  if (state$omitted > 0L) results$coefficients$setNote("missing",
    paste(state$omitted, "observations omitted because model variables were missing."))
  if (unavailable) results$coefficients$setNote("interpretation",
    "Supplementary effect summaries are unavailable for this specification.")
  uncertainty = "SE and confidence limits refer to raw coefficients."
  if (t_intervals) uncertainty = paste0(uncertainty," t reference with ",stats::df.residual(model)," residual df.")
  if (inherits(model,"negbin")) uncertainty = paste(uncertainty,"Uncertainty conditions on estimated size (theta).")
  jamovi_table_footnote(results$coefficients,c("Std. Error","Std..Error","SE","lower","upper"),uncertainty)
  effect_rows = which(reported$term!="(Intercept)")
  if (shown("Prediction Difference (+/- 1 SD)")) jamovi_table_footnote(results$coefficients,
    "Prediction Difference (+/- 1 SD)",
    "Prediction differences compare mean - 1 SD with mean + 1 SD, or factor levels, at the reference profile.",rows=effect_rows)
  assignment = attr(stats::model.matrix(model),"assign")[match(rownames(values),colnames(stats::model.matrix(model)))]
  factors = attr(stats::terms(model),"factors")
  factor_variables = names(Filter(is.factor,stats::model.frame(model)[-1]))
  factor_terms = if (length(factors)) which(colSums(factors[intersect(rownames(factors),factor_variables),,drop=FALSE])>0L) else integer()
  jamovi_table_footnote(results$coefficients,"term","Factor coefficients follow the fitted contrasts.",
    rows=which(assignment %in% factor_terms))
  interaction_terms = which(attr(stats::terms(model),"order")>1L)
  if (length(interaction_terms)) {
    involved = rownames(factors)[rowSums(factors[,interaction_terms,drop=FALSE])>0L]
    conditional_terms = which(colSums(factors[involved,,drop=FALSE])>0L)
    for (row in which(assignment %in% conditional_terms)) jamovi_table_footnote(results$coefficients,
      c("raw.coefficients","Estimate"),"Interaction effects depend on the other interacting predictors.",rows=row)
  }
  if (model$family$family != "binomial") {
    jamovi_table_footnote(results$coefficients,"multiplicative.coef","Multipliers act on the expected outcome.",rows=effect_rows)
    jamovi_table_footnote(results$coefficients,"inverse.coef","Reciprocals are not response ratios.",rows=effect_rows)
    jamovi_table_footnote(results$coefficients,"std.mult.coef",if(model$family$link=="inverse")
      "Standardized reciprocals use a one-SD predictor change." else "Standardized multipliers use a one-SD predictor change.",rows=effect_rows)
    return(invisible(NULL))
  }
  if (!is.null(irt)) {
    irt = irt[irt$variable %in% attr(values, "continuous"), , drop=FALSE]
    jamovi_glm_table_rows(results$irt, irt)
    results$irt$setVisible(nrow(irt) > 0L)
    if (length(attr(values,"conditioning"))>1L) jamovi_table_footnote(results$irt,"variable",paste(
      "Estimates are conditional on other numeric predictors at their means and factors at fitted reference levels.",
      if(length(interaction_terms)) "Interaction results depend on these settings." else ""))
    jamovi_table_footnote(results$irt,"instantaneous_slope","Probability change per predictor unit, not average marginal effects.")
    jamovi_table_footnote(results$irt,"intercept_threshold","Thresholds are in predictor units.",missing=TRUE)
    for(row in which(!is.finite(irt$intercept_threshold))) jamovi_table_footnote(results$irt,
      "intercept_threshold","Blank thresholds are undefined or nonunique.",rows=row,missing=TRUE)
  }
  if (!is.null(probabilities)) {
    jamovi_glm_table_rows(results$probabilities, probabilities)
    results$probabilities$setVisible(nrow(probabilities) > 0L)
    jamovi_table_footnote(results$probabilities,"Probability",
      "Each level is set across eligible observations; probabilities are averaged over the other observed predictors.")
  }
  jamovi_table_footnote(results$coefficients,c("standardized.OR","inverse.standardized.OR"),
    "Std. OR uses a one-SD change in the fitted predictor contrast.",rows=effect_rows)
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
