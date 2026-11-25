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
  titles = c(term="Term",raw.coefficients="Coefficient", Estimate="Coefficient", OR="OR",
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
  model_note = if (inherits(model,"negbin")) paste0(
    "Family: Negative Binomial; Link: ",model$family$link,"; Estimated size (θ): ",
    formatC(model$theta,format="f",digits=3,decimal.mark=".")) else
    paste0("Family: ", model$family$family, "; Link: ", model$family$link)
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
  notes = list(uncertainty=uncertainty)
  targets = list(uncertainty=c("Std. Error","Std..Error","SE","lower","upper"))
  if (shown("Prediction Difference (+/- 1 SD)")) {
    notes$differences = "Prediction differences compare mean - 1 SD with mean + 1 SD, or factor levels, at the reference profile."
    targets$differences = "Prediction Difference (+/- 1 SD)"
  }
  interaction_terms = which(attr(stats::terms(model),"order")>1L)
  if (model$family$family == "binomial") {
    # SD scales fitted contrast columns, not factor level changes or an
    # unconditional main effect in an interaction model.
    frame = stats::model.frame(model)[-1]
    expressions = as.list(attr(stats::delete.response(stats::terms(model)),"variables"))[-1]
    if ((any(vapply(frame,function(value)is.factor(value) || is.character(value) || is.logical(value),logical(1))) ||
        length(interaction_terms) || any(!vapply(expressions,is.symbol,logical(1)))) &&
        (shown("standardized.OR") || shown("inverse.standardized.OR"))) {
      notes$standardized = "Std. OR scales each coefficient by the SD of its fitted model-matrix column."
      targets$standardized = c("standardized.OR","inverse.standardized.OR")
    }
  } else if (model$family$link=="inverse") {
    notes$reciprocal = "Reciprocals are not response ratios."
    targets$reciprocal = "inverse.coef"
    notes$standardized = "Standardized reciprocals use a one-SD predictor change."
    targets$standardized = "std.mult.coef"
  }
  names = vapply(results$coefficients$columns,function(column)column$name,character(1))
  titles = setNames(vapply(names,jamovi_glm_column_title,character(1),link=model$family$link),names)
  jamovi_table_column_notes(results$coefficients,titles,notes,targets)
  if (model$family$family != "binomial") return(invisible(NULL))
  if (!is.null(irt)) {
    irt = irt[irt$variable %in% attr(values, "continuous"), , drop=FALSE]
    jamovi_glm_table_rows(results$irt, irt)
    results$irt$setVisible(nrow(irt) > 0L)
    jamovi_table_column_notes(results$irt,
      c(variable="Predictor",instantaneous_slope="Slope (p = .5)",intercept_threshold="Threshold (p = .5)"),
      list(reference=if (length(attr(values,"conditioning"))>1L)
        "Other numeric predictors are held at their means and categorical predictors at their reference levels." else NULL),
      list(reference="variable"))
    undefined = which(!is.finite(irt$intercept_threshold))
    if (length(undefined)) {
      # A distinct warning symbol avoids reusing the header-note letter.
      for (row in undefined) results$irt$addSymbol("intercept_threshold","†",rowNo=row)
      results$irt$setNote("undefined","† Blank thresholds are undefined or nonunique.")
    }
  }
  if (!is.null(probabilities)) {
    jamovi_glm_table_rows(results$probabilities, probabilities)
    results$probabilities$setVisible(nrow(probabilities) > 0L)
  }
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
      "Logistic overlay plot: dots represent observed proportions within bins of the x-axis variable.")
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
