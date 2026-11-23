# Quadratic selections are literal numeric predictor names, not formula text.
jamovi_lm_formula = function(options, data, second=FALSE) {
  terms = if (second) options$comparisonTerms else options$modelTerms
  enabled = if (second) options$comparisonPolynomials else options$polynomials
  quadratic = if (isTRUE(enabled)) {
    if (second) options$comparisonQuadratic else options$quadratic
  } else NULL
  jamovi_fixed_formula(options,data,terms,quadratic)
}

jamovi_glm_fit = function(formula, data, label) {
  model = tryCatch(stats::lm(formula, data=data, na.action=stats::na.fail),
                   error=function(error) stop(label, ": ", conditionMessage(error), call.=FALSE))
  if (model$rank < length(stats::coef(model)) || stats::df.residual(model) <= 0L) {
    stop(label, " is not estimable. Check redundant terms, empty factor combinations, ",
         "and the number of complete observations.", call.=FALSE)
  }
  model
}

jamovi_glm_comparison = function(options, data) {
  formula1 = jamovi_lm_formula(options,data)
  formula2 = jamovi_lm_formula(options,data,second=TRUE)
  variables = union(all.vars(formula1), all.vars(formula2))
  eligible = data[stats::complete.cases(data[, variables, drop=FALSE]), , drop=FALSE]
  if (!nrow(eligible)) stop("No complete observations are available for both models.", call.=FALSE)
  model1 = jamovi_glm_fit(formula1, eligible, "Model 1")
  model2 = jamovi_glm_fit(formula2, eligible, "Model 2")

  plot_formula = tryCatch(jamovi_comparison_layout(list(model1, model2), eligible, options$comparisonX,
                                    options$comparisonPanels, options$comparisonGroup, allow_magnet=TRUE),error=identity)
  plot_error = if (inherits(plot_formula,"error")) conditionMessage(plot_formula) else NULL
  if (!is.null(plot_error)) plot_formula = NULL
  comparison = model.comparison(model1, model2)
  # Keep the F-test numeric: model.comparison formats its rounded p-value as text.
  p = get_p_value(model1, model2, check_nested(model1, model2))
  list(model1=model1, model2=model2, formula=plot_formula, plotError=plot_error, data=eligible,
       statistics=comparison$statistics, p=if (length(p)) p[1] else NA_real_)
}

jamovi_glm_comparison_table = function(table, state) {
  statistics = state$statistics
  for (i in seq_len(2L)) {
    row = list(model=paste("Model", i), n=stats::nobs(state[[paste0("model", i)]]))
    for (name in c("aic", "bic", "bayes.factor", "rsq", "adj.rsq")) {
      row[[name]] = if (name %in% names(statistics)) as.numeric(statistics[i, name]) else NA_real_
    }
    # An omitted cell is blank; numeric NA is serialized as a NaN double.
    if (i == 1L && !is.na(state$p)) row$p = state$p
    table$addRow(rowKey=i, values=row)
  }
  table$addFootnote(rowNo=1, col="n", note="Both models use the same complete observations across their combined variables.")
  table$addFootnote(rowNo=1, col="bayes.factor", note=paste(
    "These are approximate Bayes factors derived from BIC.",
    "Model 1's Bayes factor expresses evidence favoring Model 1 over Model 2.",
    "Model 2's Bayes factor expresses evidence favoring Model 2 over Model 1."))
  table$addFootnote(rowNo=1, col="p", note="F-test for nested models only; unavailable for nonnested or equivalent models.")
}

jamovi_glm_comparison_plot = function(state) {
  if (!is.null(state$plotError)) stop(state$plotError,call.=FALSE)
  model1 = state$model1
  model2 = state$model2
  roles = flexplot_axis_given(state$formula)
  formula = jamovi_comparison_layout(list(model1, model2), state$data, roles$axis[1],
    roles$given[!is.na(roles$given)], roles$axis[-1], allow_magnet=TRUE)
  plot = jamovi_comparison_style(compare_fits(formula, data=state$data, model1=model1, model2=model2))
  note = attr(formula, "jamovi_comparison_note")
  if (is.null(note)) note = attr(state$formula, "jamovi_comparison_note")
  attr(plot, "jamovi_comparison_note") = note
  plot
}
