# GLM comparison is separate from the Linear Model adapter's F-test and R-squared.
jamovi_glm_generalized_comparison = function(options, data) {
  formulas = list(jamovi_glm_generalized_formula(options, data),
    jamovi_glm_generalized_formula(options, data, options$comparisonTerms, options$comparisonQuadratic))
  variables = unique(unlist(lapply(formulas, all.vars)))
  eligible = data[stats::complete.cases(data[, variables, drop=FALSE]), , drop=FALSE]
  if (!nrow(eligible)) stop("No complete observations are available for both models.", call.=FALSE)
  primary = jamovi_glm_generalized_fit(options, eligible, formulas[[1]])
  secondary = jamovi_glm_generalized_fit(options, eligible, formulas[[2]])
  model1 = primary$model
  model2 = secondary$model
  layout = jamovi_model_layout(list(model1, model2), primary$data,
    options$comparisonX, options$comparisonPanels)
  # model.comparison supplies the established AIC/BIC/Bayes-factor presentation.
  comparison = model.comparison(model1, model2)
  # Test nesting in the fitted design space, including categorical contrasts.
  models = list(model1, model2)
  ranks = vapply(models, function(model) model$rank, integer(1))
  full = which.max(ranks)
  reduced = 3L-full
  matrix_full = stats::model.matrix(models[[full]])
  matrix_reduced = stats::model.matrix(models[[reduced]])
  nested = diff(range(ranks)) > 0L &&
    qr(cbind(matrix_full, matrix_reduced))$rank == ranks[full]
  lr = df = p = NA_real_
  if (nested) {
    likelihoods = lapply(models, stats::logLik)
    df = attr(likelihoods[[full]], "df")-attr(likelihoods[[reduced]], "df")
    lr = 2*as.numeric(likelihoods[[full]]-likelihoods[[reduced]])
    if (df > 0 && is.finite(lr) && lr >= -1e-8) {
      lr = max(0, lr)
      p = stats::pchisq(lr, df=df, lower.tail=FALSE)
    }
  }
  list(model1=model1, model2=model2, primary=primary, data=primary$data,
    formula=layout, statistics=comparison$statistics, lr=lr, df=df, p=p,
    warnings=unique(c(primary$warnings, secondary$warnings)))
}

jamovi_glm_generalized_comparison_table = function(table, state) {
  for (i in seq_len(2L)) {
    row = list(model=paste("Model", i), n=stats::nobs(state[[paste0("model", i)]]))
    for (name in c("aic", "bic", "bayes.factor")) {
      value = as.numeric(state$statistics[i, name])
      if (is.finite(value)) row[[name]] = value
    }
    if (i == 1L && is.finite(state$p)) {
      row$lr = state$lr
      row$df = state$df
      row$p = state$p
    }
    table$addRow(rowKey=i, values=row)
  }
  formulas = vapply(list(state$model1, state$model2), function(model)
    paste(deparse(stats::formula(model)), collapse=" "), character(1))
  table$setNote("models", paste(paste("Model 1:", formulas[1]), paste("Model 2:", formulas[2]), sep="\n\n"))
  table$setNote("population", "Same complete observations for both models.")
  table$setNote("bayes", paste("Approximate Bayes factors derived from BIC:",
    "Model 1's value favors Model 1 over Model 2; Model 2's value favors Model 2 over Model 1."))
  test_note = if (is.finite(state$p)) "Nested-model likelihood-ratio test (asymptotic chi-square)." else
    "Likelihood-ratio test unavailable: nonnested/equivalent models or invalid likelihood comparison."
  if (inherits(state$model1, "negbin")) test_note = paste(test_note, "Size estimated separately per model.")
  table$setNote("test", test_note)
  if (length(state$warnings)) table$setNote("warnings", paste(state$warnings, collapse="; "))
}

jamovi_glm_generalized_comparison_plot = function(state, se=TRUE, style="auto", scale="probability") {
  model1 = state$model1
  model2 = state$model2
  x = flexplot_axis_given(state$formula)$axis[1]
  overlay = model1$family$family == "binomial" && style == "auto" &&
    is.numeric(state$data[[x]]) && length(unique(state$data[[x]])) >= 10L
  scale = match.arg(scale,c("probability","logit"))
  logit = model1$family$family == "binomial" && scale == "logit"
  if (logit && !overlay) stop("Logit visualization requires Automatic binomial visualization and a continuous numeric x predictor with at least 10 distinct values. Raw binary observations and categorical-x logit overlays are not supported.",call.=FALSE)
  plot = compare_fits(state$formula, state$data, model1=model1, model2=model2,
    pred.type="response", report.se=se, raw.data=!overlay)
  if (se) {
    # Reuse the single-model link-scale intervals with each model's own df.
    prediction_layers = plot$layers
    for (i in seq_len(2L)) {
      for (layer in prediction_layers) {
        if (!is.data.frame(layer$data) || !all(c("prediction", "model") %in% names(layer$data))) next
        rows = startsWith(as.character(layer$data$model), paste0("model", i, " ("))
        if (!any(rows)) next
        interval_plot = plot
        interval_plot$layers = list(ggplot2::ggproto(NULL, layer, data=layer$data[rows, , drop=FALSE]))
        interval_plot = jamovi_model_intervals(interval_plot, model=state[[paste0("model", i)]])
        if (length(interval_plot$layers) > 1L) plot = plot + interval_plot$layers[[2]]
      }
    }
  }
  plot = jamovi_comparison_style(plot)
  if (!overlay) return(plot)
  summary = convert_formula_to_logistic_bins(state$formula, plot$data, n_bins=10)$summary_data
  if (logit) {
    plot = jamovi_glm_logit_display(plot,all.vars(state$formula)[1])
    corrected = pmax(.5/summary$count,pmin((summary$count-.5)/summary$count,summary$proportion))
    summary$proportion = stats::qlogis(corrected)
    for (aesthetic in c("colour","linetype")) {
      scale_object = plot$scales$get_scales(aesthetic)
      if (!is.null(scale_object)) scale_object$name = "Model (logit scale)"
    }
  }
  add_logistic_overlay_layers(plot, summary, "dot", if(logit) "logit" else "probability")
}
