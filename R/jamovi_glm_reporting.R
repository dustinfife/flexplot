# Render existing estimates() results without duplicating their calculations.
jamovi_glm_estimates = function(results, state) {
  model = state$model
  values = tryCatch(estimates(model), error=identity)
  description = paste0("<p>Gaussian deviance is residual sum of squares; other families report deviance relative to a saturated model. ",
    "Confidence intervals are Wald intervals on the coefficient scale.</p>")
  if (inherits(model, "negbin")) description = paste0(description,
    "<p>Negative Binomial coefficient and prediction intervals condition on estimated size (theta).</p>")
  if (inherits(values, "error")) {
    results$effects$setNote("unsupported", paste("Additional estimates.glm summaries are unavailable for this specification:", conditionMessage(values)))
    results$explanation$setContent(paste0(description,
      "<p>Additional estimates.glm summaries are unavailable for this model specification. Raw coefficients, standard errors, confidence intervals, fit statistics and predictions remain available.</p>"))
    return(invisible(NULL))
  }
  class(values) = "data.frame"
  if (model$family$family != "binomial") {
    extra = setdiff(names(values), c("raw.coefficients", colnames(summary(model)$coefficients)))
    titles = c(multiplicative.coef="Mean multiplier", std.mult.coef="Standardized multiplier",
      inverse.coef="Reciprocal coefficient")
    for (name in extra) results$effects$addColumn(name, title=if (name %in% names(titles)) titles[[name]] else name,
      type=if (is.numeric(values[[name]])) "number" else "text")
    for (i in seq_len(nrow(values))) results$effects$addRow(rowKey=i,
      values=c(list(term=rownames(values)[i]), as.list(values[i, extra, drop=FALSE])))
    description = paste0(description,
      "<p>Family-specific estimates use estimates.glm(): log-link coefficients are multiplicative changes in the mean; ",
      "standardized multipliers use a one-SD predictor change. Inverse-link reciprocals are on the inverse-mean scale, ",
      "not response ratios. Prediction differences compare conditional predictions at mean +/- one SD. ",
      "Interaction coefficients retain their conditional interpretation.</p>")
    results$explanation$setContent(description)
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(values))) {
    results$odds$addRow(rowKey=i, values=list(term=rownames(values)[i], odds=values$OR[i], inverse=values$inverse.OR[i]))
    results$standardized$addRow(rowKey=i, values=list(term=rownames(values)[i],
      odds=values$standardized.OR[i], inverse=values$inverse.standardized.OR[i],
      difference=as.character(values[["Prediction Difference (+/- 1 SD)"]][i])))
  }
  irt = attr(values, "irt")
  irt = irt[irt$variable %in% attr(values, "continuous"), , drop=FALSE]
  for (i in seq_len(nrow(irt))) results$irt$addRow(rowKey=i, values=list(variable=irt$variable[i],
    slope=irt$instantaneous_slope[i], threshold=irt$intercept_threshold[i]))
  if (length(attr(values, "conditioning")) > 1L) results$irt$setNote("conditioning", paste(
    "IRT estimates are conditional on other continuous predictors being held at their means and categorical predictors at their reference levels.",
    "For predictors involved in interactions, estimates may differ at other moderator values."))
  results$irt$setNote("interpretation", paste(
    "Instantaneous slopes are probability change per predictor unit at probability 0.5; thresholds are predictor values at probability 0.5.",
    "Only continuous predictors are included. Undefined or nonunique thresholds are missing, not zero. These are conditional estimates, not average marginal effects."))
  probabilities = attr(values, "marginal_probabilities")
  if (!is.null(probabilities)) for (i in seq_len(nrow(probabilities))) results$probabilities$addRow(rowKey=i, values=list(
    variable=probabilities$Variable[i], level=probabilities$Level[i], probability=probabilities$Probability[i]))
  results$probabilities$setNote("averaging", "Each category is set across the eligible observations; response-scale probabilities are averaged over the other observed predictors.")
  results$standardized$setNote("scale", "Standardized odds ratios use predictor SDs, without standardizing the binary outcome. Values retain estimates.glm() display precision.")
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
