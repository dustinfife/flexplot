# Plot supplied-model predictions while retaining logistic_overlay's presentation.
visualize_binomial_model = function(object, formula, data, n_bins, overlay_type, ...) {
  # Prediction profiles are raw observations, not an already evaluated model
  # matrix: transformed columns must be recomputed when displayed values change.
  attr(data, "terms") = NULL
  options = list(...)
  scale = if (is.null(options$scale)) "probability" else options$scale
  scale = match.arg(scale, c("probability", "logit"))
  options$scale = NULL
  roles = flexplot_axis_given(formula)
  se = check_se(options$se, roles$axis)
  prediction_options = options[intersect(names(options),
                          c("prediction_method", "num_points", "bins", "breaks", "labels"))]
  if (is.null(prediction_options$num_points)) prediction_options$num_points = 80
  prediction = do.call(compare_fits, c(list(formula=formula, data=data,
                        model1=object, return.preds=TRUE, report.se=se), prediction_options))
  attr(prediction, "flexplot_comparison") = TRUE
  options[c("prediction_method", "num_points")] = NULL
  options$prediction = prediction
  options$method = "logistic"
  options$raw.data = FALSE
  options$suppress_smooth = TRUE
  options$se = FALSE
  p = do.call(flexplot, c(list(formula=formula, data=data), options))
  xvar = roles$axis[1]
  outcome = all.vars(formula)[1]
  if (!is.numeric(p$data[[xvar]])) {
    if (scale == "logit") stop("Log-odds model overlays require a continuous numeric x variable.", call.=FALSE)
    return(p)
  }

  summary = convert_formula_to_logistic_bins(formula, p$data, n_bins=n_bins)$summary_data
  if (scale == "logit") {
    corrected = pmax(.5/summary$count, pmin((summary$count-.5)/summary$count, summary$proportion))
    summary$proportion = stats::qlogis(corrected)
    summary$ymin = 0
    summary$ymax = summary$proportion
    p = p + ggplot2::labs(y="Log Odds")
  }

  for (i in seq_along(p$layers)) {
    layer = p$layers[[i]]
    curve = layer$data
    supplied_curve = inherits(layer$geom, "GeomLine") && is.data.frame(curve) &&
      all(c("prediction", "model") %in% names(curve))
    if (!supplied_curve) {
      if (scale != "logit" || !is.data.frame(curve)) next
      # Invisible probability observations/anchors must not train a log-odds axis.
      if (inherits(layer$geom, c("GeomPoint", "GeomBlank")) && outcome %in% names(curve)) {
        layer$data = curve[FALSE, , drop=FALSE]
      } else if (inherits(layer$geom, "GeomLine") && "y" %in% names(curve)) {
        layer$data$y = stats::qlogis(curve$y)
      }
      next
    }

    curve$ymin = curve$ymax = curve$prediction
    if (se) {
      # Link-scale delta intervals reduce to predict.glm's usual intervals when
      # all predictors are displayed, and also support standardized means.
      eta = object$family$linkfun(curve$prediction)
      link_se = curve$se/object$family$mu.eta(eta)
      curve$ymin = object$family$linkinv(eta - stats::qnorm(.975)*link_se)
      curve$ymax = object$family$linkinv(eta + stats::qnorm(.975)*link_se)
    }
    if (scale == "logit") {
      curve$prediction = stats::qlogis(curve$prediction)
      curve$ymin = stats::qlogis(curve$ymin)
      curve$ymax = stats::qlogis(curve$ymax)
    }
    mapping = ggplot2::aes(x=.data[[xvar]], y=.data$prediction,
                           ymin=.data$ymin, ymax=.data$ymax)
    for (aesthetic in c("colour", "linetype", "group")) {
      if (!is.null(p$mapping[[aesthetic]])) mapping[[aesthetic]] = p$mapping[[aesthetic]]
    }
    p$layers[[i]] = ggplot2::geom_smooth(data=curve, mapping=mapping,
                         stat="identity", se=se, inherit.aes=FALSE)
  }
  add_logistic_overlay_layers(p, summary, overlay_type, scale)
}
