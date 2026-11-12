# Display roles are constrained by fitted formulas, never the variable-selection pool.
jamovi_model_layout = function(models, data, x=NULL, panels=NULL, group=NULL) {
  predictors = unique(unlist(lapply(models, function(model) all.vars(stats::formula(model))[-1])))
  requested = c(x, panels, group)
  if (any(!requested %in% predictors) || any(!requested %in% names(data))) {
    stop("Visualization variables must be predictors in the fitted model (either fitted model for comparisons).", call.=FALSE)
  }
  outcome = all.vars(stats::formula(models[[1]]))[1]
  if (!length(predictors) && !length(requested)) return(make.formula(as.name(outcome), "1"))
  if (!length(x)) {
    available = setdiff(predictors, c(panels, group))
    numeric = available[vapply(data[available], is.numeric, logical(1))]
    x = if (length(numeric)) numeric[1] else available[1]
  }
  if (length(x) != 1L || is.na(x) || length(group) > 1L || length(panels) > 2L ||
      anyDuplicated(c(x, group, panels))) {
    stop("Choose different model predictors for one x variable, one optional group, and up to two panels.", call.=FALSE)
  }
  rhs = as.name(x)
  if (length(group)) rhs = call("+", rhs, as.name(group))
  if (length(panels)) rhs = call("|", rhs, formula_component(lapply(panels, as.name)))
  make.formula(as.name(outcome), rhs)
}

# Reuse prediction data and positions; glm bounds are transformed from the link scale.
jamovi_model_intervals = function(plot, se=TRUE, model=NULL) {
  if (!se) return(plot)
  for (layer in plot$layers) {
    data = layer$data
    if (!is.data.frame(data) ||
        !all(c("prediction", "prediction.lwr", "prediction.upr") %in% names(data))) next
    numeric_x = is.numeric(rlang::eval_tidy(layer$mapping$x, data=data))
    if (!is.null(model) && inherits(model, "glm") && inherits(layer$geom, "GeomPoint")) numeric_x = FALSE
    if (numeric_x && !inherits(layer$geom, "GeomLine")) next
    if (!numeric_x && !inherits(layer$geom, "GeomPoint")) next
    if (!is.null(model) && inherits(model, "glm")) {
      eta = model$family$linkfun(data$prediction)
      link_se = data$se/abs(model$family$mu.eta(eta))
      estimated = colnames(summary(model)$coefficients)[3] == "t value"
      critical = if (estimated) stats::qt(.975, stats::df.residual(model)) else stats::qnorm(.975)
      low = eta-critical*link_se
      high = eta+critical*link_se
      data$prediction.lwr = pmin(model$family$linkinv(low), model$family$linkinv(high))
      data$prediction.upr = pmax(model$family$linkinv(low), model$family$linkinv(high))
      if (model$family$family == "Gamma" && model$family$link == "inverse") {
        data$prediction.lwr = 1/high
        data$prediction.upr = ifelse(low <= 0, Inf, 1/low)
      }
      layer$data = data
    }
    mapping = layer$mapping
    mapping$shape = NULL
    mapping$ymin = ggplot2::aes(ymin=.data[["prediction.lwr"]])$ymin
    mapping$ymax = ggplot2::aes(ymax=.data[["prediction.upr"]])$ymax
    if (numeric_x) {
      mapping$fill = mapping$colour
      mapping$colour = mapping$linetype = NULL
      plot = plot + ggplot2::geom_ribbon(data=data, mapping=mapping, alpha=.2,
                                         inherit.aes=FALSE, show.legend=FALSE)
    } else {
      plot = plot + ggplot2::geom_errorbar(data=data, mapping=mapping, width=.1,
                      position=layer$position, inherit.aes=FALSE, show.legend=FALSE)
    }
  }
  plot
}
