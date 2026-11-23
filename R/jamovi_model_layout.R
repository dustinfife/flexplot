# Display roles are constrained by fitted formulas, never the variable-selection pool.
jamovi_model_layout = function(models, data, x=NULL, panels=NULL, group=NULL,
                                comparison=FALSE, allow_magnet=FALSE) {
  predictors = unique(unlist(lapply(models, function(model) all.vars(stats::formula(model))[-1])))
  requested = c(x, panels, if (!comparison) group)
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
  note = NULL
  if (comparison && length(group) && length(x)==1L && !is.na(x) &&
      !(allow_magnet && jamovi_comparison_magnet(data, outcome, x) &&
        all(group %in% predictors) && all(group %in% names(data)) &&
        length(group)==1L && !anyDuplicated(c(x, panels, group)))) {
    note = paste0("Comparison grouping (", paste(group, collapse=", "),
      ") is not applied to this comparison layout. Colors, line types and symbols identify the models. ",
      "The plot uses x and panels only; the saved grouping choice is retained for supported visualizations.")
    group = NULL
  }
  if (length(x) != 1L || is.na(x) || length(group) > 1L || length(panels) > 2L ||
      anyDuplicated(c(x, group, panels))) {
    stop("Choose different model predictors for one x variable, one optional group, and up to two panels.", call.=FALSE)
  }
  rhs = as.name(x)
  if (length(group)) rhs = call("+", rhs, as.name(group))
  if (length(panels)) rhs = call("|", rhs, formula_component(lapply(panels, as.name)))
  formula = make.formula(as.name(outcome), rhs)
  if (!is.null(note)) attr(formula, "jamovi_comparison_note") = note
  formula
}

# Native magnets distinguish models in facets, so category grouping is safe there.
jamovi_comparison_magnet = function(data, outcome, x) {
  length(x)==1L && x %in% names(data) && outcome %in% names(data) &&
    is_binary(data[[outcome]]) && check.non.number(data[[x]])
}

jamovi_comparison_layout = function(models, data, x=NULL, panels=NULL, group=NULL, allow_magnet=FALSE) {
  jamovi_model_layout(models, data, x, panels, group, comparison=TRUE, allow_magnet=allow_magnet)
}

# Publish a derived UI capability from the actual fitted common sample, including
# numeric 0/1 outcomes whose continuous-column metadata has no categorical levels.
jamovi_comparison_publish_magnet = function(options, available=FALSE) {
  option = options$option("comparisonMagnet")
  option$value = isTRUE(available)
  pb = options$asProtoBuf()
  if (is.null(pb)) return(invisible(NULL))
  values = pb$options
  if (!length(values)) return(invisible(NULL))
  index = match("comparisonMagnet", pb$names)
  if (is.na(index)) {
    index = length(values)+1L
    pb$names = c(pb$names, "comparisonMagnet")
  }
  item = values[[1]]$clone(); item$clear()
  item$o = if (isTRUE(available)) 1L else 0L
  values[[index]] = item
  pb$options = values
  invisible(NULL)
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
