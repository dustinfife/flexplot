# Display transformation only: probabilities and their uncertainty are calculated
# by the existing fitted-model machinery before entering this function.
jamovi_glm_logit_display = function(plot, outcome, transform=TRUE) {
  values = numeric()
  for (i in seq_along(plot$layers)) {
    layer = plot$layers[[i]]
    data = layer$data
    if (!is.data.frame(data)) next
    if ("prediction" %in% names(data)) {
      if (transform && (anyNA(data$prediction) || any(data$prediction < 0 | data$prediction > 1)))
        stop("The logit prediction grid contains invalid probabilities.",call.=FALSE)
      displayed = c("prediction","prediction.fit")
      if (inherits(layer$geom,c("GeomRibbon","GeomErrorbar")))
        displayed = c(displayed,"prediction.lwr","prediction.upr")
      if (transform) for (name in intersect(displayed,names(data)))
        data[[name]] = stats::qlogis(data[[name]])
      if (anyNA(data$prediction)) stop("The logit prediction grid contains invalid probabilities.",call.=FALSE)
      bounds = if (inherits(layer$geom,"GeomSmooth")) intersect(c("ymin","ymax"),names(data)) else
        if (inherits(layer$geom,c("GeomRibbon","GeomErrorbar"))) intersect(c("prediction.lwr","prediction.upr"),names(data)) else character()
      if (any(vapply(data[bounds],anyNA,logical(1)))) stop("Logit confidence limits are undefined at saturated probabilities. Disable Confidence intervals or use Probability scale.",call.=FALSE)
      values = c(values,data$prediction)
    } else if (inherits(layer$geom,c("GeomPoint","GeomBlank")) && outcome %in% names(data)) {
      data = data[FALSE,,drop=FALSE] # Invisible raw observations cannot train a logit axis.
    } else if (transform && inherits(layer$geom,"GeomLine") && "y" %in% names(data)) {
      data$y = stats::qlogis(data$y)
    }
    plot$layers[[i]]$data = data
  }
  if (!length(values) || !any(is.finite(values))) {
    stop("No finite logit predictions are available for this plotting grid. Use Probability scale or revise the plot variables.",call.=FALSE)
  }
  # Factor outcome labels and probability anchors are inappropriate on this axis.
  plot$scales$scales = Filter(function(scale)!"y" %in% scale$aesthetics,plot$scales$scales)
  plot$coordinates$limits$y = NULL
  plot + ggplot2::scale_y_continuous(name="Logit (log odds)")
}

# Resolve stale saved/UI display options against the actual estimation data.
jamovi_glm_visualization = function(data, x, binomial, style, scale) {
  scale = match.arg(scale, c("probability", "logit"))
  continuous = length(x) == 1L && !is.na(x) && x %in% names(data) &&
    is.numeric(data[[x]]) && length(unique(data[[x]][!is.na(data[[x]])])) >= 10L
  overlay = binomial && continuous && style == "auto"
  fallback = binomial && scale == "logit" && !overlay
  list(overlay=overlay, scale=if (fallback) "probability" else scale,
    note=if (fallback) "Probability scale is used because Logit requires an automatic overlay with a continuous x predictor." else "")
}

# Returning FALSE leaves jmvcore without an image path. Render a diagnostic
# while retaining the explicit error state, so errors cannot look like loading.
jamovi_glm_plot_error = function(image, error) {
  image$setError(conditionMessage(error))
  grid::grid.newpage()
  grid::grid.text(paste(strwrap(conditionMessage(error), width=75), collapse="\n"),
    gp=grid::gpar(col="grey30", fontsize=12))
  TRUE
}
