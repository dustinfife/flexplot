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
