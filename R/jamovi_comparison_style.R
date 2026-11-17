# Presentation only: retain prediction identifiers, values, and group mappings.
jamovi_comparison_style = function(plot) {
  models = character()
  for (layer in plot$layers) {
    if (is.data.frame(layer$data) && "model" %in% names(layer$data)) {
      models = union(models, as.character(layer$data$model))
    }
  }
  if (length(models) != 2L) return(plot)
  models = sort(models)
  labels = stats::setNames(c("Model 1", "Model 2"), models)
  colour = plot$scales$get_scales("colour")
  if (is.null(colour)) {
    plot = plot + ggplot2::scale_colour_discrete(name="Model", labels=labels)
  } else {
    colour$name = "Model"
    colour$labels = labels
  }
  linetype = plot$scales$get_scales("linetype")
  if (!is.null(linetype)) {
    linetype$name = "Model"
    linetype$labels = labels
    linetype$palette = ggplot2::scale_linetype_manual(values=c("solid", "dashed"))$palette
  }
  plot
}
