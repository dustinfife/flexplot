# Historical documents without explicit modes retain the pre-CAS default.
jamovi_mixed_visualization_restore = function(options) {
  pb = options$asProtoBuf()
  if (is.null(pb)) return(invisible(NULL))
  for (name in c("predictionType","comparisonPredictionType")) {
    if (!name %in% pb$names) jamovi_output_option(options,name,"cluster")
  }
  invisible(NULL)
}

# Report the actual selected clusters, not the requested upper bound.
jamovi_mixed_plot_guide = function(plot, state, type, comparison=FALSE) {
  if (type=="cas") return(jamovi_plot_note_paragraph(if(comparison)
    "Each panel removes that model's fitted random effects from the observations." else
    "All fitted random effects are removed from the observations."))
  if (type!="cluster") return("")
  data = plot$data
  if (!state$cluster %in% names(data)) return("")
  selected = length(unique(as.character(data[[state$cluster]])))
  total = length(unique(as.character(state$data[[state$cluster]])))
  if (!selected) return("")
  text = if(selected>=total) paste("All",selected,"clusters are displayed.") else
    paste(selected,if(selected==1L)"cluster was" else "clusters were","randomly selected.")
  jamovi_plot_note_paragraph(text)
}


# Follow compare_fits' single-model convention without hiding predictor legends
# or genuinely distinct prediction streams.
jamovi_mixed_style_plot = function(plot, type, group=NULL) {
  signature = list(type=type,group=group)
  if (identical(attr(plot,"jamovi_mixed_presentation"),signature)) return(plot)
  frames = c(list(plot$data),lapply(plot$layers,function(layer)layer$data))
  streams = unique(unlist(lapply(frames,function(data)
    if(is.data.frame(data) && "model" %in% names(data)) as.character(data$model))))
  if (!length(group) && length(streams)<=1L) plot = plot + ggplot2::theme(legend.position="none")
  plot = plot + ggplot2::labs(title="Model Visualization",subtitle=switch(type,
    cluster="Cluster-specific predictions",population="Population-level predictions",cas="Cluster-adjusted scatterplot"))
  attr(plot,"jamovi_mixed_presentation") = signature
  plot
}
