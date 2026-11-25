# Compatibility surface: preserve Mixed Models' saved keys and option names.
jamovi_mixed_label_row = function(role, variable, value="", display)
  jamovi_plot_label_row(role,variable,value,display)

jamovi_mixed_label_catalog = function(plot, layout) {
  if (!is.null(attr(plot,"jamovi_cas_children"))) plot = attr(plot,"jamovi_cas_children")[[1]]
  layout$comparison = isTRUE(attr(plot,"jamovi_mixed_comparison"))
  jamovi_plot_label_catalog(plot,layout)
}

jamovi_mixed_sync_labels = function(options, plot=NULL, layout=NULL, comparison=FALSE) {
  if (!is.null(plot) && !is.null(attr(plot,"jamovi_cas_children"))) plot = attr(plot,"jamovi_cas_children")[[1]]
  if (!is.null(plot)) layout$comparison = isTRUE(attr(plot,"jamovi_mixed_comparison"))
  jamovi_plot_sync_labels(options,plot,layout,
    rows=if(comparison) "comparisonPlotLabels" else "plotLabels",
    retained=if(comparison) "comparisonPlotLabelStore" else "plotLabelStore")
}

jamovi_mixed_apply_labels = function(plot, entries) {
  children = attr(plot,"jamovi_cas_children")
  if (!is.null(children)) return(jamovi_mixed_cas_compose(lapply(children,jamovi_mixed_apply_labels,entries=entries)))
  plot = jamovi_plot_apply_labels(plot,entries)
  titles = Filter(function(entry)entry$role %in% c("title","subtitle") && jamovi_plot_nonblank_label(entry),entries)
  for (entry in titles) plot = plot + do.call(ggplot2::labs,setNames(list(entry$label),entry$role))
  plot
}

jamovi_mixed_remember_labels = function(...) jamovi_plot_remember_labels(...)
jamovi_mixed_strip_labels = function(...) jamovi_plot_strip_labels(...)
jamovi_mixed_nonblank_label = function(...) jamovi_plot_nonblank_label(...)
jamovi_mixed_legend_label = function(...) jamovi_plot_legend_label(...)
jamovi_mixed_facet_label = function(...) jamovi_plot_facet_label(...)
jamovi_mixed_scale_matches = function(...) jamovi_plot_scale_matches(...)
jamovi_mixed_publish_labels = function(...) jamovi_plot_publish_labels(...)
jamovi_mixed_layer_aesthetics = function(...) jamovi_plot_layer_aesthetics(...)
jamovi_mixed_axis_key = function(...) jamovi_plot_axis_key(...)
jamovi_mixed_layer_models = function(...) jamovi_plot_layer_models(...)
