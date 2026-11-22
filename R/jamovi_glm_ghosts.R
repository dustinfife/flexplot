# This gate inspects native prepared data; it does not choose reference values.
jamovi_glm_ghost_reason = function(plot, formula) {
  roles = flexplot_axis_given(formula)
  panels = roles$given[!is.na(roles$given)]
  if (length(roles$axis)>1L) return("Use panels instead of a color / symbol variable for ghost lines.")
  if (!length(panels)) return("Select at least one panel variable for ghost lines.")
  x = roles$axis[1]
  if (!is.numeric(plot$data[[x]])) return("Ghost lines require a continuous numeric x variable.")
  panels = flexplot_magnet_display_given(plot$data,panels)
  if (nrow(unique(plot$data[panels]))<2L) return("Ghost lines require at least two prepared panels.")
  curves = Filter(function(layer) inherits(layer$geom,c("GeomLine","GeomSmooth")) &&
    is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
  if (!length(curves) || !any(vapply(curves,function(layer)
      sum(is.finite(layer$data$prediction))>=2L,logical(1))))
    return("No finite fitted prediction curve is available for ghost lines.")
  NULL
}

# Reuse the normal renderer and its core supplied-prediction ghosts. The ordinary
# plot is retained if the requested reference/layout cannot produce a ghost.
jamovi_glm_generalized_plot = function(state, x=NULL, panels=NULL, group=NULL, style="auto",
                                       se=TRUE, scale="probability", ghostLines=FALSE, ghost.reference=NULL) {
  arguments = list(state=state,x=x,panels=panels,group=group,style=style,se=se,scale=scale)
  plot = do.call(jamovi_glm_generalized_render,arguments)
  if (!isTRUE(ghostLines)) return(plot)
  formula = jamovi_model_layout(list(state$model),state$data,x,panels,group)
  reason = jamovi_glm_ghost_reason(plot,formula)
  if (is.null(reason)) {
    ghost = tryCatch(do.call(jamovi_glm_generalized_render,
      c(arguments,list(ghost.line="gray",ghost.reference=ghost.reference))),error=identity)
    if (inherits(ghost,"error")) {
      reason = conditionMessage(ghost)
    } else {
      layers = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),ghost$layers)
      valid = vapply(layers,function(layer) {
        axes = attr(layer$data,"ghost_axes")
        values = layer$data
        finite = is.finite(values[[axes[1]]]) & is.finite(values[[axes[2]]])
        length(unique(values[[axes[1]]][finite]))>=2L
      },logical(1))
      if (any(valid)) return(ghost)
      reason = "The reference panel has no finite prediction curve to copy."
    }
  }
  attr(plot,"jamovi_ghost_note") = paste("Ghost lines unavailable.",reason)
  plot
}
