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

# Style only adapter-owned ghosts, after interval layers have been constructed.
jamovi_glm_style_ghosts = function(plot) {
  ghosts = which(vapply(plot$layers,function(layer)
    !is.null(attr(layer$data,"ghost_axes")),logical(1)))
  if (!length(ghosts)) return(plot)
  curves = which(vapply(plot$layers,function(layer)
    inherits(layer$geom,c("GeomLine","GeomSmooth")) && is.data.frame(layer$data) &&
      "prediction" %in% names(layer$data),logical(1)))
  color = "gray40"
  if (length(curves)) {
    curve = plot$layers[[curves[1]]]
    mapped = !is.null(curve$mapping$colour) ||
      (isTRUE(curve$inherit.aes) && !is.null(plot$mapping$colour))
    colors = curve$aes_params$colour
    if (mapped || !length(colors)) {
      # Resolve theme/scale colors using only the supplied fitted curve: no
      # observation jitter or smoothing calculation is needed for this lookup.
      reference = plot
      reference$layers = plot$layers[curves[1]]
      colors = unique(ggplot2::ggplot_build(reference)$data[[1]]$colour)
    }
    if (is.character(colors) && length(colors)==1L && !is.na(colors)) color = colors
  }
  for (i in ghosts) {
    layer = local({
      original = plot$layers[[i]]
      ggplot2::ggproto(NULL,original)
    })
    layer$mapping$colour = layer$mapping$linetype = layer$mapping$alpha = layer$mapping$linewidth = NULL
    layer$aes_params = utils::modifyList(layer$aes_params,
      list(colour=color,linewidth=1.2,alpha=.85,linetype="dashed"))
    plot$layers[[i]] = layer
  }
  plot$layers = c(plot$layers[-ghosts],plot$layers[ghosts])
  plot
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
      if (any(valid)) return(jamovi_glm_style_ghosts(ghost))
      reason = "The reference panel has no finite prediction curve to copy."
    }
  }
  attr(plot,"jamovi_ghost_note") = paste("Ghost lines unavailable.",reason)
  plot
}
