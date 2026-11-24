# This gate inspects native prepared data; it does not choose reference values.
jamovi_model_ghost_reason = function(plot, formula) {
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
jamovi_model_style_ghosts = function(plot) {
  ghosts = which(vapply(plot$layers,function(layer)
    !is.null(attr(layer$data,"ghost_axes")),logical(1)))
  if (!length(ghosts)) return(plot)
  for (i in ghosts) {
    layer = local({
      original = plot$layers[[i]]
      ggplot2::ggproto(NULL,original)
    })
    layer$mapping$colour = layer$mapping$linetype = layer$mapping$alpha = layer$mapping$linewidth = NULL
    layer$aes_params = utils::modifyList(layer$aes_params,
      list(colour="#707070",linewidth=1.2,alpha=.85,linetype="dashed"))
    plot$layers[[i]] = layer
  }
  plot$layers = c(plot$layers[-ghosts],plot$layers[ghosts])
  plot
}

# The optional renderer retains GLM's existing transform path. Other adapters copy
# the supplied fitted curve already in the plot, without sampling or predicting again.
jamovi_model_reference_ghosts = function(plot, formula, ghostLines=FALSE,
                                         ghost.reference=NULL, render_ghost=NULL) {
  if (!isTRUE(ghostLines)) return(plot)
  if (is.null(render_ghost)) render_ghost = function() {
    roles = flexplot_axis_given(formula)
    curves = Filter(function(layer) inherits(layer$geom,c("GeomLine","GeomSmooth")) &&
      is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
    if (length(curves)!=1L) stop("The visualization does not have one supplied fitted prediction stream.",call.=FALSE)
    reference = create_ghost_reference(data=as.data.frame(plot$data),given=roles$given,axis=roles$axis,
      ghost.reference=ghost.reference)
    source = flexplot_ghost_reference_data(curves[[1]]$data,roles$given,reference,roles$axis)
    coordinates = flexplot_ghost_prediction_data(source,roles$axis,roles$given,plot$data)
    plot + flexplot_ghost_layer(coordinates,"gray")
  }
  reason = jamovi_model_ghost_reason(plot,formula)
  if (is.null(reason)) {
    ghost = tryCatch(render_ghost(),error=identity)
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
      if (any(valid)) return(jamovi_model_style_ghosts(ghost))
      reason = "The reference panel has no finite prediction curve to copy."
    }
  }
  attr(plot,"jamovi_ghost_note") = paste("Ghost lines unavailable.",reason)
  plot
}
