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
    metadata = flexplot_ghost_reference_metadata(source,roles$given)
    coordinates = flexplot_ghost_prediction_data(source,roles$axis,roles$given,plot$data)
    result = plot + flexplot_ghost_layer(coordinates,"gray")
    attr(result,"ghost_reference") = metadata
    result
  }
  reason = jamovi_model_ghost_reason(plot,formula)
  if (is.null(reason)) {
    ghost = tryCatch(render_ghost(),error=identity)
    if (inherits(ghost,"error")) {
      reason = conditionMessage(ghost)
    } else {
      layers = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),ghost$layers)
      valid = vapply(layers,function(layer) {
        flexplot_ghost_drawable(layer$data,flexplot_magnet_display_given(ghost$data,
          flexplot_axis_given(formula)$given))
      },logical(1))
      if (any(valid)) {
        metadata = attr(ghost,"ghost_reference")
        metadata$drawable = TRUE
        metadata$style = list(colour="#707070",linewidth=1.2,alpha=.85,linetype="dashed")
        ghost = jamovi_model_style_ghosts(ghost)
        attr(ghost,"ghost_reference") = metadata
        return(ghost)
      }
      reason = "The reference panel has no finite prediction curve to copy."
    }
  }
  attr(plot,"jamovi_ghost_note") = paste("Ghost lines unavailable.",reason)
  plot
}

# Use the same labeller as the displayed facets, after saved label edits apply.
jamovi_model_ghost_note = function(plot) {
  failure = attr(plot,"jamovi_ghost_note")
  if (length(failure)) return(jamovi_plot_note_paragraph(failure))
  reference = attr(plot,"ghost_reference")
  if (!isTRUE(reference$drawable) || !length(reference$columns)) return("")
  labels = as.data.frame(setNames(reference$levels,reference$columns),check.names=FALSE)
  labeller = plot$facet$params$labeller
  if (is.null(labeller)) labeller = custom.labeler
  rendered = labeller(labels)
  text = vapply(rendered,function(value) paste(gsub("\n"," ",sub(":\n"," = ",as.character(value),fixed=TRUE)),collapse=", "),character(1))
  jamovi_plot_note_paragraph(paste0("Dashed gray lines show the fitted curve from the panel where ",
    paste(text,collapse=" and "),"."))
}

jamovi_plot_note_paragraph = function(text) {
  paste0('<p style="margin:0 0 4px 0;">',jamovi_html_escape(text),"</p>")
}

jamovi_plot_compact_notes = function(html) {
  gsub("<p>",'<p style="margin:0 0 4px 0;">',html,fixed=TRUE)
}

jamovi_model_publish_ghost_note = function(result,plot,visible=TRUE,guide="") {
  content = paste0(jamovi_plot_compact_notes(guide),jamovi_model_ghost_note(plot))
  result$setContent(content)
  result$setVisible(isTRUE(visible) && nzchar(content))
  invisible(NULL)
}
