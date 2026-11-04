flexplot_ghost_fit_layer = function(p) {
  is_fit = vapply(
    p$layers,
    function(layer) {
      inherits(layer$stat, "StatSmooth") ||
        inherits(layer$stat, "StatSummary")
    },
    logical(1)
  )
  
  which(is_fit)
}


flexplot_ghost_layer = function(d_smooth, axis, outcome,
                                ghost.line, group=NULL) {
  
  names(d_smooth)[names(d_smooth) == "x"] = axis[1]
  names(d_smooth)[names(d_smooth) == "y"] = outcome
  
  mapping = ggplot2::aes(
    x=.data[[axis[1]]],
    y=.data[[outcome]]
  )
  
  if (!is.null(group)) {
    mapping = ggplot2::aes(
      x=.data[[axis[1]]],
      y=.data[[outcome]],
      group=.data[[group]],
      linetype=.data[[group]]
    )
  }
  
  ggplot2::geom_line(
    data=d_smooth,
    mapping=mapping,
    inherit.aes=FALSE,
    color=ghost.line,
    show.legend=FALSE
  )
}


flexplot_ghost_reference_data = function(data, given,
                                         ghost.reference) {
  
  k = data
  
  for (variable in given) {
    if (is.na(variable)) next
    
    reference = ghost.reference[[variable]]
    if (is.null(reference)) next
    
    column = variable
    
    if (is.numeric(k[[variable]])) {
      binned = paste0(variable, "_binned")
      if (binned %in% names(k)) column = binned
    }
    
    k = k[
      !is.na(k[[column]]) &
        as.character(k[[column]]) %in% as.character(reference),
      ,
      drop=FALSE
    ]
  }
  
  if (nrow(k) == 0) {
    stop("No observations match the ghost reference.")
  }
  
  k
}


flexplot_ghost_smooth_data = function(reference_data,
                                      outcome, axis, given,
                                      method, se,
                                      plot.type, bins, spread,
                                      jitter=NULL) {
  
  p = flexplot_render_object(
    data=reference_data,
    outcome=outcome,
    axis=axis,
    given=given,
    method=method,
    se=se,
    suppress_smooth=FALSE,
    plot.type=plot.type,
    bins=bins,
    spread=spread,
    jitter=jitter
  )
  
  fit_layers = flexplot_ghost_fit_layer(p)
  
  if (length(fit_layers) != 1) {
    stop(
      "Expected exactly one fitted layer for the ghost line."
    )
  }
  
  ggplot2::ggplot_build(p)$data[[fit_layers]]
}
