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

flexplot_ghost_layer = function(d_smooth, ghost.line) {
  
  ggplot2::geom_line(
    data=d_smooth,
    mapping=ggplot2::aes(x=.data$x, y=.data$y),
    inherit.aes=FALSE,
    color=ghost.line,
    show.legend=FALSE
  )
}



flexplot_ghost_reference_data = function(data, given,
                                         ghost.reference,
                                         axis=NULL) {
  
  k = data
  
  variables = given[!is.na(given)]
  
  if (length(axis) > 1 &&
      axis[2] %in% names(ghost.reference)) {
    variables = unique(c(variables, axis[2]))
  }
  
  for (variable in variables) {
    
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
                                      method, 
                                      plot.type, bins, spread,
                                      jitter=NULL) {
  
  p = flexplot_render_object(
    data=reference_data,
    outcome=outcome,
    axis=axis,
    given=given,
    method=method,
    se=F,
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
  
  d_smooth = ggplot2::ggplot_build(p)$data[[fit_layers]]
  
  return(d_smooth[, c("x", "y"), drop=FALSE])
}
