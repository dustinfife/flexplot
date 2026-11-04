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

flexplot_ghost_layer = function(d_smooth, ghost.line, reference_groups=NULL) {
  group_var = setdiff(names(d_smooth), c("x", "y"))
  mapping = ggplot2::aes(x=.data$x, y=.data$y, group=.data[[group_var]])
  if (!is.null(reference_groups)) {
    # Smooth group IDs follow the observed group order in the reference data.
    # Map back to category values so selected references use the full plot's scale.
    lookup = if (is.factor(reference_groups)) {
      factor(levels(droplevels(reference_groups)), levels=levels(reference_groups),
             ordered=is.ordered(reference_groups))
    } else {
      sort(unique(reference_groups))
    }
    names(lookup) = seq_along(lookup)
    mapping$linetype = ggplot2::aes(
      linetype=(!!lookup)[as.character(.data[[group_var]])]
    )$linetype
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
  
  d_smooth = d_smooth[, c("x", "y", "group"), drop=FALSE]
  # Avoid treating fitted identifiers as facet values when a source column is
  # named group (or already uses our internal grouping name).
  group_names = make.unique(c(names(reference_data), ".ghost_group"))
  names(d_smooth)[3] = group_names[length(group_names)]
  d_smooth
}
