flexplot_ghost_fit_layer = function(p) {
  is_fit = vapply(
    p$layers,
    function(layer) {
      inherits(layer$stat, "StatSmooth") ||
        inherits(layer$stat, "StatSummary") ||
        (inherits(layer$geom, "GeomLine") && inherits(layer$stat, "StatIdentity"))
    },
    logical(1)
  )
  
  which(is_fit)
}

flexplot_ghost_layer = function(d_smooth, ghost.line, reference_groups=NULL) {
  axes = attr(d_smooth, "ghost_axes")
  if (is.null(axes)) axes = c("x", "y")
  group_var = attr(d_smooth, "ghost_group")
  if (is.null(group_var)) group_var = setdiff(names(d_smooth), axes)
  mapping = ggplot2::aes(x=.data[[axes[1]]], y=.data[[axes[2]]], group=.data[[group_var]])
  if (is.factor(d_smooth[[group_var]])) {
    mapping$linetype = mapping$group
  } else if (!is.null(reference_groups)) {
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

flexplot_ghost_coordinates = function(fitted, reference_data) {
  fitted = fitted[, c("x", "y", "group"), drop=FALSE]
  group_names = make.unique(c(names(reference_data), ".ghost_group"))
  names(fitted)[3] = group_names[length(group_names)]
  fitted
}

flexplot_ghost_categorical_prediction = function(reference_data, axis, given, data,
                                                 use_magnet, comparison) {
  categories = dplyr::summarise(
    dplyr::group_by(reference_data, .data$model),
    n=dplyr::n_distinct(.data[[axis[1]]]), .groups="drop"
  )
  if (any(categories$n < 2)) {
    stop("Categorical prediction ghost lines require at least two x categories for each model in the reference panel.",
         call.=FALSE)
  }
  # Reuse the ordinary markers' dodge/slot calculation without fitting a model
  # or evaluating predictions again. Explicit model groups retain curve identity.
  point = flexplot_prediction_layer(reference_data, axis, data, use_magnet, given, comparison)[[1]]
  point$mapping$group = ggplot2::aes(group=.data$model)$group
  p = ggplot2::ggplot(data) + point +
    ggplot2::scale_x_discrete(limits=flexplot_sampling_levels(data[[axis[1]]]))
  fitted = ggplot2::ggplot_build(p)$data[[1]]
  models = flexplot_sampling_levels(reference_data$model)
  fitted$group = factor(models[fitted$group], levels=levels(reference_data$model))
  fitted
}

flexplot_ghost_prediction_data = function(reference_data, axis, given, data,
                                           use_magnet=FALSE, comparison=FALSE) {
  display_given = flexplot_magnet_display_given(reference_data, given)
  if (any(vapply(reference_data[display_given], dplyr::n_distinct, integer(1)) != 1)) {
    stop("Prediction ghost lines require one reference category or bin per conditioning variable.",
         call.=FALSE)
  }
  # Average duplicate x/model rows using predictions already calculated by
  # the comparison's chosen prediction method.
  if (is.numeric(data[[axis[1]]])) {
    grouped = dplyr::group_by(reference_data, .data$model, .data[[axis[1]]])
    centers = dplyr::summarise(grouped, prediction=mean(.data$prediction), .groups="drop")
    fitted = data.frame(x=centers[[axis[1]]], y=centers$prediction, group=centers$model)
  } else {
    fitted = flexplot_ghost_categorical_prediction(reference_data, axis, given, data,
                                                   use_magnet, comparison)
  }
  coordinates = flexplot_ghost_coordinates(fitted, reference_data)
  axes = tail(make.unique(c(names(reference_data), ".ghost_x", ".ghost_y")), 2)
  names(coordinates)[1:2] = axes
  group = names(coordinates)[3]

  # Give ghosts explicit destination facets so they cannot cover the ordinary
  # prediction in the reference panel. Use full data, independently of sampling.
  panels = expand.grid(lapply(data[display_given], unique), KEEP.OUT.ATTRS=FALSE,
                       stringsAsFactors=FALSE)
  is_reference = rep(TRUE, nrow(panels))
  for (variable in display_given) {
    is_reference = is_reference &
      as.character(panels[[variable]]) == as.character(reference_data[[variable]][1])
  }
  panels = panels[!is_reference, , drop=FALSE]
  coordinates = merge(coordinates, panels, by=NULL, sort=FALSE)
  attr(coordinates, "ghost_axes") = axes
  attr(coordinates, "ghost_group") = group
  coordinates
}

flexplot_ghost_categorical_data = function(reference_data, full_data, outcome, axis,
                                            given, ghost.reference, spread, plot.type) {
  if (plot.type %in% c("boxplot", "violin", "line")) {
    stop("Categorical ghost summaries require a summary plot, not plot.type='",
         plot.type, "'.", call.=FALSE)
  }
  if (any(lengths(ghost.reference[given]) != 1)) {
    stop("Categorical ghost summaries require one reference category or bin per conditioning variable.",
         call.=FALSE)
  }
  display_axis = flexplot_object_axis(full_data, axis)
  x = display_axis[1]
  group = if (length(display_axis) > 1) display_axis[2] else NULL
  mapping = ggplot2::aes(x=.data[[x]], y=.data[[outcome]], group=1)
  if (!is.null(group)) mapping$group = ggplot2::aes(group=.data[[group]])$group

  # Ghost summaries use the summary-point centers and their dodge positions.
  # Extract just that layer, reusing the ordinary summary statistic unchanged.
  p = ggplot2::ggplot(reference_data, mapping) +
    flexplot_categorical_summary_layers(spread, group)[[1]] +
    ggplot2::scale_x_discrete(limits=flexplot_sampling_levels(full_data[[x]]))
  fitted = ggplot2::ggplot_build(p)$data[[1]]

  # Select second-axis references after positioning, so a selected group keeps
  # its original reference-panel slot and group ID rather than being recentered.
  if (!is.null(group) && !is.null(ghost.reference[[axis[2]]])) {
    groups = flexplot_sampling_levels(reference_data[[group]])
    keep = as.character(groups[fitted$group]) %in% as.character(ghost.reference[[axis[2]]])
    fitted = fitted[keep, , drop=FALSE]
    if (!nrow(fitted)) stop("No observations match the ghost reference.", call.=FALSE)
  }
  coordinates = flexplot_ghost_coordinates(fitted, reference_data)
  # A conditioning variable may itself be named x or y. Keep coordinate fields
  # distinct so ggplot repeats the reference instead of treating it as facet data.
  axes = tail(make.unique(c(names(reference_data), ".ghost_x", ".ghost_y")), 2)
  names(coordinates)[1:2] = axes
  attr(coordinates, "ghost_axes") = axes
  coordinates
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
  
  # Avoid treating fitted identifiers as facet values when a source column is
  # named group (or already uses our internal grouping name).
  flexplot_ghost_coordinates(d_smooth, reference_data)
}
