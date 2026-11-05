flexplot_sampling_levels = function(values) {
  if (is.factor(values)) return(levels(droplevels(values)))
  sort(unique(values))
}

flexplot_sampling_discrete_limits = function(trained, full_levels) {
  union(full_levels, trained)
}

flexplot_sampling_colour_limits = function(trained, full_range) {
  range(c(trained, full_range), finite=TRUE)
}

flexplot_sampling_dodge_params = function(self, data) {
  params = ggplot2::ggproto_parent(PositionJitterdodged, self)$setup_params(data)
  params$jitter.width = self$jitter.width / (length(self$group_levels) + 2)
  params
}

flexplot_sampling_dodge_panel = function(self, data, params, scales) {
  index = match(as.character(data$colour), self$group_levels)
  coordinate = if (params$flipped_aes) "y" else "x"
  data[[coordinate]] = data[[coordinate]] +
    params$dodge.width * ((index - .5) / length(self$group_levels) - .5)
  # Keep the existing density-based jitter, reserving slots before its collision
  # step so missing sampled groups cannot recenter the remaining observations.
  params$dodge.width = 0
  ggplot2::ggproto_parent(PositionJitterdodged, self)$compute_panel(data, params, scales)
}

flexplot_sampling_scale = function(p, aesthetic, values) {
  force(values)
  scale = p$scales$get_scales(aesthetic)
  if (!is.null(scale) && !is.null(scale$limits)) return(p)
  limits = purrr::partial(flexplot_sampling_discrete_limits,
                           full_levels=flexplot_sampling_levels(values))
  continuous_colour = aesthetic == "colour" && is.numeric(values)
  if (continuous_colour) {
    limits = purrr::partial(flexplot_sampling_colour_limits, full_range=range(values, finite=TRUE))
  }
  if (!is.null(scale)) {
    scale$limits = limits
    return(p)
  }
  constructor = switch(
    aesthetic,
    x=ggplot2::scale_x_discrete,
    colour=if (continuous_colour) ggplot2::scale_colour_continuous else
      if (is.ordered(values)) ggplot2::scale_colour_ordinal else ggplot2::scale_colour_discrete,
    shape=ggplot2::scale_shape_discrete,
    linetype=ggplot2::scale_linetype_discrete
  )
  p + constructor(limits=limits)
}

flexplot_sampling_structure = function(p, data, outcome, axis, sample,
                                        related=FALSE, use_magnet=FALSE) {
  if (related || identical(axis[1], "1")) return(p)
  if (!is.numeric(data[[outcome]])) return(p)
  axis = flexplot_object_axis(data, axis)
  x = axis[1]

  if (!is.numeric(data[[x]])) {
    p = flexplot_sampling_scale(p, "x", data[[x]])
  }
  if (length(axis) > 1 && !is.na(axis[2])) {
    group = axis[2]
    for (aesthetic in c("colour", "shape", "linetype")) {
      if (is.null(p$mapping[[aesthetic]])) next
      values = rlang::eval_tidy(p$mapping[[aesthetic]], data=data)
      p = flexplot_sampling_scale(p, aesthetic, values)
    }
    if (!use_magnet && !is.numeric(data[[x]])) {
      for (i in seq_along(p$layers)) {
        position = p$layers[[i]]$position
        if (!inherits(position, "PositionJitterdodged")) next
        # Snapshot the parent rather than retaining ggproto's lazy loop binding.
        p$layers[[i]]$position = do.call(
          ggplot2::ggproto,
          list(NULL, position, group_levels=flexplot_sampling_levels(data[[group]]),
               setup_params=flexplot_sampling_dodge_params,
               compute_panel=flexplot_sampling_dodge_panel)
        )
      }
    }
  }
  if (use_magnet || !is.finite(sample)) return(p)
  # Train axes and facets with actual eligible observations, without drawing
  # extra points or constraining fitted layers. Later user scales/coordinates
  # retain their normal limits and expansion behavior.
  p + ggplot2::geom_blank(
    data=data, mapping=ggplot2::aes(x=.data[[x]], y=.data[[outcome]]),
    inherit.aes=FALSE, show.legend=FALSE
  )
}
