flexplot_magnet_y_limits = function(limits) {
  range(c(0, 1, limits), na.rm=TRUE)
}

flexplot_magnet_y_scale = function(labels=NULL) {
  if (is.null(labels)) {
    return(ggplot2::scale_y_continuous(
      breaks=c(0, .25, .5, .75, 1), limits=flexplot_magnet_y_limits
    ))
  }
  ggplot2::scale_y_continuous(
    breaks=c(0, 1), labels=labels, limits=flexplot_magnet_y_limits
  )
}

flexplot_magnet_jitter = function(jitter) {
  if (is.null(jitter) || isTRUE(jitter)) return(c(.05, 0))
  if (is.numeric(jitter) && length(jitter) == 2) return(jitter)
  if (is.numeric(jitter) && length(jitter) == 1) return(c(jitter, 0))
  match_jitter_categorical(jitter)
}

flexplot_magnet_display_given = function(data, given) {
  display_given = given
  for (i in seq_along(given)) {
    variable = given[i]
    if (is.na(variable) || !nzchar(variable) || !is.numeric(data[[variable]])) next
    binned = paste0(variable, "_binned")
    if (binned %in% names(data)) display_given[i] = binned
  }
  display_given
}

flexplot_magnet_breaks = function(break.me, breaks, data, labels, bins=3) {
  if (length(break.me) < 2 || is.null(breaks)) {
    return(flexplot_create_breaks(break.me, breaks, data, labels, bins))
  }
  if (is.null(names(breaks)) || any(!names(breaks) %in% break.me) ||
      anyDuplicated(names(breaks))) {
    stop("Name each custom magnet break once, using a variable to be binned: ",
         paste(break.me, collapse=", "), call.=FALSE)
  }

  result = stats::setNames(vector("list", length(break.me)), break.me)
  for (i in seq_along(break.me)) {
    variable = break.me[i]
    variable_breaks = if (variable %in% names(breaks)) breaks[variable] else NULL
    variable_labels = if (length(labels) >= i) labels[i] else NULL
    result[[variable]] = flexplot_create_breaks(
      variable, variable_breaks, data, variable_labels, bins
    )[[variable]]
  }
  result
}

flexplot_magnet_conditioned_jitter = function(self, data, params, layout) {
  index = match(as.character(data$colour), self$group_levels)
  data$x = data$x + .5 * ((index - .5) / length(self$group_levels) - .5)
  ggplot2::PositionJitter$compute_layer(data, params, layout)
}

flexplot_magnet_conditioned_position = function(group, jitter=c(0, 0)) {
  group_levels = if (is.factor(group)) levels(droplevels(group)) else sort(unique(group))
  ggplot2::ggproto(
    NULL, ggplot2::PositionJitter,
    group_levels=group_levels,
    width=jitter[1] / (length(group_levels) + 2), height=jitter[2],
    compute_layer=flexplot_magnet_conditioned_jitter
  )
}

flexplot_magnet_summary_position = function(data, group) {
  if (is.null(group)) return(ggplot2::position_identity())
  flexplot_magnet_conditioned_position(data[[group]])
}

flexplot_magnet_object = function(data, outcome, axis, given=NA,
                                  jitter=TRUE, alpha=0.6,
                                  dot_size=2, raw.data=TRUE, sample=Inf,
                                  show_fitted=TRUE, outcome_labels=NULL) {
  
  xvar = axis[1]
  display_axis = flexplot_object_axis(data, axis)
  display_given = flexplot_magnet_display_given(data, given)
  group = if (length(display_axis) > 1 && !is.na(display_axis[2])) display_axis[2] else NULL
  
  ## drop missing data
  groups = unique(c(display_axis, display_given))
  groups = groups[!is.na(groups) & nzchar(groups)]
  
  # Accept either raw data or data already prepared for magnet plots.
  if (!"y_scaled" %in% names(data)) {
    data = prepare_magnet_data(data, outcome, display_axis, display_given)
  }
  
  # Exclude missing outcomes from the displayed observations.
  data = data[!is.na(data[[outcome]]) & !is.na(data[[xvar]]), ,
              drop=FALSE]
  
  # Preserve incoming factor levels for every frame and position adjustment.
  if (!is.factor(data[[xvar]])) {
    data[[xvar]] = factor(data[[xvar]])
  }
  
  # Fit all axis predictors and conditioning variables on the full dataset.
  facet_vars = given[!is.na(given) & nzchar(given)]
  model_vars = unique(c(axis[!is.na(axis) & nzchar(axis)], facet_vars))
  
  model_formula = make.formula(as.name(outcome), lapply(model_vars, as.name))
  
  model = stats::glm(
    model_formula,
    data=data,
    family=stats::binomial()
  )
  
  df_pred = unique(data[groups])
  # Keep all display-bin keys, evaluating each continuous model variable at its
  # own full-data bin median, independently of x, grouping, or visualization sample.
  for (variable in setdiff(model_vars, groups)) {
    binned = paste0(variable, "_binned")
    medians = stats::aggregate(data[variable], by=data[binned], FUN=stats::median)
    df_pred[[variable]] = medians[[variable]][match(df_pred[[binned]], medians[[binned]])]
  }
  
  df_pred$pred_prob = stats::predict(
    model,
    newdata=df_pred,
    type="response"
  )
  
  # One sample-size label per displayed group.
  df_counts = dplyr::summarise(
    dplyr::group_by(
      data,
      dplyr::across(dplyr::all_of(groups))
    ),
    n_total=dplyr::n(),
    .groups="drop"
  )
  
  # Sample only displayed points, retaining full-data stacks, counts, and fits.
  points_data = sample.subset(sample, data)
  jitter = flexplot_magnet_jitter(jitter)
  point_position = ggplot2::position_identity()
  
  if (any(jitter != 0)) {
    point_position = ggplot2::position_jitter(
      width=jitter[1],
      height=jitter[2]
    )
  }
  
  raw_mapping = ggplot2::aes(
    x=.data[[xvar]],
    y=.data[["y_scaled"]]
  )
  
  fitted_mapping = ggplot2::aes(
    x=.data[[xvar]],
    y=.data[["pred_prob"]]
  )
  
  count_mapping = ggplot2::aes(
    x=.data[[xvar]],
    y=0.52,
    label=paste0("N=", .data[["n_total"]])
  )
  
  summary_position = ggplot2::position_identity()
  
  if (!is.null(group)) {
    raw_mapping = ggplot2::aes(
      x=.data[[xvar]],
      y=.data[["y_scaled"]],
      color=.data[[group]],
      group=.data[[group]]
    )
    
    fitted_mapping = ggplot2::aes(
      x=.data[[xvar]],
      y=.data[["pred_prob"]],
      color=.data[[group]],
      group=.data[[group]]
    )
    
    count_mapping = ggplot2::aes(
      x=.data[[xvar]],
      y=0.52,
      label=paste0("N=", .data[["n_total"]]),
      color=.data[[group]],
      group=.data[[group]]
    )
    
    # Reserve full-data group slots in every panel, including after sampling.
    point_position = flexplot_magnet_conditioned_position(data[[group]], jitter)
    summary_position = flexplot_magnet_summary_position(data, group)
  }
  
  # Keep blue fitted points when no second-axis grouping is present.
  fitted_layer = if (is.null(group)) {
    ggplot2::geom_point(
      data=df_pred,
      mapping=fitted_mapping,
      color="blue",
      size=3,
      position=summary_position
    )
  } else {
    ggplot2::geom_point(
      data=df_pred,
      mapping=fitted_mapping,
      size=3,
      position=summary_position
    )
  }
  
  p = ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept=0.5,
      linetype="dashed",
      color="gray50"
    ) +
    ggplot2::geom_point(
      data=points_data,
      mapping=raw_mapping,
      alpha=raw.alph.func(raw.data, alpha),
      shape=1,
      size=dot_size,
      stroke=1.1,
      position=point_position
    ) +
    (if (show_fitted) fitted_layer else NULL) +
    ggplot2::geom_text(
      data=df_counts,
      mapping=count_mapping,
      vjust=0,
      size=3.5,
      position=summary_position,
      show.legend=FALSE
    ) +
    flexplot_magnet_y_scale(outcome_labels) +
    ggplot2::labs(
      title="Magnet Plot",
      x=xvar,
      y=outcome
    ) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      color=ggplot2::guide_legend(
        override.aes=list(alpha=1, size=3, stroke=1.2)
      )
    )
  
  if (!is.null(group)) {
    colour_scale = if (is.ordered(data[[group]])) {
      ggplot2::scale_colour_ordinal(limits=point_position$group_levels)
    } else {
      ggplot2::scale_colour_discrete(limits=point_position$group_levels)
    }
    p = p + colour_scale +
      ggplot2::scale_x_discrete(limits=levels(droplevels(data[[xvar]])))
  }

  return(p)
}

flexplot_ghost_magnet_data = function(final, reference_data, given, data) {
  
  fitted_layers = Filter(
    function(layer) {
      inherits(layer$geom, "GeomPoint") &&
        "pred_prob" %in% names(layer$data)
    },
    final$layers
  )
  
  if (length(fitted_layers) != 1) {
    stop("Expected exactly one magnet prediction layer.")
  }
  
  predictions = fitted_layers[[1]]$data
  
  display_given = flexplot_magnet_display_given(data, given)
  
  reference = predictions
  
  for (variable in display_given) {
    reference = reference[
      as.character(reference[[variable]]) ==
        as.character(reference_data[[variable]][1]),
      ,
      drop=FALSE
    ]
  }
  
  if (!nrow(reference)) {
    stop("No magnet predictions match the ghost reference.")
  }
  
  # Remove reference panel identifiers so predictions repeat in other panels
  reference[display_given] = NULL
  
  reference
}
