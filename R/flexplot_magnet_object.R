flexplot_magnet_jitter = function(jitter) {
  if (is.null(jitter) || isTRUE(jitter)) return(c(.05, 0))
  if (is.numeric(jitter) && length(jitter) == 2) return(jitter)
  if (is.numeric(jitter) && length(jitter) == 1) return(c(jitter, 0))
  match_jitter_categorical(jitter)
}


flexplot_magnet_object = function(data, outcome, axis, given=NA,
                                  jitter=TRUE, alpha=0.6,
                                  dot_size=2, raw.data=TRUE, sample=Inf) {
  
  xvar = axis[1]
  display_axis = flexplot_object_axis(data, axis)
  group = if (length(display_axis) > 1 && !is.na(display_axis[2])) display_axis[2] else NULL
  
  ## drop missing data
  groups = unique(c(display_axis, given))
  groups = groups[!is.na(groups) & nzchar(groups)]
  
  # Accept either raw data or data already prepared for magnet plots.
  if (!"y_scaled" %in% names(data)) {
    data = prepare_magnet_data(data, outcome, display_axis, given)
  }
  
  # Exclude missing outcomes from the displayed observations.
  data = data[!is.na(data[[outcome]]) & !is.na(data[[xvar]]), ,
              drop=FALSE]
  
  # Preserve ordered factors; otherwise order by fitted probability.
  was_ordered = is.ordered(data[[xvar]])
  
  if (!is.factor(data[[xvar]])) {
    data[[xvar]] = factor(data[[xvar]])
  }
  
  # Fit all axis predictors and conditioning variables on the full dataset.
  facet_vars = given[!is.na(given) & nzchar(given)]
  model_vars = unique(c(axis[!is.na(axis) & nzchar(axis)], facet_vars))
  
  model_formula = stats::reformulate(model_vars, response=outcome)
  
  model = stats::glm(
    model_formula,
    data=data,
    family=stats::binomial()
  )
  
  df_pred = unique(data[unique(c(model_vars, group))])
  if (!is.null(group) && !identical(group, axis[2])) {
    # Model the continuous predictor, displaying one prediction per bin at its
    # full-data median, shared across x categories and conditioning variables.
    medians = stats::aggregate(data[axis[2]], by=data[group], FUN=stats::median)
    df_pred = unique(data[unique(c(xvar, group, facet_vars))])
    df_pred[[axis[2]]] = medians[[axis[2]]][match(df_pred[[group]], medians[[group]])]
  }
  
  df_pred$pred_prob = stats::predict(
    model,
    newdata=df_pred,
    type="response"
  )
  
  if (!was_ordered) {
    probabilities = stats::aggregate(
      df_pred$pred_prob,
      by=list(category=df_pred[[xvar]]),
      FUN=mean
    )
    
    new_levels = as.character(
      probabilities$category[order(probabilities$x, decreasing=TRUE)]
    )
    
    data[[xvar]] = factor(data[[xvar]], levels=new_levels)
    df_pred[[xvar]] = factor(df_pred[[xvar]], levels=new_levels)
  }
  
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
    
    point_position = ggplot2::position_jitterdodge(
      jitter.width=jitter[1],
      jitter.height=jitter[2],
      dodge.width=.5
    )
    
    summary_position = ggplot2::position_dodge(width=.5)
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
    fitted_layer +
    ggplot2::geom_text(
      data=df_counts,
      mapping=count_mapping,
      vjust=0,
      size=3.5,
      position=summary_position,
      show.legend=FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks=c(0, 0.25, 0.5, 0.75, 1)
    ) +
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
  
  return(p)
}
