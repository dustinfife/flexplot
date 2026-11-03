flexplot_magnet_object = function(data, outcome, axis, given=NA,
                                  jitter=TRUE, alpha=0.6,
                                  dot_size=2) {
  
  xvar = axis[1]
  
  ## drop missing data
  groups = unique(c(axis, given))
  groups = groups[!is.na(groups) & nzchar(groups)]
  
  # Accept either raw data or data already prepared for magnet plots.
  if (!"y_scaled" %in% names(data)) {
    data = prepare_magnet_data(data, outcome, axis, given)
  }
  
  # Exclude missing outcomes from the displayed observations.
  data = data[!is.na(data[[outcome]]) & !is.na(data[[xvar]]), ,
              drop=FALSE]
  
  # Preserve ordered factors; otherwise order by fitted probability.
  was_ordered = is.ordered(data[[xvar]])
  
  if (!is.factor(data[[xvar]])) {
    data[[xvar]] = factor(data[[xvar]])
  }
  
  # Fit logistic regression, accounting for conditioning variables.
  facet_vars = given[!is.na(given) & nzchar(given)]
  model_vars = unique(c(xvar, facet_vars))
  
  model_formula = stats::reformulate(model_vars, response=outcome)
  
  model = stats::glm(
    model_formula,
    data=data,
    family=stats::binomial()
  )
  
  df_pred = unique(data[model_vars])
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
  
  point_position = ggplot2::position_identity()
  
  if (isTRUE(jitter)) {
    point_position = ggplot2::position_jitter(
      width=0.05,
      height=0
    )
  }
  
  p = ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept=0.5,
      linetype="dashed",
      color="gray50"
    ) +
    ggplot2::geom_point(
      data=data,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=.data[["y_scaled"]]
      ),
      color="black",
      alpha=alpha,
      shape=1,
      size=dot_size,
      position=point_position
    ) +
    ggplot2::geom_point(
      data=df_pred,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=.data[["pred_prob"]]
      ),
      color="blue",
      size=3
    ) +
    ggplot2::geom_text(
      data=df_counts,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=0.52,
        label=paste0("N=", .data[["n_total"]])
      ),
      vjust=0,
      size=3.5
    ) +
    ggplot2::scale_y_continuous(
      breaks=c(0, 0.25, 0.5, 0.75, 1)
    ) +
    ggplot2::labs(
      title="Magnet Plot",
      x=xvar,
      y=outcome
    ) +
    ggplot2::theme_bw()
  
  return(p)
}