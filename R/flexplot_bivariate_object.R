flexplot_bivariate_object = function(data, outcome, axis, method="loess",
                                     se=FALSE, suppress_smooth=FALSE,
                                     alpha=.5, jitter=NULL, sample=Inf,
                                     raw.data=TRUE) {
  
  jitter = if (is.null(jitter)) c(0, 0) else match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)
  
  # separate the axes
  x = axis[1]
  group = if (length(axis) > 1) axis[2] else NULL
  
  # construct the mapping
  mapping = ggplot2::aes(
    x=.data[[x]],
    y=.data[[outcome]]
  )
  
  if (!is.null(group)) {
    mapping = ggplot2::aes(
      x=.data[[x]],
      y=.data[[outcome]],
      color=.data[[group]],
      group=.data[[group]]
    )
    original_group = sub("_binned$", "", group)
    binned_numeric = !identical(original_group, group) && is.numeric(data[[original_group]])
    n_groups = length(unique(data[[group]]))
    if (binned_numeric || n_groups <= 6) {
      mapping$linetype = mapping$colour
    }
    if (n_groups <= 6) mapping$shape = mapping$colour
  }
  
  p = ggplot2::ggplot(data, mapping) +
    geom_jitterd(
      data=points_data,
      alpha=raw.alph.func(raw.data, alpha),
      width=jitter[1],
      height=jitter[2]
    )
  
  if (!suppress_smooth) {
    p = p + flexplot_smooth_layer(data, outcome, method, se)
  }

  if (!is.null(mapping$shape)) {
    # Train all group aesthetics in full-data order, even when points are sampled.
    group_levels = if (is.factor(data[[group]])) {
      levels(droplevels(data[[group]]))
    } else {
      sort(unique(data[[group]]))
    }
    colour_scale = if (is.ordered(data[[group]])) {
      ggplot2::scale_colour_ordinal(limits=group_levels)
    } else {
      ggplot2::scale_colour_discrete(limits=group_levels)
    }
    p = p + colour_scale +
      ggplot2::scale_shape_discrete(limits=group_levels) +
      ggplot2::scale_linetype_discrete(limits=group_levels)
  }
  
  p + ggplot2::theme_bw() +
    ggplot2::theme(text=ggplot2::element_text(size=14))
}

flexplot_logistic_layer = function(data, outcome, se) {
  
  if (length(unique(stats::na.omit(data[[outcome]]))) != 2) {
    stop("To fit a logistic curve, the outcome must have exactly two levels.")
  }
  
  return(ggplot2::geom_smooth(
    method="glm",
    method.args=list(family=stats::binomial()),
    formula=y~x,
    se=se
  ))
}

flexplot_smooth_layer = function(data, outcome, method, se) {
  
  if (identical(method, "logistic")) {
    if (length(unique(stats::na.omit(data[[outcome]]))) != 2) {
      stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
    }
    
    return(return(flexplot_logistic_layer(data, outcome, se)))
  }
  
  if (method %in% c("poisson", "Gamma")) {
    return(ggplot2::geom_smooth(
      method="glm", method.args=list(family=method),
      se=se, formula=y~x
    ))
  }
  
  if (method %in% c("polynomial", "quadratic", "cubic")) {
    degree = if (identical(method, "cubic")) 3 else 2
    smooth_formula = if (degree == 3) {
      y ~ poly(x, 3, raw=TRUE)
    } else {
      y ~ poly(x, 2, raw=TRUE)
    }
    
    return(ggplot2::stat_smooth(
      method="lm", se=se, formula=smooth_formula
    ))
  }
  
  smooth_method = if (method %in% c("lm", "rlm")) method else "loess"
  
  ggplot2::geom_smooth(
    method=smooth_method, se=se, formula=y~x
  )
}
