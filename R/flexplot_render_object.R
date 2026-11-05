flexplot_object_axis = function(data, axis) {
  if (length(axis) < 2 || is.na(axis[2])) return(axis)
  if (!is.numeric(data[[axis[2]]])) return(axis)

  binned = paste0(axis[2], "_binned")
  if (!binned %in% names(data)) {
    stop("Expected binned second-axis variable: ", binned)
  }

  axis[2] = binned
  axis
}

flexplot_render_object = function(data, outcome, axis, given,
                                  method, se, suppress_smooth,
                                  plot.type, bins, spread,
                                  use_magnet=FALSE,
                                  related=FALSE,
                                  alpha=.99977,
                                  raw.data=TRUE,
                                  sample=Inf,
                                  jitter=NULL, magnet_summary=TRUE, ...) {
  
  # Paired-difference plots
  if (related) {
    return(flexplot_related_object(
      data=data,
      jitter=jitter,
      plot.type=plot.type,
      spread=spread,
      alpha=alpha,
      raw.data=raw.data,
      sample=sample,
      suppress_smooth=suppress_smooth
    ))
  }
  
  # Magnet plots
  if (use_magnet) {
    return(flexplot_magnet_object(
      data=data,
      outcome=outcome,
      axis=axis,
      given=given,
      alpha=alpha,
      raw.data=raw.data,
      sample=sample,
      jitter=jitter,
      show_fitted=magnet_summary
    ))
  }
  
  # Univariate plots
  if (identical(axis[1], "1")) {
    return(flexplot_distribution_object(
      data=data,
      outcome=outcome,
      plot.type=plot.type,
      bins=bins
    ))
  }
  
  if (!is.numeric(data[[outcome]]) && !is.numeric(data[[axis[1]]]) &&
      length(unique(stats::na.omit(data[[outcome]]))) >= 3) {
    return(flexplot_association_object(data, outcome, axis))
  }

  axis = flexplot_object_axis(data, axis)

  # Categorical predictors
  if (!is.numeric(data[[axis[1]]])) {
    return(flexplot_categorical_object(
      data=data,
      outcome=outcome,
      axis=axis,
      spread=spread,
      plot.type=plot.type,
      suppress_smooth=suppress_smooth,
      alpha=alpha,
      raw.data=raw.data,
      sample=sample,
      jitter=jitter,
      ...
    ))
  }
  
  # Continuous predictors
  flexplot_bivariate_object(
    data=data,
    outcome=outcome,
    axis=axis,
    method=method,
    se=se,
    suppress_smooth=suppress_smooth,
    alpha=alpha,
    raw.data=raw.data,
    sample=sample,
    jitter=jitter,
    ...
  )
}
