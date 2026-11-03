flexplot_render_object = function(data, outcome, axis, given,
                                  method, se, suppress_smooth,
                                  plot.type, bins, spread, ...) {
  
  # Univariate plots
  if (identical(axis[1], "1")) {
    return(flexplot_distribution_object(
      data=data,
      outcome=outcome,
      plot.type=plot.type,
      bins=bins
    ))
  }
  
  # Categorical predictors
  if (!is.numeric(data[[axis[1]]])) {
    return(flexplot_categorical_object(
      data=data,
      outcome=outcome,
      axis=axis[1],
      spread=spread,
      plot.type=plot.type,
      suppress_smooth=suppress_smooth,
      ...
    ))
  }
  
  # Continuous predictors
  return(flexplot_bivariate_object(
    data=data,
    outcome=outcome,
    axis=axis[1],
    method=method,
    se=se,
    suppress_smooth=suppress_smooth,
    ...
  ))
  

}