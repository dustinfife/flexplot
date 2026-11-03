# this function returns a ggplot object after determining the sort of plot the user needs
flexplot_bivariate_object = function(data, outcome, axis, method="loess",
                                     se=FALSE, suppress_smooth=FALSE,
                                     alpha=.5, jitter=NULL, sample=Inf,
                                     raw.data=TRUE) {
  
  jitter = if (is.null(jitter)) c(0, 0) else match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)

  p = ggplot2::ggplot(data, ggplot2::aes(x=.data[[axis]], y=.data[[outcome]])) +
        geom_jitterd(data=points_data, alpha=raw.alph.func(raw.data, alpha),
                 width=jitter[1], height=jitter[2])

  if (!suppress_smooth) {
    p = p + flexplot_smooth_layer(data, outcome, method, se)
  }

  p + ggplot2::theme_bw() + ggplot2::theme(text=ggplot2::element_text(size=14))
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
