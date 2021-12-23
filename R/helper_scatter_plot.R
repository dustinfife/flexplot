smoothing_function_string = function(method, suppress_smooth) {
  if (suppress_smooth)                              return('xxxx')
  if (method=="rlm")                                return('geom_smooth(method = "rlm", se = se, formula = y~x)')
  if (method=="poisson" | method=="Gamma")          return('geom_smooth(method = "glm", method.args = list(family = method), se = se, formula = y~x)')
  if (method=="polynomial" | method == "quadratic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))')
  if (method=="cubic")                              return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))')
  if (method=="lm")                                 return('stat_smooth(method="lm", se=se, formula = y~x)')

  return('geom_smooth(method="loess", se=se, formula = y~x)')
}  

create_scatter_plot = function(data, axis, jitter=c(.2,0), suppress_smooth=F, method) {
  p = 'ggplot(data=data, aes_string(x=axis, y=outcome))'
  points = points.func(axis.var=axis, data=data, jitter=jitter)
  fitted = smoothing_function_string(method, suppress_smooth)
  return(p=p, points=points, fitted=fitted)
}


