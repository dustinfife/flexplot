create_logistic_plot = function(data, axis, jitter) {
  p = 'ggplot(data=data, aes_string(x=axis, y=outcome))'
  points = points.func(axis.var=axis, data=data, jitter=jitter)
  fit.string = 'geom_smooth(method = "glm", method.args = list(family = "binomial"), formula = y~x)'
  list(p=p, points=points, fitted=fit.string)
}

# make sure method = 'logistic' under the right circumstances
identify_method = function(data, outcome, axis, method) {
  # histograms/barcharts
  if (axis[1] == "1") return("loess")
  # association plot
  if (check.non.number(data[,axis[1]])) return("loess")
  # logistic
  if (length(unique(data[,outcome]))==2) return("logistic")
  if (!is.null(method)) return(method)  
  return("loess")
}

#### this function converts a binary variable to a 1/0 for logistic regression
factor.to.logistic = function(data, outcome, method=NULL, labels=F){
  
  levels_dv = length(unique(data[,outcome]))
  
  # return if it's not logistic
  if (levels_dv != 2) return(data)
  if (labels) return(unique(data[,outcome]))
  if (is.numeric(data[,outcome])) return(data)
  if (method != "logistic") return(data)
  # at this point it's categorical, has two levels, but doesn't necessarily have "logistic" as a method  
  ### now do the conversion
  data[,outcome] = as.numeric(as.character(factor(data[,outcome], levels=unique(data[,outcome]), labels=c(0,1))))
  return(data)
  
}
