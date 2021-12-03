#' Create a plot for mediation analysis
#'
#' @param formula A formula with y on the left side and the predictors on the right. The last variable
#' entered will be plotted on the x-axis
#' @param data The dataset
#' @param method The fitted line. Defaults to "lm". 
#' @param ... Other parameters passed to flexplot
#'
#' @return a plot showing the mediation effect
#' @export
#'
#' @examples
#' mediate_plot(weight.loss~motivation + health, data=exercise_data)
#' mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data)
mediate_plot = function(formula, data, method="lm", ...) {
  
  p = added.plot(formula=formula, data=data, method=method, ...)
  
  # identify the last predictor
  vars = all.vars(formula)
  interest = vars[length(vars)]
  
  # make dv
  dv = vars[1]
  fnew = make.formula(dv, interest)
  
  # fit model
  mod = lm(fnew, data=data)
  
  if (check.non.number(data[,interest])) {
    new_data = data 
    new_data[,"residuals"] = predict(mod)
    return(p + geom_point(data=new_data, col="blue") +
             geom_line(data=new_data, aes(group=1)))
    
  }
  return(p + geom_abline(slope=coef(mod)[2], intercept=coef(mod)[1]))
  
  
}