##' Create an added variable plot
##'
##' Create an added variable plot
##'	
##' This function first residualizes the outcome variable based whatever variables the user decides to condition on.
##' The mean of the outcome variable is then added to the residuals (to maintain the interpretation of the variable),
##' then the function plots the residuals against the other variables the user specifies.
##' 
##' More specifically, if the user specifies `formula` but leaves the options `x` and `lm_formula` as NULL, it will 
##' default to residualizing the first variable(s) in the formula and plot the last variable entered against those residuals.
##' For example, if the user specifies y~x + z, this function will residualize y~x, then plot z against those residuals. 
##' 
##' If the user specifies a value of `x`, that tells the function which variable to residualize. So, again, if the user specifies
##' `y~x + z` then sets `x` to 2, the function will instead residualize based on z instead of x. 
##' 
##' For multivariate AVPs, the user can specify a value for `lm_formula`. The value of `lm_formula` specifies the fitted model
##' that is then residualized, while the value of `formula` specifies how the remaining variables are displayed, using `flexplot`
##' formula conventions.
##' @param formula A flexplot formula, specifying how the avp will visualize the variables. 
##' @param data The dataset used
##' @param lm_formula Optional. A formula specifying how to condition variables.  
##' @param method The smoothing method. Defaults to "loess"
##' @param x The variable you wish to place on the x axis. Defaults to NULL. 
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{flexplot}}
##' @author Dustin Fife
##' @export
##' @aliases added.plot added_plot avp
##' @import tibble
##' @return An added variable plot
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data, x=2)
##' added.plot(weight.loss~motivation + therapy.type, 
##' lm_formula = weight.loss~health*muscle.gain, data=exercise_data)
added.plot = added_plot = avp = function(formula, data, lm_formula=NULL, method="loess", x=NULL, ...){
  
	#### identify variable types
	variables = all.vars(formula)

	# do all the error checks
	check_all_variables_exist_in_data(variables, data)
	check_all_variables_exist_in_data(all.vars(lm_formula), data)
	check_variables_in_lm(formula, lm_formula)
	
	# prep data
	data = prep_data_for_avp(data, variables)
	formulae = make_avp_formula(formula, lm_formula, x)
	
  #### if they ask for logistic
	if (method == "logistic"){
	    fitted = fitted(glm(formulae[[1]], data=data, family=binomial))
	    data$residuals = factor.to.logistic(data=data, outcome=variables[[1]])[,variables[1]] - fitted
	    method = "loess"
	 } else {
	    data$residuals = residuals(lm(formulae[[1]], data=data)) + mean(data[,variables[1]])    
	 }
	

	#### plot it
	plot = flexplot(formulae[[2]], data=data, method=method, ...) + labs(y=formulae[[3]])
	class(plot) <- c("flexplot", class(plot))
	return(plot)
}



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


# I started working on this to make it super general, but I'm quitting here. 

# p = added.plot(formula, data, lm_formula, method, x, ...)
# 
# # identify the last predictor
# vars = all.vars(formula)
# interest = vars[length(vars)]
# 
# # make dv
# newf1 = make_avp_formula(formula, lm_formula, x)
# newf = update(newf1[[2]], as.formula(paste0(all.vars(formula)[1], "~.")))
# 
# # fit model
# mod = lm(fnew, data=data)
# new_data = data
# newf1
# if (check.non.number(data[,all.vars(newf1[[2]])[2]])) {
#   new_data = data 
#   new_data[,"residuals"] = predict(mod)
#   return(p + geom_point(data=new_data, col="blue") +
#            geom_line(data=new_data, aes(group=1)))
#   
# }
# return(p + geom_abline(data=new_dataslope=coef(mod)[2], intercept=coef(mod)[1]))
# 
# 
# }


'%!in%' <- function(x,y)!('%in%'(x,y))
