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
##'      lm_formula = weight.loss~health*muscle.gain, data=exercise_data)
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