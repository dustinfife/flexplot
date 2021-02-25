##' Create an added variable plot
##'
##' Create an added variable plot
##'	
##' This function first residualizes the outcome variable based on the first variables listed in the formula.
##' The mean of the outcome variable is then added to the residuals (to maintain the interpretation of the variable),
##' then the function plots the residuals against the second variable. 
##' @param formula A formula with exactly two predictors and an outcome variable
##' @param data The dataset used
##' @param method The smoothing method. Defaults to "loess"
##' @param x The variable you wish to place on the x axis. 
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{flexplot}}
##' @author Dustin Fife
##' @export
##' @aliases added.plot added_plot avp
##' @import tibble
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data)
added.plot = added_plot = avp = function(formula, data, method="loess", x=NULL, ...){
	#### identify variable types
	variables = all.vars(formula)

	check_all_variables_exist_in_data(variables, data)
	
	#pick out the variables
	outcome = variables[1]
	predictors = variables[-1]
	res.variable = find_variable_of_interest(predictors, x)
	remaining.vars = predictors[which(!(predictors %in% res.variable))]
	
  # prep data
	data = prep_data_for_avp(data, variables)
	
	#### model the first chosen variable
	new.form = make.formula(outcome, remaining.vars)
	
  #### if they ask for logistic
	if (method == "logistic"){
	    fitted = fitted(glm(new.form, data=data, family=binomial))
	    data$residuals = factor.to.logistic(data=data, outcome=outcome)[,outcome] - fitted
	    method = "loess"
	 } else {
	    data$residuals = residuals(lm(new.form, data=data)) + mean(data[,outcome])    
	 }
	
	##### now plot that succa
	new.form = make.formula("residuals", res.variable)

	#### if they ask for
	plot = flexplot(new.form, data=data, method=method, ...) + labs(y=paste0(outcome, " | ", paste0(remaining.vars, collapse=", ")))
	class(plot) <- c("flexplot", class(plot))
	return(plot)
}

prep_data_for_avp = function(data, variables) {
  # convert from tibble
  if (tibble::is_tibble(data)){
    data = data.frame(data)
  }
  
  #### remove missing data
  miss.vals = sapply(data[,variables], function(x) sum(is.na(x)))
  miss.vals = miss.vals[miss.vals>0]
  if (length(miss.vals)!=0){
    warning("Note: I'm removing missing values so the function will work.")
    data = na.omit(data[,variables])
  }
  return(data)
}

find_variable_of_interest = function(predictors, x=NULL) {
  
  if (is.null(x)) return(predictors[length(predictors)])
  
  # make sure x is in the data
  if (is.numeric(x) & x>length(predictors)) stop("Oops! You're asking for a variable position that is larger than the number of predictors in your formula! Pick a smaller number for x.")
  if (is.numeric(x)) return(predictors[x])
  if (x %!in% predictors) stop(paste0("Oops! I can't find the variable ", x, " in your formula"))
  return(x)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
