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
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{flexplot}}
##' @author Dustin Fife
##' @export
##' @import tibble
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data)
added.plot = function(formula, data, method="loess", ...){
	#### identify variable types
	variables = all.vars(formula)
		#### make sure all variables in in data
	missing.preds = variables[which(!(variables %in% names(data)))]
	if (length(missing.preds)>0){
		stop(paste0("One or more of your predictor variables: ", paste0(missing.preds, collapse=","), " are missing. Did you specify the right dataset and spell the variables correctly?"))
	}	
	outcome = variables[1]
	predictors = variables[-1]
	res.variable = predictors[length(predictors)]
	remaining.vars = predictors[which(!(predictors %in% res.variable))]
	
	#### bark if there's not two variables
	# if (length(predictors) != 2){
		# stop("You must have exactly two predictor variables")
	# }
	
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
	
	#### find model type
	


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
