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
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data, x=2)
##' added.plot(weight.loss~motivation + therapy.type, lm_formula = weight.loss~health*muscle.gain, data=exercise_data)
added.plot = added_plot = avp = function(formula, data, lm_formula=NULL, method="loess", x=NULL, ...){

	#### identify variable types
	variables = all.vars(formula)

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


label_avp_axis = function(formula) {
  terms = strsplit(deparse(formula), "~")[[1]]
  return(paste0(terms[1], "|", terms[2]))
}


make_avp_formula = function(formula, lm_formula=NULL, x=NULL) {

  if (!is.null(lm_formula)){
    axis_label = label_avp_axis(lm_formula)
    
    # substitute formula
    y_var = all.vars(formula)[1]
    # remove white space from entire formula
    newf = gsub(" ", "", deparse(formula))
    formula = formula(gsub(paste0(y_var, "~"), "residuals~", newf))
    
    return(list(lm_formula, formula, axis_label))
  }
  
  #pick out the variables
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  res.variable = find_variable_of_interest(predictors, x)
  remaining.vars = predictors[which(!(predictors %in% res.variable))]
  

  #### model the first chosen variable
  lm_formula = make.formula(outcome, remaining.vars)
  formula = make.formula("residuals", res.variable)
  list(lm_formula, formula, paste0(outcome, " | ", paste0(remaining.vars, collapse=", ")))
  
}

# this function ensures all variables in lm_formula are there in formula
check_variables_in_lm = function(formula, lm_formula, check_both = FALSE){
  
  if (is.null(lm_formula)) {
    return(NULL)
  }
  # make sure the outcome variable is the same
  if (all.vars(formula)[1] != all.vars(lm_formula)[1]) {
    msg = paste0("Your outcome variable for your model (", all.vars(lm_formula)[1], 
                 ") is not the same as your outcome variable for your avp (", all.vars(formula)[1], ")")
    stop(msg)
  }
  
  # make sure it's a valid formula
  if (!rlang::is_bare_formula(lm_formula)) {
    stop("You need to specify a valid formula for added variable plots")
  }
  
  
  if (check_both) {
  #make sure all the variables in lm are the same as avp
  if (!all(all.vars(formula) %in% all.vars(lm_formula) )) {
    msg = paste0("One or more of the variables provided in the lm formula (", deparse(lm_formula),
                 ") don't match your plotting formula (", deparse(formula), ")")
    stop(msg)
  }
  }
  return(NULL)
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

#' Create a plot for mediation analysis
#'
#' @param formula A formula with y on the left side and the predictors on the right. The last variable
#' entered will be plotted on the x-axis
#' @param data The dataset
#' @param method The fitted line. Defaults to "lm". 
#' @param ... Other parameters passed to flexplot
#'
#' @return a plot
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

find_variable_of_interest = function(predictors, x=NULL) {
  
  if (is.null(x)) return(predictors[length(predictors)])
  
  # make sure x is in the data
  if (is.numeric(x) & x>length(predictors)) stop("Oops! You're asking for a variable position that is larger than the number of predictors in your formula! Pick a smaller number for x.")
  if (is.numeric(x)) return(predictors[x])
  if (x %!in% predictors) stop(paste0("Oops! I can't find the variable ", x, " in your formula"))
  return(x)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
