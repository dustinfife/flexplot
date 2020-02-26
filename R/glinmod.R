##' Perform a general linear model
##'
##' Perform a general linear model
##'	
##' A general linear model (GLM) is a statistical model of the form Y = BX, where X can be any number of numeric or categorical predictors. The GLM subsumes
##' the most common statistical models, including t-tests, ANOVAs, regressions, etc. The function will automatically detect whether the variables are numeric
##' or categorical and generate estimates and graphics accordingly. 
##' @param f An equation of the form y~x1 + x2 + x3, etc. 
##' @param data The dataset containing the variables the user wishes to model
##' @param plot Should results be plotted?
##' @param ... Other parameters passed to \code{\link{visualize}} (which is then passed to \code{\link{flexplot}})
##' @seealso \code{\link{estimates}}, \code{\link{visualize}}, \code{\link{flexplot}}, \code{\link{model.comparison}}
##' @return An object containing the information from \code{\link{estimates}}
##' @author Dustin Fife
##' @import stats
##' @export
##' @examples
##' ## load the exercise dataset
##' data(exercise_data)
##' data = exercise_data
##' glinmod(weight.loss~gender, data=exercise_data)
##' ## pass parameters to flexplot
##' glinmod(weight.loss~gender + motivation, data=exercise_data, 
##'		method="lm")
glinmod = function(f, data, plot=TRUE, ...){

	#### run the lm
	mod = lm(f, data)
	
	#### compute the estimates		
	est = estimates(mod)

	
	#### visualize that succa
	if (plot){
		plot = visualize(mod,...)
		return(plot)
	} else {
		return(est)	
	}
	
}