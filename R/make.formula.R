##' \code{make.formula} is a function that easily converts a set of strings into a formula. It requires two arguments: a single response variable,
##' and a vector of strings. See examples. 
##'
##' @title Convert strings to a formula
##' @param response a single string used on the left side of a formula
##' @param predictors a string (or a vector of strings) representing the predictors. Each will be separated by a plus sign.
##' @param random a string that indicates the random component in an \code{lmer}-like object (e.g., "(1|group)"). Defaults
##' to NULL.
##' @return a formula object
##' @export
##' @author Dustin Fife
##' @examples
##' k = data.frame(matrix(rnorm(100), ncol=5))
##' names(k) = LETTERS[1:5]
##' formula = make.formula("A", LETTERS[2:5])
##' formula
##' lm(formula, data=k)
##' #do a random model
##' make.formula("A", LETTERS[2:5], random="(1|group)")
make.formula = function(response, predictors, random=NULL){
	if (is.null(random)){
		formula(paste(response, "~", 
					paste(predictors, collapse="+"),
				sep=""))
	} else {
		formula(paste(response, "~", 
					paste(predictors, collapse="+"),
					"+",random,
				sep=""))
	}
}

