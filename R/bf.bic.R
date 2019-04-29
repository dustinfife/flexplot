##' Compute Bayes Factor from BICs
##'
##' Compute Bayes Factor from BICs
##'	
##' Given two models, the bf.bic function calculates the approximate bayes factor from the two models' BIC values
##' @param model1 The first model to be compared
##' @param model2 The second model to be compared
##' @param invert Should the BF be inverted?
##' @return The approximate Bayes Factor of the model comparison
##' @author Dustin Fife
##' @export
bf.bic = function(model1, model2, invert=F){
	bf = exp((BIC(model2) - BIC(model1))/2)
	if (invert){
		1/bf
	} else {
		bf
	}
}