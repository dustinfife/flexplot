##' Compute standardized betas on a linear model object
##'
##' Compute standardized betas on a linear model object
##'	
##' Compute standardized betas on a linear model object
##' @param object a lm object
##' @param sd.y Should we scale based on standard deviation of Y as well as the predictors?
##' @param se Should standard errors be reported?
##' @return the standardized betas
##' @author Dustin Fife
##' @export
standardized.beta = function(object, sd.y=T, se=F){
	b <- summary(object)$coef[, 1]
	sx <- apply(model.matrix(object), 2, sd)
    sy <- apply(object$model[1], 2, sd)
    if (sd.y){
	    beta <- b * sx/sy
    } else {
    	beta = b*sx
    }
    if (!se){
	    return(beta)
    } else {
    	sterr = summary(object)$coef[,2]
    	beta.se = sterr*sx/sy
    	list(beta=beta, se= beta.se)
    }
}