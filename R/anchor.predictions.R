##' Generate Predictions for a Model
##'
##' Generate Predictions for a Model
##'	
##' This function is simply an easy-to-use wrapper for the predict function. With some models (e.g., logistic regression), the metrics are not very intuitive. 
##' The anchor.predictions model generates the actual predicted values, depending on what the user specifies. 
##' @param model The fitted model object to generate predictions
##' @param reference A vector (or single value) containing the name of the variable(s) the user wishes to generate predictions for. For
##' categorical variables, the function will generate predictions for every level of the categorical variable. For numeric variables, the function
##' will generate predictions at +1 and -1 standard deviations away from the mean. 
##' @param shutup The function will give a notice if you don't specify predictions for variables that are in the model. This argument tell it to shut up. 
##' @return A data frame containing the predicted values (along with the values of the predictor variables used to estimate the prediction)
##' @author Dustin Fife
##' @export
##' @examples
##' data(exercise_data)
##' linear.model = lm(weight.loss~health + gender, data= exercise_data)
##' # generate predictions for males/females
##' anchor.predictions(linear.model, "gender")
##' # generate predictions for health (+/- 1 sd from the mean)
##' anchor.predictions(linear.model, "health")
##' # fit a logistic regression model
##' data(tablesaw.injury)
##' glm.mod = glm(injury~safety + attention + gender, data= tablesaw.injury, family=binomial)
##' anchor.predictions(glm.mod, "attention")
##' anchor.predictions(glm.mod, c("safety", "gender"))
anchor.predictions = function(model, reference, shutup=F){

	terms = attr(terms(model), "term.labels")

		#### extract dataset
	d = model$model

		##### figure out which terms need to be aggregated across
	included = terms[which(terms %in% reference)]
	not.included = terms[which(!(terms %in% reference))]	

		##### figure out which are categorical
	if (length(terms)>1){	
		factors = names(which(unlist(lapply(d[,terms], is.factor)))); factors.included = factors[factors%in%included]
	} else {
		factors = ifelse(is.factor(d[,terms]), terms, NULL); factors.included = factors[factors%in%included]
	}
	numeric = terms[!(terms %in% factors)]; numeric.included = numeric[numeric%in%included]

		#### average the ones that need to be averaged
	temp.func = function(x, factors=factors, d=d) { 
		if (x %in% factors){
			return(levels(d[,x])[1])
		} else {
			return(mean(d[,x], na.rm=T))
		}
	}
	
	if (length(not.included)>0){
		averages = lapply(not.included, temp.func, factors=factors, d=d)
		average.predictions = setNames(as.list(averages), not.included)
		if (!shutup){
			message(paste0("\nNote: You didn't specify predictions for:\n      ", paste0(not.included, collapse=","), "\nI'm going to predict the average for quantitative variables and take the first level of categorical predictors.\n\n"))
		}	
	} else {
		average.predictions = NA
	}

		##### predict the ones that need to be predicted
	if (length(factors.included)>1){		
		factor.levs = lapply(d[, factors.included], unique)	
	} else if (length(factors.included)==1) {
		factor.levs = list(levels(d[, factors.included[1]]))
		names(factor.levs)[[1]] = factors.included[1]
	} else {
		factor.levs =NULL
	}


	### create function to return +/- 1 standard deviation
	f = function(x){return(c(mean(x, na.rm=T) + sd(x, na.rm=T), mean(x, na.rm=T)-sd(x, na.rm=T)))}
			
	if (length(numeric.included)>1){
		numeric.preds = lapply(d[,numeric.included], f)
	} else if (length(numeric.included) ==1){
		numeric.preds = list(f(d[,numeric.included]))
		names(numeric.preds)[[1]] = numeric.included
	} else {
		numeric.preds=NULL
	}
	
	##### generate final prediction
	if (length(numeric.preds)>0 & length(factor.levs)>0){
		if (is.na(average.predictions[1])){
			final.prediction = expand.grid(c(numeric.preds, factor.levs))
		} else {
			final.prediction = expand.grid(c(numeric.preds, factor.levs, average.predictions))
		}
	} else if (length(numeric.preds)>0){
		if (is.na(average.predictions[1])){
			final.prediction = expand.grid(c(numeric.preds))
		} else {
			final.prediction = expand.grid(c(numeric.preds, average.predictions))
		}
	} else if (length(factor.levs)>0){
		if (is.na(average.predictions[1])){
			final.prediction = expand.grid(c(factor.levs))
		} else {
			final.prediction = expand.grid(c(factor.levs, average.predictions))
		}
		
	}
	
	##### now predict
	final.prediction$prediction = predict(model, final.prediction, type="response")
	
	### rename predictions
	final.prediction[,numeric.included] = paste0(round(final.prediction[,numeric.included], digits=2), " (", c("+", "-"), "1 SD)", sep="")
	if (length(which(names(final.prediction)%in%not.included))>0){
		final.prediction = final.prediction[,-which(names(final.prediction)%in%not.included)]
	}
	
	final.prediction

}
