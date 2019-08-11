##' Display a graphic of a mixed model
##'
##' Display a graphic of a mixed model
##'	
##' This function samples the ID variable and plots the bivariate relationship, depending on the mixed model chosen
##' @param formula A flexplot friendly formula, though it can only include one predictor
##' @param data The dataset
##' @param model The model
##' @param n The number of IDs to sample and put in different panels
##' @param jitter Should the data be jittered?
##' @return A plot
##' @author Dustin Fife
##' @export
mixed.mod.visual = function(formula, data, model, n=6, jitter=F){
	
	##### sample from the IDs
	form = formula(model)
	ID = subsetString(as.character(form)[3], "|", position=2); ID = gsub(" ", "", ID); ID = gsub(")", "", ID)
	all.IDs = as.character(unique(data[,ID]))
	if (n<length(all.IDs)){
		samp = sample(all.IDs, n)
	} else {
		stop(paste0("n can't be larger than the number of unique values of ", ID))
	}
	
	##### subset the data
	samp = sort((samp))
	rows = data[,ID] %in% samp
	d_new = data[rows,]	

	
	##### only allow one X and one Y 
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	if (length(predictors)>1){
		stop("Sorry, I can only plot one predictor variable at a time")
	}


	
	##### extract the rows of the random effects
	d2 = data[!duplicated(data[,ID]),]
	select.rows = which(d2[,ID] %in% samp)
	d_new[,ID] = factor(d_new[,ID])
	
	##### jitter if needed
	if (jitter){
			jit = geom_jitter(width=.2, height=.2, alpha=.5)
		} else {
			jit = geom_point(alpha = .5)
	}


	##### generate fixed effects predictions
	preds = compare.fits(formula, data, model, return.preds=T); preds = preds[,1:(ncol(preds)-1)]	#### this contains the fixed effect prediction
	names(preds)[ncol(preds)] = outcome
	
	#### make predictions for random effects
	new_data = d_new
	new_data[,outcome]= predict(model, new_data, type="response")


	##### plot it
	### if it's categorical
	if (is.factor(data[,predictors[1]])){
		ggplot(data=d_new, aes_string(predictors[1], outcome)) +
			jit +
			#geom_abline(aes(slope=fixed.slope, intercept=fixed.intercept), col="red", size=2) +
			geom_point(data=preds, aes_string(predictors, outcome), col="red", size=4) + #### fixed effect line
			geom_point(data=new_data, aes_string(predictors, outcome), col="gray", size=4, shape=17) + 
			facet_wrap(as.formula(paste0("~", ID)), labeller = labeller(.cols=label_both)) +				
			theme_bw()		
	} else {
		ggplot(data=d_new, aes_string(predictors[1], outcome)) +
			#geom_abline(aes(slope=fixed.slope, intercept=fixed.intercept), col="red", size=2) +
			geom_line(data=preds, aes_string(predictors, outcome), col="red") + #### fixed effect line
			geom_line(data=new_data, aes_string(predictors, outcome), col="gray") +
			jit +			
			facet_wrap(as.formula(paste0("~", ID)), labeller = labeller(.cols=label_both)) +				
			theme_bw()
		}
}