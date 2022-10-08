#' Report Estimates (effect sizes and parameters)
#'
#' Report object Estimates
#' @param object a object
#' @param mc Should model comparisons be performed?
#' @export
estimates = function(object, mc=TRUE){
	UseMethod("estimates")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @param object a object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.default = function(object, mc=TRUE){
	out = summary(object)
	class(out) = "estimates"
	out
}

#' Report lmerMod object Estimates (effect sizes and parameters)
#'
#' Report lmerMod object Estimates
#' @param object a lm object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.lmerMod = function(object, mc=TRUE){
  fixed = lme4::fixef(object)
  rand = lme4::VarCorr(object)

  
  # fit a baseline model
  baseline = fit_baseline_model(object)
  icc_stats = unlist(icc(baseline))
  
  # compute rsq 
  rsq = model.comparison(object, baseline)$r_squared_change
  
  # return the objects
  ret = list( fixed = fixed,
              r.squared=rsq,
              rand = rand, 
              icc = icc_stats
             )
  attr(ret, "class") = "lmer_estimates"
  return(ret)
}

#' Print lmer_estimates Summary
#'
#' Print a lmer_estimates object
#' @aliases print.lmer_estimates
#' @param x an lmer_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.lmer_estimates = function(x,...){
    cat(paste("Fixed Effects: \n", sep=""))
    print(x$fixed)
    cat(paste("\n\nRandom Effects: \n", sep=""))
    print(x$rand)
    cat(paste("\n\nICC and Design Effect: \n", sep=""))
    print(x$icc)
    cat(paste("\n\nR Squared: \n\n", sep=""))
    print(x$r.squared)    
}	



fit_baseline_model = function(object) {
  dv = get_terms(object)$response
  re = get_re(object)
  form = as.formula(
                paste0(dv, "~1+(1|", re, ")")
              )
  return(update(object, formula=form))
}


#' Report lm object Estimates (effect sizes and parameters)
#'
#' Report lm object Estimates
#' @param object a lm object
#' @param mc Should model comparisons be performed?
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.lm = function(object, mc=TRUE){
	
	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	variables = as.character(attr(terms(object), "variables")); variables = variables[-1]
	outcome = variables[1]
	predictors = variables[-1]
	
	# for intercept only models, return the mean
	if (length(predictors) == 0 ) {
	  f = as.formula(paste0(outcome, "~1"))
	  est = compare.fits(formula = f, data=object$model, model1=object, model2=NULL, return.preds=T, report.se=T)[1,]
	  return = est[1:3]
	  names(return) = c("Mean", "Lower", "Upper")
	  return$d = coef(object)/summary(object)$sigma
	  return(return)
	}
	    
  #### look for interaction terms
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = object$model
	
  #### identify factors
	variable_types = which_terms_are_factors_or_numbers(d, terms)
	factors = variable_types$factors
	numbers = variable_types$numbers
	
	# compute semi-partial
  semi.p = compute_semi_partial(object)
  
  # get summaries for factors
  factor_summaries = populate_estimates_factors(object, factors)
  coef.matrix = factor_summaries$coef.matrix
  difference.matrix = factor_summaries$difference.matrix
  
  # get summaries for numeric
  coef.matrix.numb = populate_estimates_numeric(object, numbers)
	
  # compute r squared and its CI
  r.squared = compute_r_squared(object)
	
	#### report correlation
  correlation = compute_correlation(object)
  
  # model comparison (I think this is only for JASP)
  mod.comps = return_model_comparisons(object, terms, mc)

	ret = list(r.squared=r.squared, semi.p=semi.p, correlation = correlation, factor.summary = coef.matrix, 
	           difference.matrix=difference.matrix, factors=factors, numbers.summary=coef.matrix.numb, numbers=numbers, 
	           sigma=summary(object)$sigma,
	           model.comparison = mod.comps)
	attr(ret, "class") = "estimates"
	return(ret)
}


#' Report RandomForest object Estimates (effect sizes and parameters)
#'
#' Report RandomForest object Estimates
#' @param object a RandomForest object
#' @param mc Should model comparisons be performed? 
#' Currently not implemented for RandomForest objects
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.RandomForest = function(object, mc=TRUE) {
  
  y = unlist(attr(object, "data")@get("response"))
  ### compute OOB
  
  oob = predict(object, OOB=T, type="response")
  if (!is.numeric(y)){
    numeric=F
    oob = round(length(which(oob==y))/length(y), digits=3)
    rsq = NA
  } else {
    ### quantile of differences
    numeric = T
    rsq = cor(oob, y)[1,1]^2
    oob = round(quantile(abs(oob-y)), digits=3)
  }
  
  #### compute variable importance
  importance = party::varimp(object)
  if (!numeric){
    importance = round(sort(importance, decreasing=T), digits=4)
  } else {
    vals = sort(importance, decreasing=T)
    vals[vals<0] = 0
    importance = round(sqrt(vals), digits=3)
  }
  
  estimates = list(oob=oob, rsq = rsq, importance=importance)
  attr(estimates, "class") = "rf_estimates"
  attr(estimates, "numeric") = numeric
  return(estimates)
  
}

#' Print rf_estimates Summary
#'
#' Print a rf_estimates object
#' @aliases print.rf_estimates
#' @param x an rf_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.rf_estimates = function(x,...){
  if (attr(x, "numeric")) {
    cat(paste("\n\nQuantiles of absolute value of OOB performance (i.e., abs(predicted - actual)):\n\n", sep=""))
    print(x$oob)
    cat(paste("\n\nModel R Squared:\n\n", sep=""))
    print(x$rsq)
    cat(paste("\n\nVariable importance (root MSE of predicted versus permuted):\n\n", sep=""))
    print(x$importance)
  } else {
    cat(paste("\n\nOOB accuracy in prediction:\n\n", sep=""))
    cat(x$oob)
    cat(paste("\n\nVariable importance (mean decrease in accuracy when permuted):\n\n", sep=""))
    print(x$importance)
  }
  
}	

#' Report glm object Estimates (effect sizes and parameters)
#'
#' Report glm object Estimates
#' @param object a glm object
#' @param mc Should model comparisons be performed? Currently not used
#' @return One or more objects containing parameter estimates and effect sizes
#' @export
estimates.glm = estimates.glmerMod = function(object, mc=FALSE){
  
	#### generate list of coefficients
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = extract_data_from_fitted_object(object)
	
	#### identify factors
	if (length(terms)>1){
		factors = names(which(unlist(lapply(d[,terms], function(x) is.factor(x) | is.character(x)))));
		#d[,factors] = apply(d[,factors, drop=FALSE], 2, as.factor)
		numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
	} else {
		factors = terms[which(is.factor(d[,terms]) | is.numeric(d[,terms]))]
		#d[,factors] = as.factor(d[,factors])
		numbers = terms[which(is.numeric(d[,terms]))]
	}
	
	#### output predictions
	if (class(object)[1]=="glmerMod") {
	  preds = NA
	} else {
	n.func = function(term){
	  anchor.predictions(object, term, shutup=T)$prediction
	  }
	preds = lapply(terms, n.func); names(preds) = terms
	}
	

	#### output coefficients
	
	if (class(object)[1] == "glmerMod" & family(object)$link == "logit"){
	  coef.matrix = data.frame(raw.coefficients = lme4::fixef(object), 
	                           OR = exp(lme4::fixef(object)), 
	                           inverse.OR = 1/exp(lme4::fixef(object)) 
	                           )
	} else if (family(object)$link=="logit"){
		coef.matrix = data.frame(raw.coefficients = coef(object), 
		                         OR = exp(coef(object)), 
		                         inverse.OR = 1/exp(coef(object)), 
		                         standardized.OR = exp(standardized.beta(object, sd.y=F)), 
		                         inverse.standardized.OR = 1/exp(standardized.beta(object, sd.y=F)))
		
	} else if (family(object)$link=="log"){
		coef.matrix = data.frame(raw.coefficients = coef(object), 
				multiplicative.coef = exp(coef(object)), 
				std.mult.coef = exp(standardized.beta(object, sd.y=F)))
	} else if (family(object)$link=="inverse"){
		coef.matrix = data.frame(raw.coefficients = coef(object), 
				inverse.coef = 1/(coef(object)), 
				std.mult.coef = 1/(standardized.beta(object, sd.y=F)))
	}
	
	#options(warn=0)
	if (!is.na(preds)[1] & length(numbers)>0){
	  coef.matrix[numbers,"Prediction Difference (+/- 1 SD)"] = 
	    sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
	  nms = row.names(coef.matrix); nms2 = names(coef.matrix)
  	coef.matrix = data.frame(lapply(coef.matrix, function(y) if(is.numeric(y)) round(y, 3) else y), row.names=nms); names(coef.matrix) = nms2
	}
	
	#### for those that are factors, put the first prediction in the -1 SD column
	string.round = function(x, digits){
		return.val = ifelse(round(x, digits)==0, paste0("<0.", rep(0, times=digits-1), "1"), round(x, digits=digits))
		return.val
	}

	
	if (length(factors)>0){
	for (i in 1:length(factors)){
		current.pre = preds[factors[i]]
		levs = unique(d[,factors[i]]); levs = paste0(factors[i], levs)
		# find that level in the coef.matrix
		im.here = (levs %in% row.names(coef.matrix))
		
		if (length(which(im.here))>0) {
  		coef.matrix[levs[im.here],"Prediction Difference (+/- 1 SD)"] = 
	  	  paste0(string.round(unlist(current.pre)[which(im.here)] - 
	  	                      unlist(current.pre)[which(!im.here)], digits=2), " (relative to ", levs[!im.here], ")")
		}
		
		if (length(factors)==1){
			coef.matrix[1,"Prediction Difference (+/- 1 SD)"] = paste0(string.round(unlist(current.pre)[1], digits=2), " (", levs[1], " prediction)")
			row.names(coef.matrix)[1] = levs[1]
		}
	}}
	coef.matrix
}

#' Report zeroinfl object Estimates (effect sizes and parameters)
#'
#' Report zeroinfl object Estimates
#' @param object a zeroinfl object
#' @param mc Should model comparisons be performed? Currently not used
#' @return Estimates for a zero inflated model
#' @export
estimates.zeroinfl = function(object, mc=FALSE){
	
	#### get dataset
	d = object$model
	
	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	#### identify factors
	if (length(terms)>1){
		factors = names(which(unlist(lapply(d[,terms], is.factor))));
		numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
	} else {
		factors = terms[which(is.factor(d[,terms]))]
		numbers = terms[which(is.numeric(d[,terms]))]
	}	
	
	#### output predictions
	n.func = function(term){anchor.predictions(object, term, shutup=T)$prediction}
	preds = lapply(terms, n.func); names(preds) = terms
	
	#### output coefficients
	coef.matrix = data.frame(A = coef(object)[1:(length(terms)+1)], 
							B = coef(object)[(length(terms)+2):length(coef(object))])
	names(coef.matrix) = c(object$dist, object$link)
	row.names(coef.matrix) = subsetString(row.names(coef.matrix), "_", 2)
	coef.matrix[numbers,"Prediction Difference (+/- 1 SD)"] = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
	
	coef.matrix
	
}


remove_interaction_terms = function(object) {
  #### generate list of coefficients
  terms = attr(terms(object), "term.labels")
  interaction = length(grep(":", terms))>0
  if (interaction){
    terms = terms[-grep(":", terms)]
  }
  return(terms)
}


#' Print estimates Summary
#'
#' Print estimates Summary
#' @aliases print.estimates
#' @param x an estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.estimates = function(x,...){
	#### print summary
	cat(paste("Model R squared:\n", round(x$r.squared[1], digits=3), " (", round(x$r.squared[2], digits=2),", ", round(x$r.squared[3], digits=2),")\n\nSemi-Partial R squared:\n",sep=""))
	print(round(x$semi.p, digits=3))
	
	#### print correlation
	if (!is.na(x$correlation[1])){
	cat(paste("Correlation:\n", round(x$correlation[1], digits=3), "\n"))
	}
	
	#### replace NA with - 
	f = function(x){ x[is.na(x)] = "-"; x}
	if (length(x$factors)>0){
		cat(paste("\nEstimates for Factors:\n"))
		x$factor.summary[,3:ncol(x$factor.summary)] = round(x$factor.summary[,3:ncol(x$factor.summary)], digits=2)
		x$factor.summary[,3:ncol(x$factor.summary)] = apply(x$factor.summary[,3:ncol(x$factor.summary)], 2, f)
		#print(round(x$numbers.summary, digits=2))		
		print(x$factor.summary)
		cat(paste0("\n\nMean Differences:\n"))
		x$difference.matrix[,3:ncol(x$difference.matrix)] = round(x$difference.matrix[,3:ncol(x$difference.matrix)], digits=2)
		x$difference.matrix$variables[is.na(x$difference.matrix$variables)] = ""
		print(x$difference.matrix)		
	}
	if (length(x$numbers)>0){
		cat(paste("\n\nEstimates for Numeric Variables = \n"))
		x$numbers.summary[,2:ncol(x$numbers.summary)] = round(x$numbers.summary[,2:ncol(x$numbers.summary)], digits=2)
		#print(round(x$numbers.summary, digits=2))		
		print(x$numbers.summary)		
	}
	#cat(paste("\nsigma = ", round(x$sigma, digits=4), "\n\n"))
}
