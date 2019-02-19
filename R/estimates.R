#' Report Estimates (effect sizes and parameters)
#'
#' Report object Estimates
#' @aliases estimates.regression estimates estimates.ttest estimates.default
#' @param object a object
#' @export
estimates = function(object,...){
	UseMethod("estimates")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @aliases report.regression report report.ttest report.default
#' @param object a object
#' @export
estimates.default = function(object){
	out = summary(object)
	class(out) = "estimates"
	out
}

#' Report regression object Estimates (effect sizes and parameters)
#'
#' Report regression object Estimates
#' @aliases estimates.regression estimates
#' @param object a regression object
#' @export
estimates.regression = function(object){
	file.name = deparse(substitute(object))
	cat(paste("R squared:\n", round(object$R.squared[1], digits=3), " (", round(object$R.squared[2], digits=2),", ", round(object$R.squared[3], digits=2),")\n\nParameter Estimates:\n",sep=""))
	print(object$Estimates)
	cat(paste("\n\nr = ", round(object$r, digits=4), "\nsigma = ", round(object$Sigma, digits=4)))
}


#' Report glm object Estimates (effect sizes and parameters)
#'
#' Report glm object Estimates
#' @aliases estimates.glm estimates
#' @param object a glm object
#' @export
estimates.glm = function(object){
	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	#### get dataset
	d = object$model
	
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
	options(warn=-1)
	if (family(object)$link=="logit"){
		coef.matrix = data.frame(raw.coefficients = coef(object), OR = exp(coef(object)), inverse.OR = 1/exp(coef(object)), standardized.OR = exp(standardized.beta(object, sd.y=F)), inverse.standardized.OR = 1/exp(standardized.beta(object, sd.y=F)))
		
	} else if (family(object)$link=="log"){
		coef.matrix = data.frame(raw.coefficients = coef(object), 
				multiplicative.coef = exp(coef(object)), 
				std.mult.coef = exp(standardized.beta(object, sd.y=F)))
	} else if (family(object)$link=="inverse"){
		coef.matrix = data.frame(raw.coefficients = coef(object), 
				inverse.coef = 1/((object)), 
				std.mult.coef = 1/(standardized.beta(object, sd.y=F)))
	}
	
	
	options(warn=0)
	coef.matrix[numbers,"Prediction Difference (+/- 1 SD)"] = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})

	
	
	#### for those that are factors, put the first prediction in the -1 SD column
	string.round = function(x, digits){
		return.val = ifelse(round(x, digits)==0, paste0("<0.", rep(0, times=digits-1), "1"), round(x, digits=digits))
		return.val
	}


	if (length(factors)>0){
	for (i in 1:length(factors)){
		current.pre = preds[factors[i]]
		levs = levels(d[,factors[i]]); levs = paste0(factors[i], levs)
		coef.matrix[levs[-1],"Prediction Difference (+/- 1 SD)"] = paste0(string.round(unlist(current.pre)[-1] - unlist(current.pre)[1], digits=2), " (relative to ", levs[1], ")")
		
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
#' @aliases estimates.zeroinfl estimates
#' @param object a zeroinfl object
#' @export
estimates.zeroinfl = function(object){
	
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


#' Report lm object Estimates (effect sizes and parameters)
#'
#' Report lm object Estimates
#' @aliases estimates.lm estimates
#' @param object a lm object
#' @export
estimates.lm = function(object){
	#data(exercise_data); data=exercise_data
	#f = make.formula("weight.loss", c("gender", "motivation"))
	#object = lm(f, data=data)
	n = nrow(model.frame(object)) 
	
	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	variables = all.vars(formula(object))
    outcome = variables[1]
    
    #### look for interaction terms
	interaction = length(grep(":", terms))>0
	
	#### get dataset
	d = object$model
	
	#### identify factors
	if (length(terms)>1){
		factors = names(which(unlist(lapply(d[,terms], is.factor))));
		numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
	} else {
		factors = terms[which(is.factor(d[,terms]))]
		numbers = terms[which(is.numeric(d[,terms]))]
	}


	#### compute change in r squared
	ssr = drop1(aov(object))[-1,"Sum of Sq"]
	if (length(ssr)<(nrow(anova(object))-1)){
		cat("Note: I am not reporting the semi-partial R squared for the main effects because an interaction is present. To obtain main effect sizes, drop the interaction from your model. \n\n")
	}
	sst = sum(anova(object)[,"Sum Sq"])
	semi.p = ssr/sst	
	max = nrow(anova(object))-1
	min = max-length(semi.p)+1
	nms = row.names(anova(object))[min:max]	
	names(semi.p) = nms
	
if (length(factors)>0){
		#### generate table with names
		if (length(factors)==1){
			factor.names = levels(d[,factors])
			num.rows = length(factor.names)
			a = length(unique(factor.names))
			num.rows2 = (a*(a-1))/2
		} else {
			factor.names = unlist(lapply(d[,factors], levels))
			num.rows = sum(unlist(apply(d[,factors], 2, function(x) { length(unique(x))})))			
			num.rows2 = apply(d[,factors], 2, function(x){ a = length(unique(x)); (a*(a-1))/2})
		}

	
			#### create empty matrix with variable names
			#### identify the number of rows
			coef.matrix = data.frame(variables = rep("", num.rows), levels=NA, estimate=NA, lower=NA, upper=NA, df.spent=NA, df.remaining=NA)
			coef.matrix$variables = factor(coef.matrix$variables, levels=c("", factors))		
				#### write variable names/levels/df
			coef.matrix$df.spent = NA
			
				#### difference.matrix
				
			difference.matrix = data.frame(variables = 1:num.rows2, comparison = NA, difference=NA, 
					lower=NA, upper=NA, cohens.d=NA)	
			#difference.matrix$variables = factor(difference.matrix$variables, levels=c("", factors))		
				#### compute standardized estimates
			# coef.std = standardized.beta(object, se=T)
			# #### remove those that are numeric
			# if (length(numbers)>0){
				# coef.std$beta = coef.std$beta[-which(names(coef.std$beta) %in% numbers)]
				# coef.std$se = coef.std$se[-which(names(coef.std$se) %in% numbers)]			
			# }
			p = 1; p2=1; i=1
			for (i in 1:length(factors)){
				
				#### populate df based on levels
				levs = length(levels(d[,factors[i]]))
				levs2 = (levs*(levs-1))/2
				current.rows = p:(p+levs-1)
				current.rows2 = p2:(p2 + levs2-1)
				coef.matrix$levels[current.rows] = levels(d[,factors[i]])
				coef.matrix$df.spent[p] = levs-1
				
				#### populate variable names
				coef.matrix$variables[p] = factors[i]
	
				#### populate the estimates/lower/upper
				f = make.formula(outcome, factors[i])			
				est = compare.fits(f, data=d, object, return.preds=T, silent=T)
				coef.matrix$estimate[current.rows] = est$prediction.fit
				coef.matrix$lower[current.rows] = est$prediction.lwr
				coef.matrix$upper[current.rows] = est$prediction.upr
				
				#### populate standardized estimates
				# names(coef.std$beta) = gsub(factors[i], "", names(coef.std$beta))
				# names(coef.std$se) = gsub(factors[i], "", names(coef.std$se))				
				#### populate the df
				coef.matrix$df.remaining = object$df
				
				#### fill in the difference matrix
				difference.matrix$variables[p2] = factors[i]
				tk = data.frame(TukeyHSD(aov(object))[factors[i]])
				difference.matrix$comparison[current.rows2] = row.names(tk)
				difference.matrix[current.rows2,c("difference", "lower", "upper")] = tk[,1:3]
				
				difference.matrix$cohens.d[current.rows2] = difference.matrix$difference/summary(object)$sigma

				#### increment the counter
				p = p + levs
				p2 = p2+levs2
				
			}	
			
			
			# std.rows = 	coef.matrix$levels %in% names(coef.std$beta)
			# coef.matrix$std.estimate[std.rows] = coef.std$beta[-1]
			# lower.limits = coef.std$beta[-1] - 1.96*coef.std$se[-1]
			# upper.limits = coef.std$beta[-1] + 1.96*coef.std$se[-1]
			# coef.matrix$std.lower[std.rows] = lower.limits
			# coef.matrix$std.upper[std.rows] = upper.limits

			
	} else {
		coef.matrix = NA
		difference.matrix=NA
	}	
	#### NUMERIC VARIABLES
	if (length(numbers)>0){
		vars = c("(Intercept)", numbers)
		coef.matrix.numb = data.frame(variables=vars, estimate=NA, lower=NA, upper=NA, std.estimate=NA, std.lower=NA, std.upper=NA, df.spent=1, df.remaining=object$df)
		coef.matrix.numb$estimate = coef(object)[vars]
		
		#### get upper and lower using matrix multiplication
		upper.lower = t(matrix(coef(object)[vars], ncol=2, nrow=length(vars), byrow=F) + t(t(t(c(1.96,-1.96)))%*%t(summary(object)$coef[vars,2])))

		coef.matrix.numb$lower = (upper.lower)[2,]
		coef.matrix.numb$upper = (upper.lower)[1,]

		
		##### standardized estimates
		coef.std = standardized.beta(object, se=T)
			#### remove those that are numeric
		num = which(names(coef.std$beta) %in% numbers)	
		coef.std$beta = coef.std$beta[num]
		coef.std$se = coef.std$se[num]					
		coef.matrix.numb$std.estimate = c(0, coef.std$beta)
		upper.lower = t(matrix(c(0, coef.std$beta), ncol=2, nrow=length(vars), byrow=F) + t(t(t(c(1.96,-1.96)))%*%t(c(0, coef.std$beta))))
		coef.matrix.numb[,c("std.upper", "std.lower")] = t(upper.lower)	
		
	} else {
		coef.matrix.numb = NA
	}



	#### report R squared
	r.squared = summary(object)$r.squared
	t.crit = qt(.975, df=n-2)	
	se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
	r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
	r.squared = round(r.squared, digits=4)
	
	#### report correlation
	if (length(numbers)==1 & length(factors)==0){
		correlation = cor(d)[1,2]
	} else {
		correlation = NA
	}

	# #### print summary
	# cat(paste("Model R squared:\n", round(r.squared[1], digits=3), " (", round(r.squared[2], digits=2),", ", round(r.squared[3], digits=2),")\n\nSemi-Partial R squared:\n",sep=""))
	# print(semi.p)
	# if (length(factors)>0){
		# cat(paste("\nEstimates for Factors:\n"))
		# print(coef.matrix)
	# }
	# if (length(numbers)>0){
		# cat(paste("\n\nEstimates for Numeric Variables = \n"))
		# print(coef.matrix.numb)		
	# }
	# cat(paste("\nsigma = ", round(summary(object)$sigma, digits=4), "\n\n"))
	
	ret = list(r.squared=r.squared, semi.p=semi.p, correlation = correlation, factor.summary = coef.matrix, difference.matrix=difference.matrix, factors=factors, numbers.summary=coef.matrix.numb, numbers=numbers, sigma=summary(object)$sigma)
	attr(ret, "class") = "estimates"
	return(ret)
}


#' Print estimates Summary
#'
#' Print estimates Summary
#' @aliases print.estimates
#' @param x an estimates object
#' @param ... ignored
#' @export
print.estimates = function(x,...){
	#### print summary
	cat(paste("Model R squared:\n", round(x$r.squared[1], digits=3), " (", round(x$r.squared[2], digits=2),", ", round(x$r.squared[3], digits=2),")\n\nSemi-Partial R squared:\n",sep=""))
	print(round(x$semi.p, digits=3))
	
	#### print correlation
	cat(paste("Correlation:\n", round(x$correlation[1], digits=3)))
	
	#### replace NA with - 
	f = function(x){ x[is.na(x)] = "-"; x}
	if (length(x$factors)>0){
		cat(paste("\nEstimates for Factors:\n"))
		x$factor.summary[,3:ncol(x$factor.summary)] = round(x$factor.summary[,3:ncol(x$factor.summary)], digits=2)
		x$factor.summary[,3:ncol(x$factor.summary)] = apply(x$factor.summary[,3:ncol(x$factor.summary)], 2, f)
		#print(round(x$numbers.summary, digits=2))		
		print(x$factor.summary)
	}
	if (length(x$numbers)>0){
		cat(paste("\n\nEstimates for Numeric Variables = \n"))
		x$numbers.summary[,2:ncol(x$numbers.summary)] = round(x$numbers.summary[,2:ncol(x$numbers.summary)], digits=2)
		#print(round(x$numbers.summary, digits=2))		
		print(x$numbers.summary)		
	}
	cat(paste("\nsigma = ", round(x$sigma, digits=4), "\n\n"))
}