##' Compare the fits of two models
##'
##' This function takes two fitted models as input and plots them to visually compare how the two differ in terms of fit.
##' It can take a \code{glm}, \code{rlm}, \code{lm}, and \code{randomForest} model (and maybe others as well). The function takes
##' a \code{\link{flexplot}}-like formula as input.  
##'	
##' @param formula A formula that can be used in flexplot. The variables inside must not include variables outside the fitted models. 
##' @param data The dataset containing the variables in formula
##' @param model1 The fitted model object (e.g., lm) containing the variables specified in the formula
##' @param model2 The second fitted model object (e.g., lm) containing the variables specified in the formula
##' @param return.preds Should the function return the predictions instead of a graphic? Defaults to F
##' @param report.se Should standard errors be reported alongside the estimates? Defaults to F. 
##' @param re Should random effects be predicted? Only applies to mixed models. Defaults to F. 
##' @param pred.type What type of predictions should be outputted? This is mostly for \code{glm} models. Defaults to "response." 
##' @param ... Other parameters passed to flexplot
##' @author Dustin Fife
##' @return Either a graphic or the predictions for the specified model(s)
##' @export
##' @examples 
##' data(exercise_data)
##' mod1 = lm(weight.loss~therapy.type + motivation, data=exercise_data)
##' mod2 = lm(weight.loss~therapy.type * motivation, data=exercise_data)
##' compare.fits(weight.loss~therapy.type | motivation, data=exercise_data, mod1, mod2)
compare.fits = function(formula, data, model1, model2=NULL, return.preds=F, report.se=F, re=F, pred.type="response", ...){
	#### if mod2 is null..
	if (is.null(model2)){
		model2 = model1
		old.mod = 1
	} else {
		old.mod = 0
	}

	#### get type of model
	model1.type = class(model1)[1]
	model2.type = class(model2)[1]	


	#### extract the terms from each MODEL
	testme1 = formula(model1); terms.mod1=all.vars(testme1)[-1]
	testme2 = formula(model2); terms.mod2=all.vars(testme2)[-1]
	testme = unique(c(all.vars(testme1)[-1], all.vars(testme2)[-1]))
	
	
	##### extract variable names
	variables = all.vars(formula)
    outcome = variables[1]
    predictors = variables[-1]
    
    ##### make sure they're putting the same variables from formula in terms
	if (!(all(predictors %in% testme))){
		stop(paste0("Sorry, but some variables in formula don't match what's in the model. Specifically: ", paste0(variables[!(variables%in%terms.mod1)], collapse=",")))
	}
	
	##### make sure they're using the right dataset
	if (!(all(predictors %in% names(data)))){
		stop(paste0("Sorry, but some variables in formula don't match what's in the dataset. Specifically: ", paste0(variables[!(variables%in%data)], collapse=","), ". Did you input the wrong dataset?"))
	}	
	
    #### for the rare occasion where deleting missing data changes the levels...
    if (length(predict(model1))<nrow(data) | length(predict(model2))<nrow(data)){
      data = na.omit(data[,variables])
    }    
    
	  #### create random column just to make the applies work (yeah, it's hacky, but it works)
    data$reject = 1:nrow(data); data$reject2 = 1:nrow(data)
    predictors = c(predictors, "reject", "reject2")

    #### get variable types
    numb = names(which(unlist(lapply(data[,predictors], is.numeric))))
    cat = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))

    ##### make "quadriture" points for quant variables
    var.mins = apply(data[, numb], 2, min, na.rm=T)
    var.max = apply(data[, numb], 2, max, na.rm=T)    
    min.max = data.frame(var.mins, var.max); min.max$size = c(50, rep(10, nrow(min.max)-1))
	f = function(d){seq(from=d[1], to=d[2], length.out=d[3])}
	min.max = as.list(apply(min.max, 1, f))

    #### get unique values for categorical vars
    if (length(cat)==1){
    	un.vars = lapply(data[cat], unique)    	
    } else {
		un.vars =lapply(data[,cat], unique); names(un.vars) = cat
	}

    
    #### combine into one dataset
    all.vars = c(min.max, un.vars)    
    #### get rid of extra variables
    tot.vars = length(predictors)
    rejects = grep("reject", names(all.vars))
	all.vars = all.vars[-rejects]
	all.vars = lapply(all.vars, function(x) x[!is.na(x)])
	pred.values = expand.grid(all.vars)



	# ##### look for interactions and remove them
	# if (length(grep(":", terms.mod1))>0){
	# 	terms.mod1 = terms.mod1[-grep(":", terms.mod1)]
	# 	model1.type = ifelse(model1.type=="lm", "interaction", model1.type)
	# }
	# if (length(grep(":", terms.mod2))>0){
	# 	terms.mod2 = terms.mod2[-grep(":", terms.mod1)]
	# 	model2.type = ifelse(model2.type=="lm", "interaction", model2.type)
	# }	
	# 
	# ##### look for polynomials and remove them
	# if (length(grep("^2", terms.mod1, fixed=T, value=T))>0 ){
	# 	terms.mod1 = terms.mod1[-grep("^2", terms.mod1, fixed=T)]
	# 	model1.type = ifelse(model1.type=="lm", "polynomial", model1.type)
	# }
	# if (length(grep("^2", terms.mod2, fixed=T, value=T))>0 & model1.type=="lm"){
	# 	terms.mod2 = terms.mod2[-grep("^2", terms.mod1, fixed=T)]
	# 	model2.type = ifelse(model2.type=="lm", "polynomial", model2.type)
	# }	
	

	
	#### if the outcome is an ordered factor...
		
	
	#### if it's not in model 1:
	#### input the mean (if numeric) or a value (if categorical)
	if (length(which(!(terms.mod1 %in% predictors)))>0){
		not.in.there = terms.mod1[which(!(terms.mod1 %in% predictors))]
		for (i in 1:length(not.in.there)){
			if (is.numeric(data[,not.in.there[i]])){
				message(paste0("Note: You didn't choose to plot ", not.in.there[i], " so I am inputting the median\n"))
				pred.values[,not.in.there[i]] = median(data[,not.in.there[i]], na.rm=T)
			} else {
				val = unique(data[,not.in.there[i]])[1]
				message(paste0("Note: You didn't choose to plot ", not.in.there[i], " so I am inputting '", val, "'\n"))
				pred.values[,not.in.there[i]] = val
			}
		}
	}
 

	#### generate predictions
	if (model1.type == "lmerMod" | model1.type == "glmerMod"){
		pred.mod1 = data.frame(prediction = predict(model1, pred.values, type="response", re.form=NA), model= "fixed effects")		
	} else if (model1.type == "polr"){
		pred.mod1 = data.frame(prediction = predict(model1, pred.values, type="class", re.form=NA), model= model1.type)		
	} else if (model1.type == "lm" | model1.type == "polynomial" | model1.type=="interaction"){
		int = ifelse(report.se, "confidence", "none")
		pred.mod1 = data.frame(prediction = predict(model1, pred.values, interval=int), model=model1.type)
	} else {	
		int = ifelse(report.se, "confidence", "none")
		pred.mod1 = data.frame(prediction = predict(model1, pred.values, type=pred.type, interval=int), model= model1.type)		
	}

	#### generate separate predictions for random effects
	if ((model2.type == "lmerMod" | model2.type == "glmerMod") & re){
		pred.mod2 = data.frame(prediction = predict(model2, pred.values, type="response"), model= "random effects")	
		old.mod=0	
	} else if ((model2.type == "lmerMod" | model2.type == "glmerMod") & !re){
		pred.mod2 = pred.mod1
	} else if (model2.type == "polr"){
		pred.mod2 = data.frame(prediction = predict(model2, pred.values, type="class", re.form=NA), model= model2.type)		
	} else if (model2.type == "lm" | model2.type == "polynomial" | model2.type=="interaction"){
		int = ifelse(report.se, "confidence", "none")
		pred.mod2 = data.frame(prediction = predict(model2, pred.values, interval="confidence")[,1], model=model2.type)
	} else {
		pred.mod2 = data.frame(prediction = predict(model2, pred.values, type= pred.type), model= model2.type)		
	}
	
	#### convert polyr back to numeric (if applicable)
	if (model1.type == "polr" | model2.type == "polr"){
		data[,outcome] = as.numeric(as.character(data[,outcome]))		
		pred.mod1$prediction = as.numeric(as.character(pred.mod1$prediction))
		pred.mod2$prediction = as.numeric(as.character(pred.mod2$prediction))		
	}

		#### if they have the same name, just call them model1 and model2
	if (!re){
		pred.mod1$model = paste0(deparse(substitute(model1)), " (", model1.type, ")", collapse="")
    if (pred.mod1$model[1] == pred.mod2$model[1]){
      pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, " 2)", collapse="")
    } else {
      pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, ")", collapse="")
    }
	}
	
	#### report one or two coefficients, depending on if they supplied it
	if (old.mod==0){
		prediction.model = rbind(pred.mod1, pred.mod2)
		prediction.model = cbind(pred.values, prediction.model)
	} else {
		prediction.model = pred.mod1
		prediction.model = cbind(pred.values, prediction.model)
	}


	#### eliminate those predictions that are higher than the range of the data
	if (!is.factor(data[,outcome])){
	min.dat = min(data[,outcome], na.rm=T); max.dat = max(data[,outcome], na.rm=T)
		if (length(which(prediction.model$prediction>max.dat)>0 | length(which(prediction.model$prediction<min.dat)))){
			prediction.model  = prediction.model[-which(prediction.model$prediction>max.dat | prediction.model$prediction<min.dat), ]
		}
	} else {
		#### if they supply a factor, convert it to a number!!!!!
		prediction.model$prediction = round(as.numeric(as.character(prediction.model$prediction)), digits=3)
	}

	#### create flexplot
	if (return.preds){
		prediction.model
	} else {
        
		flexplot(formula, data=data, prediction=prediction.model, suppress_smooth=T, se=F, ...)
	}	

}	