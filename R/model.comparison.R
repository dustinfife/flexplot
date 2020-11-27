##' Perform a model comparison
##'
##' Perform a model comparison
##'	
##' This function takes two models (either nested or not nested) and compares them in terms of AIC, BIC,
##' bayes factor, and the p-value
##' @param model1 the first model to be compared
##' @param model2 the second model to be compared
##' @author Dustin Fife
##' @export
model.comparison = function(model1, model2){

  
  #### find model types
  class.mod1 = class(model1)
  class.mod2 = class(model2)
  
  #### extract the names
  m1.name = deparse(substitute(model1))
  m2.name = deparse(substitute(model2))	
  
  ### check for nested functions
  nested = check_nested(model1, model2)
  
  ### handle missing data from one model to the next
  new_models = check_model_rows(model1, model2, nested)
  model1 = new_models[[1]]; model2 = new_models[[2]]
  
  #### if class is randomForest, tell them to do something else
  if (length(grep("randomForest", class.mod1))>0 | length(grep("randomForest", class.mod2))>0){
    message("Dear Friend. My sincere apologies, but I am limited in my abilities to compare randomForest models. 
Might I interest you in a suite of other functions, including compare.fits, perhaps? In the mean time, I'll give you *something* useful.")
    differences=standardized_differences(model1, model2)
    to.return = differences
  ### if they have two logistics they're comparing
  } else if (family(model1)$link=="logit" & family(model2)$link=="logit"){
    mod1.predictions = sensitivity.table(model1)
    mod2.predictions = sensitivity.table(model2)	
    vars1 = all.vars(model1); vars2 = all.vars(model2)
    predictions = data.frame(rbind(unlist(mod1.predictions), unlist(mod2.predictions))); row.names(predictions) = c(m1.name, m2.name)
    if (length(vars1)>length(vars2)){
      difference = unlist(predictions[1,]) - unlist(predictions[2,])
    } else {
      difference = predictions[2,] - predictions[1,]
    }
    
    aic = c(AIC(model1), AIC(model2))
    bic = c(BIC(model1), BIC(model2))
    bayes.factor = bf.bic(model1, model2)
    p = get_p_value(model1, model2)
    ### make sure bayes factor is attached to the more likely model
    if ((bic[1]<bic[2] & bayes.factor<1) | bic[2]<bic[1] & bayes.factor>1){
      model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(1/bayes.factor, bayes.factor), p = c(p, NA))  
    } else if ((bic[2]<bic[1] & bayes.factor<1) | (bic[1]<bic[2] & bayes.factor>1)){
      model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(bayes.factor, 1/bayes.factor), p = c(p, NA))  
    }  
    
    row.names(model.table) = c(m1.name, m2.name)
    
    predictions = data.frame(rbind(predictions, difference)); row.names(predictions)[3] = "Difference"
    to.return = list(statistics=model.table, predictions=predictions)
    
  ### for linear models and other things  
  } else {

    #### collect terms
  	mod1 = attr(terms(model1), "term.labels")
  	mod2 = attr(terms(model2), "term.labels")	
  	
  	#### check for nested models
  	if (all(length(mod1)>length(mod2) & (mod2 %in% mod1)) & class.mod1[1] == class.mod2[1]){
  		nested = T
  	} else if (all(length(mod2)>=length(mod1) & mod1 %in% mod2)) {
  		nested = T
  	} else {
  		nested = F
  	}
  	
  	if (nested & class(model1)[1] == "lm"){
  	  #### check for different Ns for each dataset (if missing data was omitted)
  	  if (nrow(model1$model) != nrow(model2$model)){
  	    message(paste0("Note: your models were fit to two different datasets. ",
  	                   "This is *probably* because you have missing data in one, but not the other.",
  	                   "I'm going to make the dangerous assumption this is the case and do some ninja moves",
  	                   " in the background (hiya!). If you don't want me to do this, handle the missing data in advance", sep=""))
  	    
  	    ### refit the larger n model with the smaller dataset
  	    if (nrow(model1$model)>nrow(model2$model)){
  	      model1 = update(model1, data=model2$model)
  	    } else {
  	      model2 = update(model2, data=model1$model)
  	    }
  	  }
  	  
  	  
  		anova.res = anova(model1, model2)
  		p = unlist(anova.res["Pr(>F)"])[2]
  		r.squared = c(summary(model1)$r.squared, summary(model2)$r.squared)
  		#1-pchisq( abs(anova.res$Deviance[2]), abs(anova.res$Df[2]))
  	} else if (nested & class(model1)[1] == "lmerMod"){
  	  anova.res = anova(model1, model2)
  	  p = unlist(anova.res["Pr(>Chisq)"])[2]
  		r.squared = c(NA, NA)
  	} else if (nested & class(model1)[1] == "glm") {
  	  anova.res = anova(model1, model2, test="LRT")
  	  p = unlist(anova.res["Pr(>Chi)"])[2]
  	  r.squared = c(NA, NA)
  	} else {
  	  p = NA
  	  r.squared = c(NA, NA)
  	}
  	
  	aic = c(AIC(model1), AIC(model2))
  	bic = c(BIC(model1), BIC(model2))
  	bayes.factor = bf.bic(model1, model2)
  	
  	### make sure bayes factor is attached to the more likely model
  	if ((bic[1]<bic[2] & bayes.factor<1) | bic[2]<bic[1] & bayes.factor>1){
  	  model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(1/bayes.factor, bayes.factor), p.value=c(p, NA), r.squared=r.squared)  
  	} else if ((bic[2]<bic[1] & bayes.factor<1) | (bic[1]<bic[2] & bayes.factor>1)){
  	  model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(bayes.factor, 1/bayes.factor), p.value=c(p, NA), r.squared=r.squared)  
  	} 
  	  
  
  	#browser()
  	row.names(model.table) = c(m1.name, m2.name)
  	if (is.na(model.table$p.value[1])){
  		model.table = round(model.table[,!is.na(model.table[1,])], digits=3)	
  	} else {
  		model.table = round(model.table[,!is.na(model.table[1,])], digits=3)
  		model.table[is.na(model.table)] = ""
  		model.table$p.value[1] = format.pval(p, digits=3)
  	}

  	#### compute difference in predicted value (scaled)
  	differences=standardized_differences(model1, model2)
  	to.return = list(statistics=model.table, pred.difference = differences)
  	## for lmerMod, report change in R squared
  	if (class(model1)[1] == "lmerMod" & class(model2)[1] == "lmerMod" & nested) {
  	  if (length(mod1)<length(mod2)){
  	    r_squared_change = rsq_change(model2, model1)
  	  } else {
  	    r_squared_change = rsq_change(model1, model2)
  	  }
  	  to.return$r_squared_change =  r_squared_change
  	}  	
  	
  }
  

	return(to.return)
}

get_p_value = function(model1, model2 ) {
  if (class(model1)[1]=="glm" & family(model1)$link=="logit") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chi)"]))
  if (class(model1)[1]=="glmerMod" & family(model1)$link=="logit") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chisq)"]))
}

check_model_rows = function(model1, model2, nested) {
  
  # if they're not nested, it doesn't matter. Just return the models
  if (!nested) {
    return(list(model1, model2))
  }
  
  data_1 = extract_data_from_fitted_object(model1)
  data_2 = extract_data_from_fitted_object(model2)
  
  # if theyre not the same, give a message
  if (nrow(data_1) != nrow(data_2)) {
    msg = paste0("Note: your models were fit to two different datasets. ",
                 "This is *probably* because you have missing data in one, but not the other.",
                 "I'm going to make the dangerous assumption this is the case and do some ninja moves",
                 " in the background (hiya!). If you don't want me to do this, handle the missing data in advance", sep="")
    message(msg)
    
    ### refit the larger n model with the smaller dataset
    if (nrow(model1$model)>nrow(model2$model)){
      model1 = update(model1, data=data_2)
    } else {
      model2 = update(model2, data=data_1)
    }    
  }

  return(list(model1, model2))
}  
  
##' Compute sensitivity/specificity/etc. 
##'
##' Compute sensitivity/specificity/etc. 
##'	
##' This function computes sensitivity, specificity, positive/negative predictive value and accuracy and reports
##' them as a list. 
##' @param object an object that can be predicted (e.g., glm). Note the thing to be predicted must have only two outcomes
##' @author Dustin Fife
##' @export
sensitivity.table = function(object){

  if (class(object)[1] == "RandomForest") {
    predmat = table(Observed = attr(object, "responses")@variables[,1],
                    Predicted = predict(object))
  } else {
    predmat = generate_predictions_table(object)
  }
  
  TP = predmat[2,2]
	FP = predmat[2,1]
	TN = predmat[1,1]
	FN = predmat[1,2]
	sens = TP/(TP+FN)
	spec = TN/(TN + FP)
	ppv = TP/(FP+TP)
	npv = TN/(TN+FN)
	acc = (TP+TN)/(TP+FP+TN+FN)
	list(acc=acc,sens=sens, spec=spec, ppv=ppv, npv=npv)
}


generate_predictions_table = function(object) {
  data = extract_data_from_fitted_object(object)
  dv_name = all.vars(formula(object))[1]
  dv = data[[dv_name]]
  predictions = check_logistic_all_same(object)
  predmat = table(Observed=dv, Predicted=predictions)
  predmat
  
}

check_logistic_all_same = function(object) {
  predictions = round(predict(object, type="response"))
  
  if (var(predictions) != 0) return(predictions)
  
  # convert predictions to a factor (to preserve zeroes)
  predictions = factor(predictions, levels=c(0,1), labels=c(0,1))
  return(predictions)
}