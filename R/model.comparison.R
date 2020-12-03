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
browser()
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
  
  # return predictions for binary variables
  predictions = compare_sensitivity_specificity(model1, model2, m1.name, m2.name)
  
  # return AIC/BIC/etc table
  model.table = model_comparison_table(model1, model2, m1.name, m2.name, nested)
  
  #### compute difference in predicted value (scaled)
  differences=standardized_differences(model1, model2)
  
  final_output = list(accuracy_table = predictions, statistics = model.table, predicted_differences = differences)
  return(final_output[!sapply(final_output, is.null)])

}

# done
model_comparison_table = function(model1, model2, m1.name="Full", m2.name="Reduced", nested) {
  
  # check if AIC can be computed
  if(!is.null(attr((try(AIC(model1), silent=TRUE)), "class")[1]) | !is.null(attr((try(AIC(model2), silent=TRUE)), "class")[1])) {
    return(NULL)
  }
  
  aic = c(AIC(model1), AIC(model2))
  bic = c(BIC(model1), BIC(model2))
  bayes.factor = bf.bic(model1, model2)
  p = get_p_value(model1, model2, nested)
  rsq = get_r_squared(model1, model2)
  
  ### make sure bayes factor is attached to the more likely model
  if ((bic[1]<=bic[2] & bayes.factor<=1) | bic[2]<=bic[1] & bayes.factor>=1){
    model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(1/bayes.factor, bayes.factor), p = c(p, NA), rsq = rsq)  
  } else if ((bic[2]<bic[1] & bayes.factor<1) | (bic[1]<bic[2] & bayes.factor>1)){
    model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(bayes.factor, 1/bayes.factor), p = c(p, NA), rsq = rsq)  
  }  
  row.names(model.table) = c(m1.name, m2.name)
  
  # format the numbers nicely
  if (is.na(model.table$p[1])){
    model.table = round(model.table[,!is.na(model.table[1,])], digits=3)	
  } else {
    model.table = round(model.table[,!is.na(model.table[1,])], digits=3)
    model.table[is.na(model.table)] = ""
    model.table$p[1] = format.pval(model.table$p[1], digits=3)
  }
  
  # get rid of columns with NA
  keep = colSums(apply(model.table, 2, is.na))!=2
  
  return(model.table[,keep])
}


# done
get_p_value = function(model1, model2, nested=TRUE) {
  
  if (!nested) return(NA)
  if (class(model1)[1]=="lm") return(sort(anova(model1, model2)[,"Pr(>F)"]))
  if (class(model1)[1]=="glm") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chi)"]))
  if (class(model1)[1]=="glmerMod" | class(model1)[1] == "lmerMod") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chisq)"]))
}

get_r_squared = function(model1, model2){
  if (class(model1)[1] == "lm" & class(model2)[1] == "lm") {
    rsq1 = summary(model1)$r.squared %>% round(digits=3)
    rsq2 = summary(model2)$r.squared%>% round(digits=3)
    return(c(rsq1, rsq2))
  }
  
  return(c(NA, NA))
  
}


compare_sensitivity_specificity = function(model1, model2, m1.name, m2.name) {
  
  # make sure outcome is binary
  if (!is_model_outcome_binary(model1) | !is_model_outcome_binary(model2)) return(NULL)
  
  # create sensitivity/specificity/etc.
  mod1.predictions = sensitivity.table(model1)
  mod2.predictions = sensitivity.table(model2)	
  
  # figure out which model has more variables, then subtract the differences
  vars1 = all.vars(model1); vars2 = all.vars(model2)
  if (length(vars1)>length(vars2)){
    difference = unlist(mod1.predictions) - unlist(mod2.predictions)
  } else {
    difference = unlist(mod2.predictions) - unlist(mod1.predictions)
  }
  
  # create table of predicted difference
  predictions = rbind(unlist(mod1.predictions), unlist(mod2.predictions), difference) %>% 
    data.frame() 
  row.names(predictions) = c(m1.name, m2.name, "difference")
  
  return(predictions)
}

# done
is_model_outcome_binary = function(model) {
  dv = extract_data_from_fitted_object(model)[,1]
  if (length(unique(dv))==2) return(TRUE) else return(FALSE)
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