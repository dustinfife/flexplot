


# done
get_p_value = function(model1, model2, nested=TRUE) {
  
  if (!nested) return(NA)
  if (class(model1)[1]=="lm") return(sort(anova(model1, model2)[,"Pr(>F)"]))
  if (class(model1)[1]=="glm") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chi)"]))
  if (class(model1)[1]=="glmerMod" | class(model1)[1] == "lmerMod") return(sort(anova(model1, model2, test="LRT")[,"Pr(>Chisq)"]))
  if (class(model1)[1]=="zeroinfl") return(pchisq(logLik(mod1)-logLik(mod2), df=1, lower.tail=F))
}

get_r_squared = function(model1, model2, nested=TRUE){
  
  mod1_terms = attr(terms(model1), "term.labels")
  mod2_terms = attr(terms(model2), "term.labels")
  
  if (class(model1)[1] == "lm" & class(model2)[1] == "lm") {
    rsq1 = summary(model1)$r.squared %>% round(digits=3)
    rsq2 = summary(model2)$r.squared %>% round(digits=3)
    return(c(rsq1, rsq2))
  }
  # mixed models
  if (class(model1)[1] == "lmerMod" & class(model2)[1] == "lmerMod" & nested) {
    if (length(mod1_terms)<length(mod2_terms)) return(rsq_change(model2, model1))
    return(rsq_change(model1, model2))
  }
  
  return(c(NA, NA))
  
}

get_adj_r_squared = function(model1, model2, nested=TRUE){
  
  mod1_terms = attr(terms(model1), "term.labels")
  mod2_terms = attr(terms(model2), "term.labels")
  
  if (class(model1)[1] == "lm" & class(model2)[1] == "lm") {
    rsq1 = summary(model1)$adj.r.squared %>% round(digits=3)
    rsq2 = summary(model2)$adj.r.squared %>% round(digits=3)
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
    msg = paste0("Note: your models were fit to two different datasets.\n",
                 "This is *probably* because you have missing data in one, but not the other.\n",
                 "I'm going to make the dangerous assumption this is the case and do some ninja moves\n",
                 " in the background (hiya!). If you don't want me to do this, handle the missing data in advance\n", sep="")
    message(msg)
    
    ### refit the larger n model with the smaller dataset
    ### note: lme4 requires the datasets to have the same name so I'm refitting both
    if (nrow(data_1)>nrow(data_2)){
      model1 = update(model1, data=data_2)
      model2 = update(model2, data=data_2)
    } else {
      model2 = update(model2, data=data_1)
      model1 = update(model1, data=data_1)
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
##' @return A table containing sensitivity/specificity/etc.
##' @export
sensitivity.table = function(object){
  
  if (class(object)[1] == "RandomForest") {
    predmat = table(Observed = attr(object, "responses")@variables[,1],
                    Predicted = predict(object))
  } else {
    predmat = generate_predictions_table(object)
  }
  
  TP = predmat[2,2]
  FP = predmat[1,2]
  TN = predmat[1,1]
  FN = predmat[2,1]
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