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
  
  # return predictions for binary variables
  predictions = compare_sensitivity_specificity(model1, model2, m1.name, m2.name)
  
  # return AIC/BIC/etc table
  model.table = model_comparison_table(model1, model2, m1.name, m2.name, nested)
  
  #### compute difference in predicted value (scaled)
  differences=standardized_differences(model1, model2)
  
  # for mixed models, put as separate R squared
  if (class(model1)[1] == "lmerMod" & class(model2)[1] == "lmerMod" & nested) {
    r_squared_change = get_r_squared(model1, model2, nested)
    model.table[,"rsq"] = NULL
    final_output = list(
                        statistics = model.table, 
                        predicted_differences = differences, 
                        r_squared_change = r_squared_change)
    return(final_output)
  }
  
  final_output = list(accuracy_table = predictions, statistics = model.table, predicted_differences = differences)
  return(final_output[!sapply(final_output, is.null)])

}

# done

model_comparison_table = function(model1, model2, m1.name="Full", m2.name="Reduced", nested) {
  
  # check if AIC can be computed
  if(!is.null(attr((try(AIC(model1), silent=TRUE)), "class")[1]) | !is.null(attr((try(AIC(model2), silent=TRUE)), "class")[1])) {
    return(NULL)
  }
  
  # check if AIC is infinity (can be when you have a poisson)
  if(is.infinite(AIC(model1)) | is.infinite(AIC(model2))) {
    message("Your AIC is infinite. I can't be sure why, but it may be because you tried to fit continuous data with a poisson model? If so, use a Gamma and try again.\n")
    return(NULL)
  }
  
  aic = c(AIC(model1), AIC(model2))
  bic = c(BIC(model1), BIC(model2))
  bayes.factor = bf.bic(model1, model2)
  p = get_p_value(model1, model2, nested)
  rsq = get_r_squared(model1, model2, nested)
  adj_rsq = get_adj_r_squared(model1, model2, nested)
  ### make sure bayes factor is attached to the more likely model
  if ((bic[1]<=bic[2] & bayes.factor<=1) | bic[2]<=bic[1] & bayes.factor>=1){
    model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(1/bayes.factor, bayes.factor), p = c(p, NA), rsq = as.numeric(rsq[1:2]), adj.rsq = adj_rsq[1:2] )
  } else if ((bic[2]<bic[1] & bayes.factor<1) | (bic[1]<bic[2] & bayes.factor>1)){
    model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(bayes.factor, 1/bayes.factor), p = c(p, NA), rsq = as.numeric(rsq[1:2]), adj.rsq = adj_rsq[1:2]  )
  }  
  row.names(model.table) = c(m1.name, m2.name)
  
  # format the numbers nicely
  if (is.na(model.table$p[1])){
    model.table = round(model.table[,!is.na(model.table[1,])], digits=3)	
  } else {
    model.table = round(model.table[,!is.na(model.table[1,])], digits=3)
    model.table[is.na(model.table)] = ""
    model.table$p[1] = format.pval(as.numeric(model.table$p[1]), digits=3)
  }
  
  # get rid of columns with NA
  keep = colSums(apply(model.table, 2, is.na))!=2
  
  return(model.table[,keep])
}

check_AIC = function(model1, model2) {
  
}