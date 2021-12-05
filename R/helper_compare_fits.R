get_re = function(model) {
  if (!(class(model)[1] %in% c("lmerMod", "glmerMod"))) {
    return(NULL)
  }
  text_formula = paste0(formula(model))[3]
  re = gsub(".* | [.+]", "\\1", text_formula)
  re = gsub(")", "", re)
  return(re)
}

whats_model2 = function(model1,model2=NULL) {
  if (is.null(model2)){
    return(model1)
  } 
  return(model2)
}

# function that extracts variables from cforest model
get_cforest_variables = function(model, return.type=c("all", "predictors", "response")) {
  return.type = match.arg(return.type)
  
  ## get all variables 
  vars = attr(model, "data")@formula
  
  if (return.type == "all") {
    response = unlist(strsplit(as.character(vars$response)[2], " + ", fixed=T))
    input = unlist(strsplit(as.character(vars$input)[2], " + ", fixed=T))
    all_vars = c(response, input)
    return(all_vars)
  }
  
  if (return.type == "predictors") {
    input = unlist(strsplit(as.character(vars$input)[2], " + ", fixed=T))
    return(input)
  }
  
  response = unlist(strsplit(as.character(vars$response)[2], " + ", fixed=T))
  return(response)
}

get_terms = function(model) {
  model.type = class(model)[1]
  
  #### extract the terms from each MODEL
  if (model.type == "RandomForest") {
    predictors = get_cforest_variables(model, "predictors");
    response = get_cforest_variables(model, "response");
    return(list(predictors = predictors, response=response))
  } 
  
  form = formula(model) 
  predictors=all.vars(form)[-1]  
  response = all.vars(form)[1]
  return(list(predictors = predictors, response=response))
}

check_missing = function(model1, model2=NULL, data, variables) {
  ### if they haven't supplied model 2, no need to check
  if (is.null(model2)) return(data)
  
  n1 = get_model_n(model1)
  n2 = get_model_n(model2)
  
  if (n1<nrow(data) | n2<nrow(data)){
    data = na.omit(data[,variables])
  }
  
  return(data)
}

get_model_n = function(model) {
  mod_class = class(model)[1]
  if (mod_class == "RandomForest") return(attr(model, "responses")@nobs)
  if (mod_class == "randomForest.formula") return(length(model$predicted))
  if (mod_class == "lmerMod" | mod_class == "glmerMod") return(nobs(model))
  if (mod_class == "rpart") return(length(model$y))
  
  return(nrow(model$model))
  
}

get_variable_types = function(predictors, data) {
  # with related t, formula becomes difference~1, (no predictors). 
  # need to recognize that and return something different
  if (length(predictors)==0) return(list(cat=character(0), numb=character(0)))
  #### get variable types
  cat      = names(which(unlist(lapply(data[,predictors, drop=FALSE], function(x) (!is.numeric(x)) | length(unique(x))<21)) ))
  numb     = predictors[predictors %!in% cat]
  list(cat=cat, numb=numb)
}

### function to generate prediction matrix spanning the range of the data
generate_predictors = function(model, data = NULL, predictors=NULL, model_terms=NULL, num_points = 50, return.preds=F,...) {
  
  if (is.null(predictors))  predictors = all.vars(formula(model))[-1]
  if (is.null(model_terms)) model_terms = all.vars(formula(model))[-1]
  if (is.null(data))        data = extract_data_from_fitted_object(model)
  
  variable_types = get_variable_types(predictors, data)
  cat  = variable_types$cat
  numb = c(variable_types$numb)
  
  # get the ranges of numeric variables (or return NULL)
  min.max = create_ranges_numberic_variables(data, numb, num_points, return.preds, ...)
  
  #### get unique values for categorical vars
  un.vars = lapply(data[,cat, drop=FALSE], unique)    	
  
  #### combine into one dataset
  all.vars = c(min.max, un.vars)    
  all.vars = lapply(all.vars, function(x) x[!is.na(x)])
  pred.values = expand.grid(all.vars)
  
  
  # if all variables in model are in the formula, we're done
  if (all(model_terms %in% predictors)) return(pred.values)
  
  # otherwise, we need to set some values to the mean (or a random category level)
  not.in.there = model_terms[which(!(model_terms %in% predictors))]
  predicted_values = not.in.there %>% purrr::map(return_predicted_value_for_missing_variables, 
                                                 data=data, model=model)
  # map converts all numbers to characters, for some reason.
  # convert them back to numbers
  which_are_numeric = sapply(data[,not.in.there, drop=FALSE], is.numeric)
  predicted_values[which_are_numeric] = as.numeric(predicted_values[which_are_numeric])
  pred.values[,not.in.there] = predicted_values
  return(pred.values)
}

return_predicted_value_for_missing_variables = function(variable, data, model) {
  
  if (is.numeric(data[,variable])) {
    message(paste0("Note: You didn't choose to plot ", variable, " so I am inputting the median"))
    return(median(data[,variable], na.rm=T))
  }
  
  val = unique(as.character(data[[variable]]))[1]
  #only display the message if it's not a glmer mod
  if (!(class(model)[1] %in% c("lmerMod", "glmerMod"))) {
    message(paste0("Note: You didn't choose to plot ", 
                   variable, " so I am inputting '", val, "'"))
  }  
  return(val)
  
}

create_ranges_numberic_variables = function(data, numb, num_points, return.preds=F, ...) {
  
  if (length(numb)==0) return(NULL)
  
  # create matrix of min/max for each numeric variable
  var.mins = apply(data[, numb, drop = FALSE], 2, min, na.rm=T)
  var.max =  apply(data[, numb, drop = FALSE], 2, max, na.rm=T)  
  min.max = data.frame(var.mins, var.max) 
  
  # set quadriture points to size of bins
  if ("bins" %in% names(list(...))) bin_size = list(...)[['bins']] else bin_size = 10
  
  ##### make "quadriture" points for quant variables
  # if they're asking to return the predictions, don't limit quadriture points for the non-x-axis variables
  # otherwise, limit it to the number of bins
  min.max$size = numb %>% 
    purrr::map_dbl(function(x) {
      ifelse(return.preds, min(num_points, length(unique(data[,x]))), bin_size)
    })
  if (length(numb)>0) min.max$size[1] = min(length(unique(data[,numb[1]])), num_points)
  
  # now convert that list to a list of quadriture points
  f = function(x, d){seq(from=d[x,1], to=d[x,2], length.out=d[x,3])}
  min.max = 1:nrow(min.max) %>% 
    purrr::map(function(x) f(x, min.max))
  names(min.max) = numb
  return(min.max)
}

generate_predictions = function(model, re, pred.values, pred.type, report.se) {
  
  model.type = class(model)[1]
  if ((model.type == "lmerMod" | model.type == "glmerMod") & !re){
    return(data.frame(prediction = 
                        predict(model, pred.values, type="response"), model="fixed effects"))
  }  
  
  if ((model.type == "lmerMod" | model.type == "glmerMod") & re){
    return(data.frame(
      prediction = 
        predict(model, pred.values, type="response", re.form=NA), model="random effects"))
  }  
  
  if (model.type == "polr"){
    return(
      data.frame(prediction = predict(model, pred.values, type="class", re.form=NA), model= model.type)		
    )
  }
  
  if (model.type=="RandomForest") {
    
    ## get dataset to test that classes are all the same
    response = attr(model, "data")@get("response")
    outcome = attr(model, "data")@get("input")
    data = cbind(response, outcome)
    # check if classes differ from old to new, and correct if they are
    class_preds = lapply(pred.values, class)
    class_data = lapply(data[names(pred.values)], class)
    if (!identical(class_preds, class_data)) {
      for (i in 1:length(class_preds)) {
        if (class(data[,names(pred.values[i])])== "factor") 
          pred.values[,i] = factor(pred.values[,i], levels=levels(data[,names(pred.values[i])]))
        else 
          class(pred.values[,i]) = class(data[,names(pred.values[i])])
      }
    }
    
    prediction = predict(model, newdata=pred.values, OOB = TRUE)
    d = data.frame(prediction = prediction, model=model.type)
    names(d)[1] = "prediction"
    return(d
    )    
  }
  
  if (model.type == "rpart") {
    return(
      data.frame(prediction = predict(model, pred.values), model= model.type)		
    )
  }
  
  int = ifelse(report.se, "confidence", "none")
  return(data.frame(prediction = predict(model, pred.values, interval=int, type=pred.type), 
                    model=model.type)
        )
}


return_model_labels = function(model1, model2, model_column=NULL) {
  
  model1.type = class(model1)[1]
  model2.type = class(model2)[2]
  
  model1_name = deparse(substitute(model1, env = -1))
  model2_name = deparse(substitute(model2, env = -1))
  return(model1_name)
  
  
  # #### if they have the same name, just call them model1 and model2
  # if (!re){
  #   pred.mod1$model = paste0(deparse(substitute(model1)), " (", model1.type, ")", collapse="")
  #   if (pred.mod1$model[1] == pred.mod2$model[1]){
  #     pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, " 2)", collapse="")
  #   } else {
  #     pred.mod2$model = paste0(deparse(substitute(model2)), " (", model2.type, ")", collapse="")
  #   }
  # }
}








#