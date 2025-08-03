generate_predictions = function(model, re, pred.values, pred.type, report.se) {
  UseMethod("generate_predictions")
}

generate_predictions = function(model, re, pred.values, pred.type, report.se) {
  UseMethod("generate_predictions")
}

generate_predictions.lmerMod = function(model, re, pred.values, pred.type, report.se) {
  if (!re) {
    return(data.frame(prediction = 
                        predict(model, pred.values, type="response", re.form=NA), 
                      model="fixed effects"))
  } else {
    random_effects = data.frame(prediction = predict(model, pred.values, type="response", re.form=NULL), 
                                model="random effects")
    fixed_effects = data.frame(prediction = predict(model, pred.values, type="response", re.form=NA), 
                               model="fixed effects")
    return(rbind(random_effects, fixed_effects))
  }
}

generate_predictions.glmerMod = generate_predictions.lmerMod  # Same logic

generate_predictions.polr = function(model, re, pred.values, pred.type, report.se) {
  return(data.frame(prediction = predict(model, pred.values, type="class"), 
                    model = "polr"))
}

generate_predictions.RandomForest = function(model, re, pred.values, pred.type, report.se) {
  response = attr(model, "data")@get("response")
  outcome = attr(model, "data")@get("input")
  data = cbind(response, outcome)
  # check if classes differ from old to new, and correct if they are
  class_preds = lapply(pred.values, class)
  class_data = lapply(data[names(pred.values)], class)
  if (!identical(class_preds, class_data)) {
    for (i in 1:length(class_preds)) {
      
      if ("factor" %in% (class(data[,names(pred.values[i])]))) {
        
        ordered = ifelse("ordered" %in% (class(data[,names(pred.values[i])])), T, F) 
        pred.values[,i] = factor(pred.values[,i], levels=levels(data[,names(pred.values[i])]), ordered=ordered)
      } else {
        class(pred.values[,i]) = class(data[,names(pred.values[i])])
      }  
    }
  }
  
  prediction = predict(model, newdata=pred.values, OOB = TRUE)
  d = data.frame(prediction = prediction, model=model.type)
  names(d)[1] = "prediction"
  return(d)  
}

generate_predictions.rpart = function(model, re, pred.values, pred.type, report.se) {
  return(data.frame(prediction = predict(model, pred.values), 
                    model = "rpart"))
}

generate_predictions.default = function(model, re, pred.values, pred.type, report.se) {
  int = ifelse(report.se, "confidence", "none")
  return(data.frame(prediction = predict(model, pred.values, interval=int, type=pred.type),
                    model = class(model)[1]))
}