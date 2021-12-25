flexplot_modify_prediction = function(prediction=NULL, formula=NULL,
                                      break.me = NULL, bins=3, labels=NULL, breaks=NULL, predictors=NULL, axis=NULL, given=NULL){
  
  if (is.null(prediction)) return(NULL)
  
  ## prep data (for testing)
  if (is.null(predictors)) { 
    predictors = all.vars(formula)[-1]
    given.axis = flexplot_axis_given(formula)
    given = given.axis$given
    axis = given.axis$axis
  }
  if (is.null(break.me)) {
    break.me = flexplot_break_me(prediction, predictors, given, axis, bins)
    breaks = flexplot_create_breaks(break.me = break.me, breaks, data, labels, bins=bins)  
    prediction = bin_variables(data=prediction, bins=bins, labels=labels, break.me=break.me, breaks=breaks)
  }
  
  axis_2_is_missing = is.na(axis[2])
  number_of_models = length(unique(prediction$model))
  if (!axis_2_is_missing & number_of_models>1){
    stop("Sorry. I can't plot the model(s) lines when there are already lines in the plot. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z), or choose to display only one model")
  }
  
  
  #### bin the predictions, where needed
  if (length(break.me)>0){
    for (i in 1:length(break.me)){
      ### find that variable in the model and bin it
      prediction[[break.me[i]]] = bin.me(break.me[i], prediction, bins, labels[i], breaks[[break.me[i]]])
    }
    
    ### now average fit within bin
    groups = c("model", paste0(break.me, "_binned"), predictors[-which(predictors%in%break.me)])
    prediction = prediction %>% group_by_at(groups) %>% summarize(prediction = mean(prediction)) %>% as.data.frame
  }
  
  return(prediction)
}

flexplot_generate_prediction_lines = function(data, plot, axis){
  
  if (is.null(data)) {
    plot$plot_string$pred.line = "xxxx"
    return(plot)
  }
  
  # if axis 1 is categorical, connect the means
  if (!is.numeric(data[[axis[1]]])) {
    plot$plot_string$pred.line = 'geom_point(data=prediction, aes(y=prediction, color=model), position=position_dodge(width=.2)) + 
            geom_line(data=prediction, aes(y=prediction, linetype=model, group=model, color=model), position=position_dodge(width=.2))'
    return(plot)
  }  
    
  ##### if they specify an axis[2], modify the "fitted" string
  if (!is.na(axis[2])){
    plot$plot_string$pred.line = 'geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), size=1)' 				
    plot$plot_string$fitted = "xxxx"
    return(plot)
  }  
    
  #### if they supply more than two models to compare...
  if (length(levels(data$model))>2){
    plot$plot_string$pred.line = 'geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1)' 									
  } else {
    plot$plot_string$pred.line = 'geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1) + scale_linetype_manual(values=c("solid", "dotdash"))' 				
  }
  return(plot) 
}

factorize_predictions = function(prediction, data, axis) {
  if (is.null(prediction)) return(prediction)
  
  prediction$model = factor(prediction$model)
  
  ### make the levels consistent between prediction/data for axis 1
  x_is_categorical = !is.numeric(data[[axis[1]]])
  if (x_is_categorical){
    prediction[[axis[1]]] = factor(prediction[[axis[1]]], levels=levels(data[[axis[1]]]))
  }
  return(prediction)
}

modify_ggplot_string_for_predictions = function(plot, axis) {
  # don't color the lines when there's predictions added
  if (length(axis)>1) {
    plot$p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2], shape=axis[2])) + labs(color= axis[2], shape= axis[2])'
  }
  return(plot)
}