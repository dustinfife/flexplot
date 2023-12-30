#### this function converts a binary variable to a 1/0 for logistic regression
factor.to.logistic = function(data, outcome, method=NULL, labels=F){
  
  levels_dv = length(unique(data[,outcome]))
  # return if it's not logistic
  if (levels_dv != 2) return(data)
  if (labels) return(unique(data[,outcome]))
  if (is.numeric(data[,outcome])) return(data)
  if (method != "logistic") return(data)
  # at this point it's categorical, has two levels, but doesn't necessarily have "logistic" as a method  
  
  # if it's an ordered factor, make the second level the referent level
  if (class(data[,outcome])[1] == "ordered") {
    data[,outcome] = factor(data[,outcome], levels=levels(data[,outcome]), labels=c(0,1)) %>% 
      as.character() %>%
      as.numeric() 
    return(data)
  }
  
  # the rest are just regular factors
  data[,outcome] = factor(data[,outcome], levels=unique(data[,outcome]), labels=c(0,1)) %>%
    as.character() %>%
    as.numeric() 
  return(data)
  
}

return_labels_for_logistic_regression = function(data, outcome, method) {
  if (method != "logistic")                  return(NULL)
  if (length(unique(data[,outcome]))!=2)     return(NULL)
  if (class(data[,outcome])[1] == "ordered") return(levels(data[,outcome]))
  if (is.numeric(data[,outcome]))            return(sort(unique(data[,outcome])))
  return(sort(unique(data[,outcome]), decreasing = F))
}  

flexplot_alpha_default = function(data, axis, alpha){
  if (axis[1] == "1") return(alpha)
  if (!is.numeric(data[,axis[1]]) & alpha == .99977) return(.2)
  if ( is.numeric(data[,axis[1]]) & alpha == .99977) return(.5)
  return(alpha)
}

flexplot_generate_prediction_lines = function(prediction, axis, data){
  
  # if axis 1 is categorical, connect the means with lines
  if (!is.numeric(data[[axis[1]]])) {
    return('geom_point(data=prediction, aes(y=prediction, color=model),   position=position_dodge(width=.4)) + 
             geom_line(data=prediction, aes(y=prediction, linetype=model, group=model, color=model), position=position_dodge(width=.4))')
  }
  
  # if they give an axis 2, draw a line for each level of axis 2
  if (!is.na(axis[2])) {
    return('geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), linewidth=1)')
  }
  
  return('geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), linewidth=1) + scale_linetype_manual(values=c("solid", "dotdash"))')
}

#### flexplot function for paneling
flexplot_panel_variables = function(given, break.me){
  
  if (is.na(given[1])) return("xxxx")
  
  #### prep the given variables to be stringed together
  given2 = given
  if (length(break.me)>0){
    given2[given2%in%break.me] = paste0(given2[given2%in%break.me], "_binned")
  }	
  
  if (given[1]=="") {
    given.as.string = paste0(given2[2], "~.")
  } else {
    given.as.string = ifelse(length(given)>1 & !is.na(given2[1]),
                             paste0(rev(given2), collapse="~"), 
                             paste0("~",given2))
  }
  
  facets = paste0('facet_grid(as.formula(', given.as.string, '),labeller = custom.labeler)')			
  return(facets)
  
}

make_levels_same_for_prediction_dataset = function(data, prediction, axis) {
  axis_1_is_categorical = !is.numeric(data[[axis[1]]])
  axis_1_not_1          = axis[1] != "1"
  if (axis_1_is_categorical & axis_1_not_1){
    prediction[[axis[1]]] = factor(prediction[[axis[1]]], levels=levels(data[[axis[1]]]))
  }
  return(prediction)
}

check_se = function(se=NULL, axis) {
  if (!is.null(se)) return (se)
  if (length(axis)>1) return(F)
  return(T)
}

flexplot_histogram = function(data, outcome, plot.type="histogram", bins=3) {
  
  ### figure out how many levels for the variable
  levels = length(unique(data[,outcome]))	
  
  # if categorical, do a barchart
  if (!is.numeric(data[,outcome])) {
    return('ggplot(data=data, aes(!!sym(outcome))) + geom_bar() + theme_bw() + labs(x= outcome)')
  }
  
  #### if numeric, do a histogram
  if (plot.type=="qq"){
    return('ggplot(data=data, aes(sample = !!sym(outcome))) + stat_qq() + stat_qq_line() + theme_bw() + labs(x=outcome)')
  } 
  
  if (plot.type == "density") {
    return('ggplot(data=data, aes(!!sym(outcome))) + geom_density() + theme_bw() + labs(x=outcome)')
  } 
  
  
  bins = calculate_bins_for_histograms(bins, levels)
  return(
    paste0('ggplot(data=data, aes(!!sym(outcome)))  + geom_histogram(fill="lightgray", col="black", bins=', bins, ') + theme_bw() + labs(x=outcome)')
  )

}

flexplot_related = function(data, jitter = .1, plot.type = "errorbar", spread="quartiles", suppress_smooth=F) {
  
  levs = attr(data, "levels")
  p = paste0("ggplot(data, aes(y=Difference, x=1)) + theme_bw()+ geom_hline(yintercept=0, col='lightgray') + labs(x='Difference (", 
             levs[2], "-", levs[1], ")') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())")
  
  points = points.func(axis.var="Difference", data=data, jitter=jitter*.5)
  
  if (plot.type == "boxplot"){
    fitted = 'geom_boxplot(alpha=.1)'
  } else if (plot.type == "violin"){
    fitted = 'geom_violin(alpha=.1)'
  } else {
    fitted = paste0(
              fit.function(outcome, "Difference", data=data, suppress_smooth=suppress_smooth, 
                           method="lm", spread=spread, categorical=T), 
              " + coord_cartesian(xlim=c(.75, 1.25))")
  }
  
  return(list(p=p, points=points, fitted=fitted))
}

flexplot_bivariate_string = function(data, outcome, axis, 
                                     jitter=.1, plot.type = "x",
                                     suppress_smooth = F, spread = "quartiles", method="lm") {
  
  # association plot
  if (!is.numeric(data[[outcome]]) & !is.numeric(data[[axis]])) {
    p = "ggplot(data=data, aes(x=!!sym(axis), y=!!sym('Frequency'), fill=!!sym(outcome))) + geom_bar(stat='identity', position='dodge') + theme_bw()"
    points = "xxxx"
    fitted = "xxxx"
    return(list(p=p, points=points, fitted=fitted))
  }
  
  # bivariate plot (the points.func function will determine whether it's numeric or categorical x axis)
  p = 'ggplot(data=data, aes(x=!!sym(axis), y=!!sym(outcome)))'
  points = points.func(axis.var=axis, data=data, jitter=jitter)
  if (plot.type == "boxplot"){
    fitted = 'geom_boxplot(alpha=.1)'
  } else if (plot.type == "violin"){
    fitted = 'geom_violin(alpha=.1)'
  } else if (plot.type == "line") {
    fitted = 'geom_line()'
  } else {
    fitted = fit.function(outcome, axis, data=data, suppress_smooth=suppress_smooth, method=method, spread=spread)		
  }
  
  return(list(p=p, points=points, fitted=fitted))
}

flexplot_multivariate_aes = function(data, outcome, prediction=NULL, axis) {
  ### if they supply predictions, do not vary color
  if (!is.null(prediction)){
    return('ggplot(data=data, aes(x=!! sym(predictors[1]), y=!! sym(outcome), color=!! sym(axis[2]), shape=!! sym(axis[2]))) + labs(color= axis[2], shape= axis[2])')
  } 
  
  
  if (is.numeric(data[,axis[2]])){
    axis[2] = paste0(axis[2], "_binned"); axis2_binned = axis[2]
    p = paste0('ggplot(data=data, aes(x=', axis[1], ', ', y=outcome, 
               ', color=', axis2_binned, ', linetype = ', axis2_binned, 
               ', shape=', axis2_binned, ')) + labs(color= "', axis2_binned, '", linetype= "', axis2_binned, '", shape= "', axis2_binned, '")')
    return(p)
  } 
  
    # if they're trying to plot more than 10 symbols...
  if (length(unique(data[,axis[2]]))>6) {
    message("It looks like you're trying to plot more than 6 colors/lines/symbols.\nI gotta give it to you...you're ambitious. Alas, I can't do that, so I'm removing the colors/lines/symbols.\n I hope we can still be friends.")
    return('ggplot(data=data, aes(x=!!sym(predictors[1]), y=!!sym(outcome), color=!!sym(axis[2])))')
  }
  
  return('ggplot(data=data, aes(x=!!sym(predictors[1]), y=!!sym(outcome), color=!!sym(axis[2]), linetype = !!sym(axis[2]), shape=!!sym(axis[2]))) + labs(color= axis[2], linetype= axis[2], shape= axis[2])')
  
}


make_prediction_dataset_same_type_on_x1 = function(data, prediction, axis1) {
  if (is.null(prediction)) return(NULL)
  if (is.numeric(data[,axis1])  & ! is.numeric(prediction[,axis1])) prediction[,axis1] = as.numeric(as.character(prediction[,axis1]))
  if (!is.numeric(data[,axis1]) &   is.numeric(prediction[,axis1])) prediction[,axis1] = (as.character(prediction[,axis1]))
  return(prediction)
}




#