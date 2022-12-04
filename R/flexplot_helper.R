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
    return('geom_point(data=prediction, aes(y=prediction, color=model),   position=position_dodge(width=.2)) + 
             geom_line(data=prediction, aes(y=prediction, linetype=model, group=model, color=model), position=position_dodge(width=.2))')
  }
  
  # if they give an axis 2, draw a line for each level of axis 2
  if (!is.na(axis[2])) {
    return('geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), size=1)')
  }
  
  return('geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1) + scale_linetype_manual(values=c("solid", "dotdash"))')
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