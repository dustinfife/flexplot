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