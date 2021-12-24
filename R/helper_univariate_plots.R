modify_univariate_data_numeric = function(data, axis, outcome) {
  
  # write conditions
  outcome_is_numeric = is.numeric(data[[outcome]])
  levels_outcome = length(unique(data[[outcome]]))
  outcome_is_ordered = is.ordered(data[[outcome]])
  
  if (outcome_is_numeric & levels_outcome<5){
    data[,outcome] = factor(data[,outcome], ordered=TRUE)
    return(data)
  }
  
  # order outcome by sample size
  if (!outcome_is_numeric & !outcome_is_ordered) {
    order = names(sort(table(data[[outcome]]), decreasing = T))
    data[[outcome]] = factor(data[[outcome]], levels = order, ordered=T)
    data[[outcome]]
    return(data)
  }
  return(data)
}

univariate_string = function(data, outcome, plot.type) {
  
  if (!is.numeric(data[,outcome]))
    return('ggplot(data=data, aes_string(outcome)) + geom_bar() + theme_bw() + labs(x= outcome)')
  
  if (plot.type=="qq"){
    return('ggplot(data=data, aes_string(sample = outcome)) + 
      stat_qq() + stat_qq_line() + theme_bw() + labs(x=outcome)')
  } 
  
  if (plot.type == "density") {
    return('ggplot(data=data, aes_string(outcome)) + 
      geom_density() + theme_bw() + labs(x=outcome)')
  } 
  
  return('ggplot(data=data, aes_string(outcome)) + 
    geom_histogram(fill="lightgray", col="black", bins=min(30, round(levels/2))) + 
    theme_bw() + labs(x=outcome)')
}

create_univariate_plot = function(data, outcome, plot.type="histogram") {
  
  #### if numeric, do a histogram
  p = univariate_string(data, outcome, plot.type)
  points = "xxxx"
  fitted = "xxxx"		
  return(list(p=p, points=points, fitted=fitted))
  
}