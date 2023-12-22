### ggplot seems to be converting character to factors, then converting factors to numbers.
### so a 0/1 becomes a 1/2
# identify those variables that are ordered factors in ggplot, but numeric IRL
# then convert
convert_ordered_factors = function(plot_data, data) {
  ggplot_classes = lapply(plot_data, is.ordered) %>% unlist
  data_classes   = lapply(data[,names(plot_data)], is.ordered) %>% unlist
  ordered_mismatch = ggplot_classes & !data_classes
  if (!any(ordered_mismatch)) return(data)
  
  variables_to_convert = names(which(ordered_mismatch))
  data[[variables_to_convert]] = factor(data[[variables_to_convert]], ordered=T) %>% as.numeric
  return(data)
}

summary_function_depending_on_class = function(x) {
  if (is.numeric(x)) return(mean(x))
  return(x[1])
}

