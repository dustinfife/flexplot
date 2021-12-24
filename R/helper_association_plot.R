### prep data for association plot
modify_association_plot_data = function(data, outcome, axis, given) {
  m = as.data.frame(table(data[,axis], data[,given], data[,outcome])); names(m)[1:(ncol(m)-1)] = c(axis, given, outcome)
  chi = suppressWarnings(chisq.test(data[,axis], data[,outcome]))
  obs.exp = (chi$observed - chi$expected)/chi$expected
  m$Freq = as.vector(obs.exp)
  names(m)[names(m)=="Freq"] = "Proportion"
  return(m)
}

create_association_plot = function() {
  p = "ggplot(data=data, aes_string(x=axis, y='Proportion', fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()"
  points = "xxxx"
  fitted = "xxxx" 
  return(list(p=p, points=points, fitted=fitted))
}