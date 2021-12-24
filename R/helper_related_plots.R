create_related_plot = function(data, outcome, plot.type,
                               suppress_smooth=F, spread="quartile", jitter=.1){
  levs = attr(data, "levels")
  p = paste0("ggplot(data, aes(y=Difference, x=1)) + 
                theme_bw()+ geom_hline(yintercept=0, col='lightgray') + 
                labs(x='Difference (", levs[2], "-", levs[1], ")') + 
                theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())")
  points = points.func(axis.var="Difference", data=data, jitter=jitter[1]*.5)
  fitted = related_plot_string(data, outcome, plot.type, suppress_smooth)
  return(list(p=p, points=points, fitted=fitted))

}

related_plot_string = function(data, outcome, plot.type,
                               suppress_smooth=F, spread="quartile") {
  if (plot.type == "boxplot") return('geom_boxplot(alpha=.1)')
  if (plot.type == "violin")  return('geom_violin(alpha=.1)')
  fit_string = create_summary_beeswarm(suppress_smooth, spread, mean.line=F)
  fitted = paste0(
            modify_fit_string_for_ggplot2(fit_string), 
            " + coord_cartesian(xlim=c(.75, 1.25))")
  return(fitted)
}
  
modify_related_data = function(data, related, axis, outcome, variables) {
    
  if (!related) return(data)
  
  #### extract levels of the predictors
  levs = unique(data[,axis[1]])
  
  #### create difference scores
  g1 = data[data[, axis[1]]==levs[1], outcome]
  g2 = data[data[, axis[1]]==levs[2], outcome]		
  
  ### error checking
  if (length(variables)!=2) stop("Currently, the 'related' option is only available when there's a single predictor.")
  if (length(levs)!=2) stop("Sorry, I can only accept two levels of the grouping variable when related=T.")
  if (length(g1) != length(g2)) stop("Sorry, the length of the two groups are not the same. I can only create difference scores when the group sizes are identical.")
  
  lab = paste0("Difference (",levs[2], "-", levs[1], ')')
  data = data.frame(Difference=g2-g1)
  attr(data, "levels") = levs
  data[,variables] = NA
  return(data)
  }