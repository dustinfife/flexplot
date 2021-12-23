create_summary_beeswarm = function(suppress_smooth, spread, mean.line) {
  if (suppress_smooth){
    summary1="xxxx"
    summary2="xxxx"
    sum.line="xxxx"			
    return(paste0(summary1, "+",summary2, "+", sum.line))
  } 
  
  if (mean.line){
    sum.line = 'stat_summary(aes_string(group= axis[2]), geom="line", fun.y="mean", position=position_dodge(width=.5), color = "#bf0303")'
  } else {
    sum.line='xxxx'
  }
  
  if (spread=="stdev"){
    summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')" 
    summary2 = "stat_summary(geom='errorbar', 
                            fun.ymin = function(z){mean(z)-sd(z)}, 
                            fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.2, 
                            position=position_dodge(width=.5), color = '#bf0303')"
    return(paste0(summary1, "+",summary2, "+", sum.line))
  }
  
  if (spread=="sterr"){	
    summary1 = "stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')"
    summary2 = "stat_summary(geom='errorbar', 
                              fun.ymin = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, 
                              fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, 
                              position=position_dodge(width=.2), color = '#bf0303')"
    return(paste0(summary1, "+",summary2, "+", sum.line))
    
  } 
  
  if (spread == "quartiles"){	
    summary1 = "stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')" 
    summary2 = "stat_summary(geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')"
    return(paste0(summary1, "+",summary2, "+", sum.line))
  }
}

modify_fit_string_for_ggplot2 = function(fit.string) {
  if (packageVersion("ggplot2")>"3.2.1"){
    fit.string = gsub("fun.ymin", "fun.min", fit.string, fixed=T)
    fit.string = gsub("fun.ymax", "fun.max", fit.string, fixed=T)
    fit.string = gsub("fun.y", "fun", fit.string, fixed=T)
    return(fit.string)
  } 
  
  return(fit.string)
}

create_beeswarm_plot = function(data, axis, jitter=c(.2,0), suppress_smooth=F, 
                                spread="quartile", mean.line=F) {
  p = 'ggplot(data=data, aes_string(x=axis, y=outcome))'
  points = points.func(axis.var=axis, data=data, jitter=jitter)
  if (plot.type == "boxplot"){
    fitted = 'geom_boxplot(alpha=.1)'
  } else if (plot.type == "violin"){
    fitted = 'geom_violin(alpha=.1)'
  } else if (plot.type == "line") {
    fitted = 'geom_line()'
  } else {
    fitted = create_summary_beeswarm(suppress_smooth, spread, mean.line)		
  }
  
  fitted = modify_fit_string_for_ggplot2(fitted)
  return(p=p, points=points, fitted=fitted)
}
  
  ### check package version of ggplot2
  