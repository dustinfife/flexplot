jamovi_plots = function(formula, data, options=NULL) {
  
  # get the predictors/outcome
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  
  ### produce a histo⁄€‹€⁄gram early (so I don't run into errors when I'm asking for
  ### length of pred™ictors and such)
  
  if (length(outcome)==1 & length(predictors) == 0) {
    return(flexplot(formula, data))
  }
  
  # go through all the ways I've modified it
  linemethod = ifelse_null(options$line, options$line=="Time Series", "line", "histogram")
  line       = get_fitted_line(options$line)
  samp       = ifelse(is.null(options$sample), Inf, options$sample*.01*nrow(data))
  se.type    = ifelse_null(options$center,1==1, unlist(strsplit(options$center," + ", fixed=T))[2])
  alpha      = ifelse(is.null(options$alpha), .5, options$alpha*.01)
  suppr      = ifelse(is.null(options$suppr), F, options$suppr)
  jittx      = ifelse(is.null(options$jittx), 0, options$jittx)
  jitty      = ifelse(is.null(options$jitty), 0, options$jitty)
  se         = ifelse(is.null(options$se),    F, options$se)
  ghost      = ifelse(is.null(options$ghost), F, options$ghost)
  resid      = ifelse(is.null(options$resid), F, options$resid)
  bins       = ifelse(is.null(options$bins),  3, options$bins)
  
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  
  # set up conditional statements
  diff_selected      = ifelse(is.null(options$diff), FALSE, options$diff)
  only_one_predictor = length(predictors)==1 
  only_two_levels    = length(unique(data[,predictors[1]]))==2 
  groups_are_equal   = table(data[,predictors[1]])[1]==table(data[,predictors[1]])[2]
  related = ifelse(diff_selected & only_one_predictor & only_two_levels & groups_are_equal,
                   TRUE, FALSE)
  given_exists = !is.na(given)
  preds_exist  = length(options$preds)>0
  
  ### added variable plot
  if ((given_exists | length(predictors)>1) & resid==TRUE) {
    p = added.plot(formula, data=data, spread=se.type, 
                   method=line, alpha = alpha, 
                   sample = samp, jitter=c(options$jittx, options$jitty), 
                   bins=options$bins, suppress_smooth=suppr, related=related)
    return(p)
  }
  
  if (given_exists & ghost==TRUE){
    
    p = flexplot(formula, data=data, se=se,spread=se.type, 
                 method=line, alpha = alpha, ghost.line="black", 
                 sample = samp, jitter=c(options$jittx, options$jitty), 
                 bins=bins, suppress_smooth=suppr, related=related,
                 plot.type=linemethod)
    return(p)
  }  
  
  p = flexplot(formula, data=data, se=se, spread=se.type, 
               method=line,  alpha = alpha, sample = samp, 
               jitter=c(options$jittx, options$jitty),
               suppress_smooth=suppr, bins=options$bins, related=related,
               plot.type=linemethod)
  return(p)
  
}