
# set.seed(1212)
# objects = mixed_model_plot(MathAch~SES + School,
#                  lme4::lmer(MathAch~SES + (SES | School), math),
#                  T, sample=3, return_objects = T)
# add_geoms_to_mixed_plot(objects$prediction, objects$step3, objects$object)
# objects = mixed_model_plot(MathAch~Sex + School,
#                  lme4::lmer(MathAch~Sex + (Sex | School), math),
#                  T, sample=3, return_objects = T)
# add_geoms_to_mixed_plot(objects$prediction, objects$step3, objects$object)
add_geoms_to_mixed_plot = function(prediction, step3, object, ...) {

  # get necessary objects
  d = object@frame
  term.re = extract_random_term(object)
  preds = names(d)[-1]
  outcome = names(d)[1]
  terms = all.vars(formula(step3))[-1]
  terms.fixed = terms[-which(terms %in% term.re)]  

  # output two datasets (one for fixed effects, one for random effects)
  m = prediction[prediction$model=="fixed effects",]
  newd = prediction[prediction$model=="random effects",]; names(newd)[names(newd)=="prediction"] = outcome
  
    # bin the numeric variables if they were binned in the flexplot
  
  m =    add_bin_to_new_dataset(step3, m,    terms, term.re, "prediction")
  newd = add_bin_to_new_dataset(step3, newd, terms, term.re, outcome)

  #if axis 1 is numeric, do lines
  if (is.numeric(d[,terms[1]])){

    # convert to ordinal when there's <5 unique values
    m    = convert_numeric_to_ordinal(m,    terms[1])
    newd = convert_numeric_to_ordinal(newd, terms[1])
    
    # convert to ordered factor if original was an ordered factor
    if (class(step3$data[,terms[1]])[1]=="ordered" &
        class(newd[[terms[1]]])[1]     !="ordered") {
      newd[[terms[1]]] = factor(newd[[terms[1]]],levels=c(unique(newd[[terms[1]]])),
                                                                             ordered=T)
      m[[terms[1]]] = factor(m[[terms[1]]],levels=c(unique(m[[terms[1]]])),
                                ordered=T)
    }
    
    # step3 + geom_line(data=newd, 
    #                   aes_string(terms[1], outcome, group=term.re, color=term.re), alpha = .5)
    # generate the geoms
    random_geom = geom_line(data=newd, 
                aes_string(terms[1], outcome, group=term.re, color=term.re), alpha = .5)
    
    # if there are no random effects, we shouldn't do these. 
    if (nrow(m)>0) {
      fixed_geom  = geom_line(data=m, 
                aes_string(terms[1], "prediction", color=NA, group=1), linetype=1, lwd=2, col="black") 
    } else {
      fixed_geom = NULL
    }
    return(list(random_geom = random_geom, fixed_geom = fixed_geom))
  }  
  
  #### aggregate the means across variables		

  means = prediction %>% group_by_at(vars(one_of(c(terms, "model")))) %>% summarize(Value = mean(prediction))
  means = add_bin_to_new_dataset(step3, means, c(terms, "model"), term.re, "Value")
  
  # change name of terms to "_binned" if there is one
  binned_vars = grep("_binned", names(step3$data), value=T)
  if (length(binned_vars)>0) {
    unbinned_name = gsub("_binned", "", binned_vars)
    terms[terms==unbinned_name] = paste0(unbinned_name, "_binned")
  }
  fixed.means = means[means$model=="fixed effects",]
  fixed.means = fixed.means %>% dplyr::group_by_at(vars(all_of(c(terms)))) %>% 
    summarize(Value=mean(Value))

  # figure out which ones are binned
  binned.var = grep("_binned",names(step3$data), value=T) 
  unbinned.var = binned = gsub("_binned", "", binned.var)
  
  means = means[means$model=="random effects",]
  names(means)[ncol(means)] = names(fixed.means)[ncol(fixed.means)] = outcome
  names(fixed.means)[names(fixed.means)%in%unbinned.var] = binned.var
  names(means)[names(means)%in%unbinned.var] = binned.var	
  
  random_geom = 
      ### random effects
      list(
        geom_point(data=means, aes_string(x=terms[1], y=outcome), size=.5),
        geom_line(data=means, aes_string(x=terms[1], y=outcome, group=term.re), lwd=.25, linetype=2, alpha = .90))
      
  fixed_geom =  
      ### fixed effects
      list(
        geom_point(data=fixed.means, aes_string(x=terms[1], y=outcome), size=3, color="black", shape=16),
        geom_line(data=fixed.means, aes_string(x=terms[1], y=outcome, group=term.re), lwd=2, color="black", linetype=1)) 
    
  return(list(random_geom = random_geom, fixed_geom = fixed_geom))
}


# set.seed(1212)
# mixed_model_plot(MathAch~SES + School,
#                  lme4::lmer(MathAch~SES + (SES | School), math),
#                  T, sample=3) + coord_cartesian(ylim=c(0, 25), xlim=c(-2, 2))
mixed_model_plot = function(formula, object, random_plot, sample=3, return_objects = F,...){
  
  data = object@frame

  # if they're plotting without random effects...
  if (!random_plot) return(compare.fits(formula, data=data, model1=object, re=F, clusters=sample,...))

  #### otherwise...
  prediction = compare.fits(formula, data=data, model1=object, re=T, return.preds=T, clusters=sample) %>% 
    na.exclude	

  #### subset data so it's the same as that sampled in prediction
  re = extract_random_term(object)
  selected_REs = prediction[,re]
  data_sampled = data[data[,re] %in% selected_REs,]
  data_sampled[,re] = factor(data_sampled[,re])
  
  # return the flexplot as the base
    step3 = flexplot(formula, data=data_sampled, suppress_smooth=T, ...)
  step3 = step3 +
    add_geoms_to_mixed_plot(prediction, step3, object)
  
    #### remove legend if n>10
  if (sample>10){
    step3 = step3 + theme(legend.position="none")
  }	
  if (return_objects) return(list(step3=step3, prediction = prediction, object=object))
  return(step3)
    
}



#' Visualize a fitted lmerMod model 
#'
#' Visualize a fitted lmerMod model
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Model plot? All of them?
#' @param ... Other arguments passed to flexplot
#' @param formula A flexplot-style formula
#' @param sample The number of individuals' datapoints to sample as raw data. Defaults to 3
#' @param plots.as.list Should the plots be returned as a list? Defaults to FALSE. 
#' @rawNamespace import(dplyr, except = c(filter, lag))
#' @return a plot containing a visual of the chosen model
#' @export
visualize.lmerMod = function(object, plot=c("all", "residuals", "model"), formula=NULL, 
                             sample = 3, plots.as.list=FALSE,...){
  
  d = object@frame
  plot = match.arg(plot, c("all", "residuals", "model"))
  
  #### generate residuals plots
  if (plot != "model") 	res.plots = residual.plots(data=d, object)
  
  #### return residuals if that's all they're asking for
  if (plot=="residuals"){
    p = arrange.plot(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl, step3=NULL,plot=plot, terms=res.plots$terms, numbers=res.plots$numbers)
    if (plots.as.list) return(list(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl))
    return(p)
  } 
  
  #### get the objects of interest
  term.re = extract_random_term(object)
  preds = remove_nonlinear_terms(names(d)[-1])
  outcome = names(d)[1]
  
  # convert re to factor (otherwise flexplot will try to bin it)
  d[,term.re] = factor(d[,term.re], ordered=T)
  
  # get a new dataset that samples the clusters
  k = d#randomly_sample_clusters(d, term.re, sample)

  # get the formula
  formula = make_formula_mixed(preds, term.re, outcome, formula)

  
  terms = all.vars(formula)[-1]
  terms.fixed = terms[-which(terms %in% term.re)]
  
  # figure out whether the RE is in the formula
  random_plot = are_re_plotted(formula, term.re)
  
  ##### generate fixed effects predictions
  #### if random is in NOT in the second slot
  
  step3 = mixed_model_plot(formula,
                   object,
                   random_plot, sample=sample,...)
  
  
  if (plot=="model") return(step3)
  p = arrange.plot(res.plots$histo, res.plots$res.dep, res.plots$sl, step3, plot, res.plots$terms, res.plots$numbers)
  return(p)
  
}

