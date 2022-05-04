#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a fitted object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @import ggplot2
#' @export
visualize = function(object, plot=c("all", "residuals", "model"),formula=NULL,...){
  UseMethod("visualize")
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a model object
#' @param plot what should be plotted? Residuals? model plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @export
visualize.default = function(object, plot=c("all", "residuals", "model"),formula=NULL,...){
  
  ## get dataset name
  data = eval(getCall(object)$data)
  
  ## get formula
  variables = all.vars(formula(object))
  predictors = variables[-1]
  response = variables[1]
  new_form = make_flexplot_formula(predictors, response, data)
  
  ## call compare.fits
  compare.fits(new_form, data=data, model1=object)
  
}


#' Visualize a randomForest model 
#'
#' Visualize a randomForest model
#' @param object a randomForest object
#' @param plot what should be plotted? Residuals? model plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @export
visualize.randomForest = function(object, plot=c("all", "residuals", "model"),formula=NULL,...){
  
  ## get dataset name
  data = eval(getCall(object)$data)
  
  ## get formula
  variables = all.vars(formula(object))
  predictors = variables[-1]
  response = variables[1]
  new_form = make_flexplot_formula(predictors, response, data)
  
  ## call compare.fits
  compare.fits(new_form, data=data, model1=object,...)
  
}



#' Visualize a fitted RandomForest model 
#'
#' Visualize a RandomForest model
#' @param object a RandomForest object
#' @param plot what should be plotted? Residuals? model plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @export
visualize.RandomForest = function(object, plot=c("all", "residuals", "model"),formula=NULL,...) {
  all_terms = get_terms(object)
  response = attr(object, "data")@get("response")
  outcome = attr(object, "data")@get("input")
  data = cbind(response, outcome)
  if (is.null(formula)) formula = make_flexplot_formula(all_terms$predictors, all_terms$response, data)
  compare.fits(formula, data=data, model1=object,...)
}





#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a lm object
#' @param plot what should be plotted? Residuals? Model plot? All of them?
#' @param formula A flexplot-style formula
#' @param plots.as.list Should the plots be returned as a list? Defaults to FALSE. 
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @export
visualize.lm = function(object, plot=c("all", "residuals", "model"), formula = NULL, plots.as.list=FALSE,...){
  
  plot = match.arg(plot, c("all", "residuals", "model"))
  
  
  d = object$model
  data = object$model
  variables = all.vars(formula(object))
  outcome = variables[1]
  predictors = variables[-1]
  
  ## see if all predictors are categorical
  dv_is_factor = check.non.number(data[,outcome])
  all_ivs_factors = all(variable_types(predictors, data)$characters)
  if (dv_is_factor & all_ivs_factors) {
    stop("Well, darn. You've found a limitation of flexplot. Flexplot cannot use visualize when
         all your variables are categorical. Sorry!")
  }
  
  #### use flexplot to visualize a model
  if ((plot=="all" | plot == "model" ) & is.null(formula)){
    
    #### generate formula as best we can
    #### get dataset
    
    #### now decide where things go
    if (length(predictors)>4){
      message("Note: to visualize more than four variables, I'm going to do an 'added variable plot.'")
      
      f = object$call[[2]]
      step3 = added.plot(f, data=d, ...) + labs(title="Analysis Plot")
      class(step3) <- c("flexplot", class(step3))
      return(step3)
    } else {
      
      
      f = make_flexplot_formula(predictors, outcome, data)
      #step3 = flexplot(f, data=data, ...)+ labs(title="Analysis Plot")
      step3 = compare.fits(f, data=data, model1=object, ...) + labs(title="Analysis Plot")
      
      #class(step3) <- c("flexplot", class(step3))
      #return(step3)			
      ### if they have more than two variables, also include a added variable plot
      if (length(terms)>1){
        step3b = added.plot(f, data=d,...)+ labs(title="Added Variable Plot")
        step3 = cowplot::plot_grid(step3, step3b, rel_widths=c(.6, .4))
        #class(step3) <- c("flexplot", class(step3))
        #return(step3)				
      }
    }
    
  } else if (plot=="all" | plot=="model"){
    step3 = compare.fits(formula, data=data, model1=object, ...) + labs(title="Analysis Plot")
    ### if they have more than two variables, also include a added variable plot
    if (length(terms)>1){
      step3b = added.plot(f, data=d,...)+ labs(title="Added Variable Plot")
      step3 = cowplot::plot_grid(step3, step3b, rel_widths=c(.6, .4))			
    }		
    
  }
  
  if (plot=="residuals"){
    res.plots = residual.plots(d, object,...)
    p = arrange.plot(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl, step3=NULL,plot=plot, terms=res.plots$terms, numbers=res.plots$numbers)
    if (plots.as.list){
      list(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl)
    } else {
      return(p)
    }
  } else if (plot=="model"){
    return(step3)
  } else {
    res.plots = residual.plots(d, object)
    p = arrange.plot(res.plots$histo, res.plots$res.dep, res.plots$sl, step3, plot, res.plots$terms, res.plots$numbers)
    return(p)
  }
}


utils::globalVariables(c("model", "Value", "y", "dataset", "switch_orientation"))





#' Visualize a fitted glmerMod model 
#'
#' Visualize a fitted glmerMod model
#' @param object a glmer object
#' @param plot what should be plotted? Residuals? Model plot? All of them?
#' @param ... Other arguments passed to flexplot
#' @param formula A flexplot-style formula
#' @param sample The number of individuals' datapoints to sample as raw data. Defaults to 3
#' @param plots.as.list Should the plots be returned as a list? Defaults to FALSE. 
#' @rawNamespace import(dplyr, except = c(filter, lag))
#' @return a plot containing a visual of the chosen model
#' @export
visualize.glmerMod = function(object, plot=c("all", "residuals", "model"), formula=NULL, 
                              sample = 3, plots.as.list=FALSE,...){
  
  #### figure out what is numeric
  d = object@frame
  plot = match.arg(plot, c("all", "residuals", "model"))
  
  #### generate residuals plots
  res.plots = residual.plots(data=d, object)
  
  #### now generate a model plot
  levels = apply(d, 2, FUN=function(x) length(unique(x)))
  outcome = names(d)[1]
  
  #### extract formula
  form = as.character(formula(object))[3]
  
  #### identify random effects
  term.re = trimws(substr(form, regexpr("\\|", form)[1]+1, regexpr("\\)", form)[1]-1))		
  
  #### find remaining terms
  preds = names(d)[-1]#[which(!(names(d)[-1] %in% term.re))]
  
  #### randomly sample the re terms and convert to numeric
  
  samp = sample(unique(d[, term.re]), size=sample)
  k = d[d[,term.re]%in%samp,]; k[,term.re] = as.factor(k[,term.re])
  
  ### come up with formula
  if (is.null(formula)){
    slots = c(1,3,4)
    form.slots = rep(NA, times=4)
    for (i in 1:min(4,length(preds))){
      if (preds[i]!=term.re){
        form.slots[slots[i]] = preds[i]
      }
    }
    
    ### for random effects models, just put school in first slot
    if (length(preds)>1) {
      form.slots[2] = term.re
    } else {
      form.slots[1] = term.re
    }
    symbol.slots = c("~","+", "|", "+")
    formula = paste0(symbol.slots, form.slots, collapse="")
    formula = gsub("\\|NA", "", formula);formula = gsub("\\+NA", "", formula);
    formula = paste0(outcome, formula, collapse="")
    
    formula = formula(formula)
  } 
  
  ### figure out where random component is
  f.char = as.character(formula)[3]
  criteria = paste0("\\+.*", term.re)
  
  ### if random component is in slot 2, modify the formula
  if (length(grep(criteria, f.char))>0){
    modify=T
    
    ### if there's a | in the formula, put it back
    crit2 = paste0("\\+.*", term.re,".*\\|")
    if (length(grep(crit2, f.char))>0){
      termses = gsub(crit2, "|", f.char)  
    } else {
      termses = gsub(criteria, "", f.char)
    }
    
    formula.new = make.formula(outcome, termses)			
  } else {
    modify = F
  }
  
  
  terms = all.vars(formula)[-1]
  terms.fixed = terms[-which(terms %in% term.re)]
  
  
  ##### generate fixed effects predictions
  #### if random is in NOT in the second slot
  if (!modify){
    step3 = compare.fits(formula, data=k, model1=object, model2=object, re=T, ...)
  } else {
    #### otherwise...
    prediction = compare.fits(formula, data=k, model1=object, re=T, return.preds=T)	
    
    ### to prevent conflicts with base::filter
    newd = prediction[prediction$model=="random effects",]; names(newd)[names(newd)=="prediction"] = outcome
    #newd = prediction %>% dplyr::filter(model=="random effects") %>% dplyr::mutate(MathAch = prediction)			
    #formula_new = MathAch~SES + School | Sex
    step3 = flexplot(formula, data=k, suppress_smooth=T, ...) 
    
    
    #if axis 1 is numeric, do lines
    if (is.numeric(d[,terms[1]])){
      m = prediction[prediction$model=="fixed effects",]
      
      ### flexplot turns <5 unique numeric values to ordinal variable
      ### we need to do the same here
      if (is.numeric(m[,terms[1]]) & length(unique(m[,terms[1]]))<5){
        m[,terms[1]] = factor(m[,terms[1]], ordered=TRUE)
        newd[,terms[1]] = factor(newd[,terms[1]], ordered=TRUE)
      }	
      
      step3 = step3+ 
        geom_line(data=m, 
                  aes_string(terms[1], "prediction", color=NA, group=1), linetype=1, lwd=2, col="black") +
        geom_line(data=newd, 
                  aes_string(terms[1], outcome, group=term.re, color=term.re))
      
      
      #if axis 1 is categorical, plot means as dots
    } else {
      
      #### see if any variables are binned, then bin the same variables
      ggdata = step3$data
      binned.var = names(ggdata)[grep("_binned",names(step3$data))] 
      unbinned.var = binned = gsub("_binned", "", binned.var)
      
      if (length(binned)>0){
        ### use ggplots data to find num bins and levels
        bin.levels = levels(step3$data[,binned.var])
        labs = levels(ggdata[,binned.var])			
        bins = length(labs)	
        newd[,binned] = bin.me(variable=binned, data=newd, bins=bins, labels=labs, breaks=NULL, check.breaks=F)				
        prediction[,binned] = bin.me(variable=binned, data= prediction, bins=bins, labels=labs, breaks=NULL, check.breaks=F)				
      }			
      
      #### aggregate the means across variables		
      means = prediction %>% group_by_at(vars(one_of(c(terms, "model")))) %>% summarize(Value = mean(prediction))
      fixed.means = means[means$model=="fixed effects",]
      fixed.means = fixed.means %>% dplyr::group_by_at(vars(one_of(c(terms.fixed)))) %>% 
        summarize(Value=mean(Value))
      
      means = means[means$model=="random effects",]
      #means = means %>% dplyr::filter(model=="random effects") 
      names(means)[ncol(means)] = names(fixed.means)[ncol(fixed.means)] = outcome
      names(fixed.means)[names(fixed.means)==unbinned.var] = binned.var
      names(means)[names(means)==unbinned.var] = binned.var			
      #if (term.re %in% names(fixed.means)) 
      fixed.means[,term.re] = NA
      fixed.means[,term.re] = factor(fixed.means[,term.re], levels=levels(means[,term.re]))
      #head(fixed.means)
      #### plot it
      step3 = step3 + 
        ### fixed effects
        geom_point(data=fixed.means, aes_string(x=terms[1], y=outcome), size=3, color="black", shape=16) +
        geom_line(data=fixed.means, aes_string(x=terms[1], y=outcome, group=1), lwd=2, color="black", linetype=1) +
        
        ### random effects
        geom_point(data=means, aes_string(x=terms[1], y=outcome), size=.5) +
        geom_line(data=means, aes_string(x=terms[1], y=outcome, group=term.re), lwd=.5, linetype=2) 			
      
    }	
    
    #### remove legend if n>10
    if (sample>10){
      step3 = step3 + theme(legend.position="none")
    }	
    
  }
  
  
  #### now put them all together
  if (plot=="residuals"){
    p = arrange.plot(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl, step3=NULL,plot=plot, terms=res.plots$terms, numbers=res.plots$numbers)
    if (plots.as.list){
      list(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl)
    } else {
      return(p)
    }
  } else if (plot=="model"){
    return(step3)
  } else {
    p = arrange.plot(res.plots$histo, res.plots$res.dep, res.plots$sl, step3, plot, res.plots$terms, res.plots$numbers)
    return(p)
  }	
  
}

arrange.plot  = function(histo, res.dep, sl, step3, plot, terms, numbers){
  
  #### return the plots
  if (plot=="model"){
    plot = step3
  } else if (plot=="residuals"){
    if (length(numbers)>0){
      top.row =suppressMessages(cowplot::plot_grid(histo, res.dep,ncol=2))
      bottom.row =suppressMessages(cowplot::plot_grid(NULL, sl, NULL, ncol=3, rel_widths=c(.25, .5, .25)))
      plot = suppressMessages(cowplot::plot_grid(top.row, bottom.row, nrow=2))			
    } else {
      plot = suppressMessages(cowplot::plot_grid(histo, sl, ncol=1))
      class(plot) <- c("flexplot", class(plot))			
    }
  } else {
    if (length(terms)==1){
      if (length(numbers)>0){
        top.row = suppressMessages(cowplot::plot_grid(step3, histo))
        bottom.row = suppressMessages(cowplot::plot_grid(res.dep, sl, ncol=2))
        heights = c(.5, .5)
      } else {
        top.row = suppressMessages(cowplot::plot_grid(NULL, step3, NULL, ncol=3, rel_widths=c(.25, .5, .25)))
        bottom.row = suppressMessages(cowplot::plot_grid(histo, sl, ncol=2))
        heights = c(.6, .4)
      }
      
    } else {
      if (length(numbers)>0){
        top.row = step3
        bottom.row = suppressMessages(cowplot::plot_grid(histo, res.dep, sl, ncol=3))
        heights = c(.7, .3)				
      } else {
        top.row = step3
        bottom.row = suppressMessages(cowplot::plot_grid(histo, sl, ncol=2))
        heights = c(.7, .3)								
      }
    }	
    plot = suppressMessages(cowplot::plot_grid(top.row, bottom.row, nrow=2, rel_heights=heights))
  }	
  
  class(plot) <- c("flexplot", class(plot))
  return(plot)
}


### this function just produces residual plots, so I can reuse it between methods

residual.plots = function(data, object,...){
  
  terms = attr(terms(object), "term.labels")
  
  #### remove interaction and polynomial terms from "terms"
  terms = remove_nonlinear_terms(terms)
  
  
  #### identify factors
  if (length(terms)>1){
    factors = names(which(unlist(lapply(data[,terms], is.factor))));
    numbers = names(which(unlist(lapply(data[,terms], is.numeric))));
  } else {
    factors = terms[which(is.factor(data[,terms]) | is.character(data[,terms]))]
    numbers = terms[which(is.numeric(data[,terms]))]
  }
  #
  #### figure out what is numeric
  levels = apply(data, 2, FUN=function(x) length(unique(x)))
  #### if there's too few levels and it's not categorical
  factors = !sapply(data, function(x) is.factor(x) | is.character(x))
  if (any(levels<5 & factors)){
    message("Note: one or more of your variables has less than 5 values, yet they're treated as numeric.\n\n")
  }
  
  #### extract names
  x.names = names(data)[-1] 
  y.name = names(data)[1]
  
  #### export residuals
  data$residuals = residuals(object)
  data$abs.res = abs(data$residuals)
  data$fitted = fitted(object)
  
  #### plot residuals
  levels = length(unique(round(data[,"residuals"], digits=2)))	
  data$fitted = round(data$fitted, digits=2)	
  
  histo = ggplot2::ggplot(data=data, aes(x=residuals)) + 
    geom_histogram(fill='lightgray', col='black', bins=min(30, round(levels/2))) +
    theme_bw() + theme(text=element_text(size=14)) +
    labs(x="Residuals", y = "Count", title="Histogram of Residuals")
  class(histo) = c("flexplot", class(histo))
  if (length(numbers)>0){
    #res.dep = ggplot2::ggplot(data=d, aes(x=fitted, y=residuals)) + geom_point() + geom_smooth(method="loess", se=F) + 
    #theme_bw() + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
    res.dep = flexplot(residuals~fitted, data=data,...) + 
      labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
    class(res.dep) = c("flexplot", class(res.dep))		
    
  } else {
    res.dep = NULL
  }
  
  if (length(unique(data$fitted))<7){
    
    sl = flexplot(abs.res~fitted, data=data, method="lm",...) + 
      labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")	
    nd = aggregate(abs.res~fitted, data=data, FUN=median)
    # this is necessary bc ggplot converts to ordered factor when there's few levels
    if (is.ordered(sl$data$fitted)) nd[,1] = factor(nd[,1], ordered=T)
    
    sl = sl + geom_line(data=nd, col="#bf0303", size=1.5, group=1)
    #class(sl) = c("flexplot", class(sl))		
  } else {
    sl = flexplot(abs.res~fitted, data=data, method="lm",...)+ labs(x="fitted", y="Absolute Value\nof Residuals", title="S-L Plot")			
    class(sl) = c("flexplot", class(sl))					
  }
  
  
  
  
  plots = list(histo=histo, res.dep=res.dep, sl=sl, terms=terms, factors=factors, numbers=numbers)
  return(plots)
  #class(plot) <- c("flexplot", class(plot))
  #return(plot)
}