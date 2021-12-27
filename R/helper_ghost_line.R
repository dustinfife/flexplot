variables_to_be_ghosted = function(formula=NULL, axis=NULL, given=NULL) {
  if (!is.null(formula)) {
    given.axis = flexplot_axis_given(formula)
    given = given.axis$given
    axis = given.axis$axis
  }
  if (length(axis)>1) return(c(axis[2], na.omit(given)))
  if (!is.na(given)) return(given)
  return(NULL)
}

fill_empty_ghost_reference = function(data, var_to_ghost, value=NULL) {
  
  if (!is.null(value)) return(value)
  if (is.numeric(data[[var_to_ghost]])) return(median(data[[var_to_ghost]], na.rm=T))
  l = factor(data[,var_to_ghost])
  middle = levels(l); middle = middle[round((length(middle))/2)]
  return(middle)
}

fill_ghost_reference = function(data, var_to_ghost, value=NULL,
                                bins=3, labels=NULL, breaks=NULL) {
  
  #if the current iteration is null, fill it with median or some factor
  if (is.null(value)) {
    value = fill_empty_ghost_reference(data, var_to_ghost, value)
  } 

  # return the factors (since the previous step already chose the middle one)
  if (!is.numeric(data[[var_to_ghost]])) {
    return(value)
  }
    
  # if breaks are missing (for testing purposes only)
  if (is.null(breaks)) {
    breaks = flexplot_create_breaks(break.me = var_to_ghost, breaks, data, labels, bins=bins)  
  }
  
  # when the variable is numeric, we need to bin it
  labels = label_bins(labels, breaks[[var_to_ghost]])
  binned_name = cut(as.numeric(value), breaks[[var_to_ghost]], labels= labels, include.lowest=T, include.highest=T)
  return(binned_name)
}

get_ghost_reference_value = function(ghost.reference, name) {
  if (is.null(ghost.reference)) return(NULL)
  if (!(name  %in% names(ghost.reference))) return(NULL)
  return(ghost.reference[[name]])
}

create_ghost_reference= function(formula = NULL, data, given=NULL, ghost.line=NULL,
                                 ghost.reference=NULL, axis=NULL, bins=3, labels=NULL, breaks=NULL){
  
  if (is.null(ghost.line)) return("xxxx")
  
  if (is.null(axis)) {
    given.axis = flexplot_axis_given(formula)
    given = given.axis$given
    axis = given.axis$axis
  }
  
  to.ghost = variables_to_be_ghosted(NULL, axis, given)
  
  if (is.null(ghost.reference)) {
    message(paste0("Note: You didn't specify a reference for the ghost line, which means I get to chose it. ", 
                   "That shows a lot of trust. I appreciate that. I won't let you down."))
  }
  ghost_reference_validated = list()
  
  for (i in 1:length(to.ghost)){
    value = get_ghost_reference_value(ghost.reference, to.ghost[i])
    ghost_reference_validated[[i]] = fill_ghost_reference(data=data, var_to_ghost=to.ghost[i], value=value,
                                                          bins, labels, breaks)
  }
  
  names(ghost_reference_validated) = to.ghost
  
  # rename the numeric variables to "x_binned"
  which_ghosts_are_numeric = sapply(data[,to.ghost, drop=F], is.numeric)
  names(ghost_reference_validated)[which_ghosts_are_numeric] = paste0(names(ghost_reference_validated)[which_ghosts_are_numeric], "_binned")
  
  return(ghost_reference_validated)
}

create_ghost_dataset = function(data, axis, prediction, given, ghost.reference, predictors, p, fitted, method, outcome, se){
  
  #### if they specified a prediction, extract data from prediction model
  if (!is.null(prediction)) k = prediction else k = data
  
  s=1
  
  #### norrow down based on GIVEN, not the axis (because we're trying to find the right panel, then plot only the one group line)
  for (s in 1:length(given)){
    
    if (is.numeric(data[,given[s]])){
      binned.name = paste0(given[s], "_binned")
    } else {
      binned.name = given[s]
    }
    
    ### specify k based on whether they supply a prediction
    if (is.null(prediction)){
      k = k[(k[,binned.name])==unlist(ghost.reference[[given[s]]]),]				
    } else {
      rows = which(k[,binned.name]==unlist(ghost.reference[[given[s]]]))
      k = k[rows,]
      
      names(k)[names(k)=="prediction"] = outcome
      
    }
    
  }
  
  #### is k gone???
  if (nrow(k)==0){
    stop("there was an error in generating the ghost line. Please email the developer: fife.dustin@gmail.com")
  }
  
  
  ### create ggplot object to extract the fit for the ghost line
  if (!is.null(prediction)){
    
    #### ghost line needs a FITTED LINE, otherwise it generates weird zigzags. When you do a prediction line with no fitted line, that's a problem.
    #### each given variable still has multiple entries, so average across those entires
    
    #### average within the binned values
    grouped.vars = c("model", predictors[(!(predictors %in% given))])
    k = k %>% group_by_at(vars(one_of(grouped.vars))) %>% summarize_at(.vars = outcome, .funs=mean)
    g0 = paste0('ggplot(data=k, aes_string(x=axis[1], y=outcome, linetype="model", group="model"))+', fitted)							
  } else {
    g0 = paste0(gsub("data=[[:alnum:]]+,", "data=k,", p), "+",fitted)
    #g0 = paste0('ggplot(data=k, aes_string(x=axis[1], y=outcome))+', fitted)
    
  }
  g0 = gsub("+xxxx", "", g0, fixed=T)
  
  #### if they have a logistic, modify the p to reflect the data
  if (method=="logistic" & !is.numeric(k[,outcome])){
    g0 = gsub("data=[[:alnum:]]+,", "data=factor.to.logistic(k,outcome),", g0)
  }
  g0 = eval(parse(text=g0))		
  
  d_smooth = suppressMessages(ggplot_build(g0)$data[[1]])
  return(d_smooth)
}