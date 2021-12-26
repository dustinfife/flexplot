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

fill_ghost_reference = function(data, var_to_ghost, value=NULL, ghost.reference=NULL,
                                bins=3, labels=NULL, breaks=NULL) {
  
  browser()
  binned.name = paste0(var_to_ghost, "_binned")
  
  #if the current iteration is null, fill it with median or some factor
  if (is.null(value)) {
    ghost.reference[[var_to_ghost]] = fill_empty_ghost_reference(data, var_to_ghost, value)
  }

  # return the factors (since the previous step already chose the middle one)
  if (!is.numeric(data[[var_to_ghost]])) {
    return(ghost.reference[[var_to_ghost]])
  }
    
  # if breaks are missing (for testing purposes only)
  if (is.null(breaks)) {
    breaks = flexplot_create_breaks(break.me = var_to_ghost, breaks, data, labels, bins=bins)  
  }
  
  # when the variable is numeric
  if (is.numeric(data[[var_to_ghost]])) { 
    binned_name = bin.me(variable=var_to_ghost, data=ghost.reference, 
                         bins=bins, labels=labels, breaks=breaks[[var_to_ghost]], check.breaks=F)
    return(binned_name)
  }
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
    ghost_reference_validated[[i]] = fill_ghost_reference(data=data, var_to_ghost=to.ghost[[i]], value=ghost.reference[[i]],
                                                          ghost.reference=ghost_reference_validated[[i]], bins, labels, breaks)
  }
  
  names(ghost.reference) = to.ghost
  
  # rename the numeric variables to "x_binned"
  which_ghosts_are_numeric = sapply(data[,to.ghost], is.numeric)
  names(ghost.reference)[which_ghosts_are_numeric] = paste0(names(ghost.reference)[which_ghosts_are_numeric], "_binned")
  
  return(ghost.reference)
}