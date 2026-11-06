# vars = flexplot_prep_variables(weight.loss~motivation|therapy.type, data=exercise_data)
# expect_error(with(vars, create_ghost_reference(data = data, given=given, axis=axis, bins=bins, labels=labels, breaks=breaks)))  # no formula
# bv = create_ghost_reference(weight.loss~motivation|therapy.type, data=exercise_data) # with formula, null ghost reference
# expect_identical(bv[[1]], "cog")
create_ghost_reference= function(formula = NULL, data, given=NULL, ghost.reference=NULL, axis=NULL, bins=3, labels=NULL, breaks=NULL){

  if (is.null(formula)){
    
    list.na = list(given=given, axis=axis, bins=bins)
    isnull =  names(which(unlist(lapply(list.na, is.null))))
    if (length(isnull)>0){
      stop(paste0("You must either provide a formula OR all variables requested. It looks like you're missing the variable ", paste0(isnull, collapse=", ")))
    }
  } else {
    vars = flexplot_prep_variables(formula, data=data)
    variables = vars$variables; outcome = vars$outcome; predictors = vars$predictors;
    given = vars$given; axis = vars$axis; numbers = vars$numbers; categories = vars$numbers
    levels = vars$levels; break.me = vars$break.me; breaks = vars$breaks;
    formula = vars$formula; data = vars$data; break.me = vars$break.me
  }

  if (!is.null(ghost.reference)){
    
    ### what needs a reference? all given and MAYBE axis[2]
    if (axis[2] %in% names(ghost.reference)) {
      to.ghost = c(given, axis[2])
    } else {
      to.ghost = c(given)
    }					
    
    #breaks.keep = names(which(!unlist(lapply(to.ghost,is.null))))
    for (i in seq_along(to.ghost)) {
      
      binned.name = to.ghost[i]
      
      if (is.numeric(data[[binned.name]])) {
        candidate = paste0(binned.name, "_binned")
        if (candidate %in% names(data)) {
          binned.name = candidate
        }
      }
      
      ### if the ghost reference is null, fill with middle value
      if (is.null(ghost.reference[[to.ghost[i]]])){
        l = data[,binned.name]
        
        middle = levels(l); middle = middle[round((length(middle))/2)]
        ghost.reference[[to.ghost[i]]] = middle
        
        message(paste0("Hey-yo: You didn't choose ghost.reference values for ", to.ghost[i], ", which means I get to chose it. That shows a lot of trust. I appreciate that. I won't let you down.\n"))
        #### when they supply ghost reference information
      } else {
        
        if (is.numeric(ghost.reference[[to.ghost[i]]]) & length(unique(data[,to.ghost[i]]))>bins){
          ghost.reference[[to.ghost[i]]] = bin.me(variable= to.ghost[i], data=ghost.reference, bins=bins, labels=labels[i], breaks=breaks[[to.ghost[i]]], check.breaks=F)
        } else {
          ghost.reference[[to.ghost[i]]] = unlist(ghost.reference[[to.ghost[i]]])
        }
        
      }
    }
    
  } else {

    #### if they have an axis 2 variable
    if (length(axis)>1){
      to.bin = c(axis[2], given)
    } else {
      to.bin = given
    }
    
    #### if they don't specify any reference group, choose the middle one
    ghost.reference=list()
    for (b in 1:length(to.bin)){
      # use x_binned if it's numeric
      if (!is.numeric(data[,to.bin[b]])){
        given.bin = to.bin[b]
      } else {
        given.bin = paste0(gsub("_binned", "", to.bin[b]), "_binned")
      }
      
      # when there's a numeric variable with unique<bins, there's an error unless I do this
      if (!(given.bin %in% names(data))) given.bin = to.bin[b]
      
      ### format given as a binned variable
      l = factor(data[,given.bin])
      middle = levels(l); middle = middle[round((length(middle))/2)]
      ghost.reference[[to.bin[b]]]=middle
    }

    message(paste0("Note: You didn't specify a reference for the ghost line, which means I get to chose it. That shows a lot of trust. I appreciate that. I won't let you down."))
    
    #### if they specify a reference group for some of them, but not all
  }

  return(ghost.reference)
}
