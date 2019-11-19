create_ghost_reference= function(ghost.reference, given, axis, data, bins, labels, breaks){

    if (!is.null(ghost.reference)){
    
    ### what needs a reference? all given and MAYBE axis[2]
    if (axis[2] %in% names(ghost.reference)) {
      to.ghost = c(given, axis[2])
    } else {
      to.ghost = c(given)
    }					
    
    #breaks.keep = names(which(!unlist(lapply(to.ghost,is.null))))
    for (i in 1:length(to.ghost)){
      
      binned.name = paste0(to.ghost[i], "_binned")
      
      ### if the ghost reference is null, fill with middle value
      if (is.null(ghost.reference[[to.ghost[i]]])){
        l = data[,binned.name]
        
        middle = levels(l); middle = middle[round((length(middle))/2)]
        ghost.reference[[to.ghost[i]]] = middle
        
        message(paste0("Hey-yo: You didn't choose ghost.reference values for ", to.ghost[i], ", which means I get to chose it. That shows a lot of trust. I appreciate that. I won't let you down.\n"))
        #### when they supply ghost reference information
      } else {
        
        if (is.numeric(ghost.reference[[to.ghost[i]]])){
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
      ### format given as a binned variable
      l = factor(data[,given.bin])
      
      middle = levels(l); middle = middle[round((length(middle))/2)]
      ghost.reference[[to.bin[b]]]=middle
    }
    
    #### if they have an axis[2], add that to the ghost reference
    # if (length(axis)>1){
    # 	ghost.reference[[axis[2]]] = data[,axis[2]]
    # }
    
    message(paste0("Note: You didn't specify a reference for the ghost line, which means I get to chose it. That shows a lot of trust. I appreciate that. I won't let you down."))
    
    #### if they specify a reference group for some of them, but not all
  }
  
  return(ghost.reference)
}