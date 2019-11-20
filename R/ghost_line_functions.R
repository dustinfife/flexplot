create_ghost_text = function(d_smooth, axis, outcome, prediction, ghost.line, ghost.reference, data){
  ### rename columns
  names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 
  
  ## add line to existing plot 
  if (!is.null(prediction) & length(levels(prediction$model))>1){  
    d_smooth$model = factor(d_smooth$group, labels=levels(prediction$model))
    ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group="model", linetype="model"), color=ghost.line, show.legend=F)'			
  } else if (length(axis)>1){	
    
    #### used to be factoring d_smooth$group, but that gave different groups for each color AND line, so just making it line now
    #### if they specify BOTH references, allow it. Otherwise, pick one
    if (length(unique(d_smooth$linetype))==1){
      d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=ghost.reference[[axis[2]]])
    } else {
      d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=sort(levels(factor(data[[axis[2]]]))))
    }
    
    ### it seems ggplot is choosing the order based on sorting
    
    ### if the ghost line specifies a specific line to plot...
    axis2_notbinned = gsub("_binned", "",axis[2])
    if (axis2_notbinned %in% names(ghost.reference)){
      d_smooth = d_smooth[d_smooth[,axis[2]]==(ghost.reference[[axis2_notbinned]]),]
      ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group=axis[2], linetype=axis[2]), color=ghost.line, show.legend=F)'								
    } else {
      ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group=axis[2], linetype=axis[2]), color=ghost.line, show.legend=F)'				
    }
  } else {	
    ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome), color=ghost.line, show.legend=F)'
  }
  
  list(ghost=ghost, d_smooth = d_smooth)
}

create_ghost_dataset = function(data, axis, prediction, given, ghost.reference, predictors, p, fitted, method, outcome, se){
  #### if they specified a prediction, extract data from prediction model
  if (!is.null(prediction)){
    k = prediction
  } else {
    k = data
  }
  s=1
  
  #### norrow down based on GIVEN, not the axis (because we're trying to find the right panel, then plot only the one group line)
  for (s in 1:length(given)){
    binned.name = paste0(given[s], "_binned")
    
    ### specify k based on whether they supply a prediction
    if (is.null(prediction)){
      k = k[(k[,binned.name])==unlist(ghost.reference[[given[s]]]),]				
    } else {
      message(binned.name)
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
  message(g0)
  g0 = eval(parse(text=g0))		
  
  d_smooth = suppressMessages(ggplot_build(g0)$data[[1]])
  return(d_smooth)
}

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