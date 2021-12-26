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
      ### the following code is for JASP to work
      if (length(grep("_binned", axis[2]))==0 & is.numeric(data[,axis[2]])){
        axis[2] = paste0(axis[2], "_binned")
      }
      d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=sort(levels(factor(data[,axis[2]]))))
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
  
  ### odd condition where if they name the group "group" it will conflict with ggplot
  if (names(d_smooth)[1] == names(d_smooth)[2]) names(d_smooth)[1] = paste0(letters[sample(1:26, 10, T)], collapse="")

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







