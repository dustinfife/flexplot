create_ghost_text = function(d_smooth, axis, outcome, prediction, ghost.line, ghost.reference, data){

  
    ### rename columns
  names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 
  ## add line to existing plot 
  if (!is.null(prediction) & length(levels(prediction$model))>1){  
    d_smooth$model = factor(d_smooth$group, labels=levels(prediction$model))
    ghost = 'geom_line(data=d_smooth, aes(x=!!sym(axis[1]), y= !!sym(outcome), group=!!sym("model"), linetype=!!sym("model")), color=ghost.line, show.legend=F)'			
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

      # sometimes not all axis[2] groups show up in each panel. 
      # if that's the case, line 25 throws an error. So we need to limit to the number that is available
      # This is quite hacky because I don't know if there's a correspondence between labels and linetype here
      total_n_of_lines = length(unique(d_smooth$linetype))
      d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=sort(unique(factor(data[,axis[2]])))[1:total_n_of_lines])
    }
    

    
    ### it seems ggplot is choosing the order based on sorting
    
    ### if the ghost line specifies a specific line to plot...
    axis2_notbinned = gsub("_binned", "",axis[2])
    if (axis2_notbinned %in% names(ghost.reference)){
      d_smooth = d_smooth[d_smooth[,axis[2]]==(ghost.reference[[axis2_notbinned]]),]
      ghost = 'geom_line(data=d_smooth, aes(x=!!sym(axis[1]), y=!!sym(outcome), group=!!sym(axis[2]), linetype=!!sym(axis[2])), color=ghost.line, show.legend=F)'								
    } else {
      ghost = 'geom_line(data=d_smooth, aes(x=!!sym(axis[1]), y= !!sym(outcome), group=!!sym(axis[2]), linetype=!!sym(axis[2])), color=ghost.line, show.legend=F)'				
    }
  } else {	
    ghost = 'geom_line(data=d_smooth, aes(x=!!sym(axis[1]), y= !!sym(outcome)), color=ghost.line, show.legend=F)'
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
    
    #when there's a numeric variable with unique<bins, there's an error unless I do this
    if (!(binned.name %in% names(k))) binned.name = given[s]
    
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
