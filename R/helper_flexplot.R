choose_flexplot_type = function(data, formula = NULL, 
                                axis=NULL, outcome=NULL, plot.type=NULL, variables=NULL, 
                                suppress_smooth=F, spread="quartile", jitter=c(.1,0), mean.line=F) {
  
  ## set up conditions
  y_is_categorical = !is.numeric(data[[outcome]])
  levels_in_y = length(unique(data[[outcome]]))
  x_is_categorical = !is.numeric(data[[axis[1]]])
  
  
  if (is.null(axis)) {
    variables = all.vars(formula, unique=FALSE)
    outcome = variables[1]
    axis = flexplot_axis_given(formula)$axis
  }  
  
  #### univariate plots
  if (length(axis)==1 & axis[1] == "1") {
    ### prevent univariates from binning numeric variables with <5 levels
    data = modify_univariate_data_numeric(data=data, axis=axis, outcome=outcome)
    plot_string = create_univariate_plot(data, outcome, plot.type)
    return(list(plot_string=plot_string, data=data))
  }
  
  ### related plot
  if (related) {
    data = modify_related_data(data=data, related=related, axis=axis, 
                               outcome=outcome, variables=variables)
    plot_string = create_related_plot(data, outcome, plot.type,
                                      suppress_smooth, spread)
    return(list(plot_string=plot_string, data=data))
  }
  
  ### association plot
  if (y_is_categorical & x_is_categorical){
    data = modify_association_plot_data(data, outcome, axis)
    plot_string = create_association_plot()
    return(list(plot_string=plot_string, data=data))
  }
  
  ### logistic plot
  if (y_is_categorical & levels_in_y == 2) {
    data = factor.to.logistic(data,outcome, method)
    plot_string = create_logistic_plot(data, axis, jitter)
    return(list(plot_string=plot_string, data=data))
  }
  
  ### beeswarm plot
  if (x_is_categorical) {
    data = flexplot_convert_to_categorical(data, axis)
    plot_string = create_beeswarm_plot(data, axis, jitter, suppress_smooth, spread, mean.line)
    return(list(plot_string=plot_string, data=data))    
  }
  
  ### scatterplot
  return(create_scatter_plot(data, axis, jitter, suppress_smooth, method))
  
}

add_second_axis = function(data, axis, plot) {
  
  if (length(axis)==1) return(plot)
  
  if (is.numeric(data[,axis[2]])){
    axis2_binned = paste0(axis[2], "_binned")
    
    plot$p = paste0('ggplot(data=data, aes_string(y=outcome, x=axis, 
                              color=', axis2_binned, ', linetype = ', axis2_binned, ', shape=', axis2_binned, ')) 
                         + labs(color= "', axis2_binned, '", linetype= "', axis2_binned, '", shape= "', axis2_binned, '")')
    return(plot)
  }
  
  # if they're trying to plot more than 10 symbols...
  unique_values_in_axis_2 = length(unique(data[,axis[2]]))
  if (unique_values_in_axis_2>6) {
    message("It looks like you're trying to plot more than 6 colors/lines/symbols.\nI gotta give it to you...you're ambitious. Alas, I can't do that, so I'm removing the colors/lines/symbols.\n I hope we can still be friends.")
    plot$p = 'ggplot(data=data, aes_string(x=axis[1], y=outcome, color=axis[2]))'
    return(plot)
  } 
  
  plot$p = 'ggplot(data=data, aes_string(x=axis[1], y=outcome, color=axis[2], linetype = axis[2], shape=axis[2])) + 
            labs(color= axis[2], linetype= axis[2], shape= axis[2])'
  return(plot$p)
}

### change se based on how many variables they have
modify_se = function(se, axis) {
  if (is.null(se)) return(NULL)
  if (length(axis)>1) return(F)
  return(T)
}


# this function tests for functions within an R formula and returns those results
formula_functions = function(formula, data) {
  
  term_labels = attr(terms(formula), "term.labels")
  
  # return original data if there's no functions
  which_are_functions = grep("(", term_labels, fixed=T)
  if (length(which_are_functions)==0) return(list(data=data, formula=formula))
  
  # extract names of variables
  vars = unlist(lapply(term_labels[which_are_functions], 
                       get_var_names_within_function, return.var=T))
  # apply function to the variables
  new_vars = term_labels[which_are_functions] %>% 
        purrr::map(perform_function, data=data) %>% 
        purrr::set_names(vars) %>% 
        data.frame
  data[,vars] = new_vars
  
  # replace formula with unmodified formula
  string_formula = deparse(formula)
  new_string_formula = gsub(term_labels[which_are_functions], vars, string_formula, fixed=T)
  new_formula = as.formula(new_string_formula)
  list(data=data, formula = new_formula)
}

# this function applied a function passed as string to the data
perform_function = function(string, data) {
  with(data, eval(parse(text=string)))
}

# function that returns the variables or functions passed from a string
get_var_names_within_function = function(string, return.var=TRUE) {
  components = unlist(strsplit(string, split="(", fixed=T))
  var = gsub(")", "", components[2], fixed=T)
  fun = match.fun(components[1])
  if (return.var) return(var)
  fun
}


# this function takes all the arguments needed for the rest of the --------
# function and stores them as a list --------------------------------------
flexplot_prep_variables = function(formula, data, method, breaks=NULL, bins=3){
  
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]

  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  flexplot_errors(variables = variables, data = data, method=method, axis=axis)
  
  #### identify which variables are numeric and which are factors
  vtypes = variable_types(predictors, data, return.names=T)
  numbers = vtypes$numbers
  categories = vtypes$characters
  if (outcome %in% categories){
    levels = length(unique(data[,outcome]))	### necessary for univariate plots
  }
  
  ### create the lists that contain the breaks
  break.me = flexplot_break_me(data, predictors, given, axis, bins)
  breaks = flexplot_create_breaks(break.me = break.me, breaks, data, labels, bins=bins)
  
  list(variables=variables, outcome=outcome, predictors=predictors, 
       given=given, axis=axis, numbers=numbers, categories=categories, 
       levels=levels, break.me=break.me, breaks=breaks,
       bins=bins, breaks=breaks)
}

#flexplot_random_names(10, data.names = c("h", "b"))
flexplot_random_names = function(data.names=NULL, n=10) {
  if (!is.null(data.names[1])){
    nm = data.names[1]
    while (nm %in% data.names) {
      nm = paste(sample(LETTERS[1:26], size=n, replace=T), collapse="")    
    }
    return (nm)
  }
  
  paste(sample(LETTERS[1:26], size=n, replace=T), collapse="")    

}  


flexplot_alpha_default = function(data, axis, alpha){
  if (axis[1] != "1"){
    ### reorder axis and alter default alpha if categorical
    if (!is.numeric(data[,axis[1]])){
      #### set default alpha
      if(alpha==.99977){
        alpha = .2
      }		
    } else {
      if(alpha==.99977){
        alpha = .5
      }	
    }
  }
  
  return(alpha)
}








flexplot_modify_data = function(formula = NULL, data, related = FALSE, variables = NULL, outcome = NULL, 
                                axis = NULL, given=NULL, labels = NULL, bins = NULL, breaks=NULL, break.me=NULL, 
                                spread=c('quartiles', 'stdev', 'sterr'), pred.data=FALSE, method="quartiles"){
  
  if (is.null(data)) return(data) 

  # make all variables into objects from the formula
  # (I believe this is only to make it easier to test, so I don't have to provide so many objects)
  if (!is.null(formula)) {
    prep_vars = flexplot_prep_variables(formula, data=data)
    variables = prep_vars$variables; outcome = prep_vars$outcome; axis = prep_vars$axis; given = prep_vars$given
    break.me = prep_vars$break.me; breaks = prep_vars$breaks; predictors = prep_vars$predictors; 
  }
  
  if (pred.data) {
    outcome = "prediction"
    variables[1] = "prediction"
    data[,"model"] = factor(data[,"model"])
  }
  
  ### remove missing values
  data = flexplot_delete_na(data, variables)

  ### convert variables with < 5 categories to ordered factors
  data = flexplot_convert_to_categorical(data, axis)
  
  # prepare data for association plot
  data = modify_association_plot_data(data=data, outcome=outcome, axis=axis)
  
  # create a binned
  data = bin_variables(data=data, bins=bins, labels=labels, break.me=break.me, breaks=breaks)
  
  # make sure method = 'logistic' under the right circumstances
  method = identify_method(data, outcome, axis, method)

  # convert data for logistic regression
  data = factor.to.logistic(data,outcome, method)

  if (axis[1] != "1") {
    data = modify_data_for_bivariate_plots(data, axis=axis, outcome=outcome, related=related, spread=spread)
  }
  
  ### reorder levels of given 2
  if (length(given)>1){ 
      ### for categorical variables, they're not binned, so we have to include the option where they're not
      if (is.numeric(data[,given[2]])) data[,paste0(given[2], "_binned")] = forcats::fct_rev(data[,paste0(given[2], "_binned")])  
  }
  return(data)
  
  
}

modify_data_for_bivariate_plots = function(data, formula = NULL, axis=NULL, outcome=NULL,related=F, 
                                            spread = "quartiles") {

  if (is.null(axis)) axis = flexplot_axis_given(formula)$axis
  if (is.null(outcome)) outcome = all.vars(formula)[1]
  
  #specify conditionals
  x_is_categorical = !is.numeric(data[,axis[1]])
  y_is_numeric     = is.numeric(data[,outcome])
  x_is_ordered     = is.ordered(data[, axis[1]])
  
  #### order by medians for numeric outcomes
  if (x_is_categorical & y_is_numeric & !x_is_ordered & !related){
    return(reorder_x_by_center(data=data, outcome=outcome, axis=axis, spread=spread))
  }
  #### order by frequency for categorical outcomes
  if (x_is_categorical & !x_is_ordered){
    return(reorder_x_by_n(data, outcome))
  }
  return(data)
}

reorder_x_by_center = function(data, outcome=NULL, axis=NULL, formula=NULL, spread = "quartiles") {
  
  if (is.null(axis)) axis = flexplot_axis_given(formula)$axis
  if (is.null(outcome)) outcome = all.vars(formula)[1]
  
  if (spread=="quartiles") fn = "median" else fn = "mean"
  ord = aggregate(data[,outcome]~data[, axis[1]], FUN=fn, na.rm=T)
  ord = ord[order(ord[,2], decreasing=T),]
  data[,axis[1]] = factor(data[, axis[1]], levels=ord[,1])
  return(data)
}

reorder_x_by_n = function(data, outcome) {
  sizes = table(data[,outcome])
  ord = order(sizes, decreasing = T)
  data[,outcome] = factor(data[, outcome], levels=names(sizes)[ord])
  return(data)
}


# expect_error(flexplot_errors("hello", exercise_data, method="logistic", axis="hello"))
# expect_error(flexplot_errors(c("weight.loss", "therapy.type"), exercise_data, method="logistic", axis="hello"))
# expect_error(flexplot_errors(c("gender", "therapy.type"), exercise_data, method="logistic", axis="therapy.type"))
# expect_error(flexplot_errors(c("weight.loss", "therapy.type"), NULL, method="logistic", axis="hello"))
flexplot_errors = function(variables, data, method=method, axis){
  
  if (is.null(data)){
    stop("Howdy! Looks like you forgot to include a dataset! Kinda hard to plot something with no data. Or so I've heard. What do I know? I'm just a computer. ")
  }
  
  if (any(duplicated(variables))){
    dup = variables[duplicated(variables)]
    stop(paste0("You know what? It seems you have, my dear user, tried using a variable more than once (specifically, ", dup, "). That's not something I can do! \n\nBut we can still be friends"))
  }
  
  if (!all(variables %in% names(data))){
    not.there = variables[which(!(variables %in% names(data)))]
    stop(paste0("Ru oh! Somebody done made a mistake! Looks like you either spelled something wrong, or included a variable not in your dataset! Have you considered spellcheck? (Oh, btw, it was the variable(s) ", paste0(not.there, collapse=","), " that caused a problem"))
  }

  #### give an error if they try to visualize logistic with a categorical x axis
  if (method=="logistic" & length(variables)>0){
    if (!is.numeric(data[,axis[1]])){
      stop(paste0("\nOh wise user of flexplot, you have encountered one of the FEW limitations of flexplot. Sorry, but you cannot plot a logistic curve when a categorical variable is on the x-axis. Sorry! Either remove the logistic request or put a numeric variable on the x axis. \n
				Best of luck on your statistical journeys."))
    }	
  } 
  
}

  #### this function figures out which variables need to be binned
#expect_identical(flexplot_break_me(exercise_data, c("muscle.gain", "income"), given="income"), "income")
#expect_identical(flexplot_break_me(exercise_data, c("weight.loss", "income"), given="income"), "income")
#expect_equal(length(flexplot_break_me(exercise_data, c("weight.loss", "income"), given=NULL)), 0)
#expect_equal(length(flexplot_break_me(exercise_data, c("weight.loss", "income", "weight.loss", "motivation", "therapy.type"), given=c("weight.loss", "motivation"))), 2)
flexplot_break_me = function(data, predictors, given, axis, bins){

  ### without this line of code, there's an error for those situations where there is no second axis
  if (length(axis)<2){
    second.axis = NA
  } else {
    second.axis = axis[2]
  }
  
  ### with a ~1 as axis one, I need to add an if statement
  if (axis[1] != "1") non.axis.one = predictors[-1] else non.axis.one = predictors
  #### get the breaks for the needed variables (remove axis 1 because it's the axis and thus will never be binned)
  #### also, lapply fails when there's just one additional predictor, hence the if statement
  
  if (length(predictors)>2){
    break.me = non.axis.one[unlist(lapply(data[,non.axis.one], FUN=is.numeric)) & ((non.axis.one %in% given) | (second.axis %in% non.axis.one))]	
  } else {
    break.me = non.axis.one[is.numeric(data[,non.axis.one] ) & ((non.axis.one %in% given) | (second.axis %in% non.axis.one))]	
  }
  
  # drop those break.me's that have the same number of levels as bins
  num_unique = apply(data[,break.me, drop=FALSE], 2, function(x) length(unique(x)))
  remove_these = which(num_unique<=bins)
  if (length(remove_these)>0) break.me = break.me[-remove_these]

  #if (length(break.me)==0) break.me = NA
  return(break.me)
}


#flexplot_create_breaks(c("weight.loss", "motivation"), breaks=NULL, data, labels=NULL)
#flexplot_create_breaks(c("weight.loss", "motivation"), breaks=list(weight.loss=c(30, 70)), data, labels=NULL)
#flexplot_create_breaks(NULL, breaks=list(weight.loss=c(30, 70)), data, labels=NULL)
#expect_error(flexplot_create_breaks(break.me = c("weight.loss", "motivation"), breaks=c(30,70), data = data, labels=NULL))
#expect_error(flexplot_create_breaks(break.me = c("weight.loss", "motivation"), breaks=list(weightloss = c(30,70)), data = data, labels=NULL))
#flexplot_create_breaks(break.me = c("weight.loss", "motivation"), breaks=list(weight.loss = c(30)), data = data, labels=list(weight.loss = c("low", "high")))
# This function creates the breaks for the binning
flexplot_create_breaks = function(break.me, breaks, data, labels, bins=3){

  #### did they provide the breaks?
  if (!is.null(breaks)) {
    named.breaks = names(breaks)
  } else {
    named.breaks = NA
  }	
  
  #### create a list of breaks
  if (length(break.me)>0){
    
    #### bark at them if they forgot to name their breaks
    if (is.null(named.breaks)){	
      stop("You must name your breaks if you provide them. Be sure to do that. (e.g., breaks = list(variable1=c(5, 10, 15)), variable2=c(0,1,2))")
      
      #### make sure they spelled breaks right and such
    } else if (!is.na(named.breaks) & !(named.breaks %in% break.me)){
      stop("I can't find ", named.breaks, " in your list of variables to be binned (", paste0(break.me, collapse=","), "). Did you spell everything right?")
    }
    
    #### make an empty list if they don't provide breaks
    if (is.null(breaks)){
      breaks = rep(list(NULL),length(break.me))
    }
    
    #### now make the breaks and convert the data
    for (i in 1:length(break.me)){
      
      #### figure out how many bins there are
      if (length(names(labels)) >= i){
        if (length(labels[[i]])>=i){
          bins = length(labels[[i]])
        } else {
          bins = bins
        }
      } else {
        bins = bins
      }
      
		
      breaks[[i]] = prep.breaks(variable=break.me[i], data, breaks=breaks[[break.me[i]]], bins)
    }
  } else {
    breaks = NULL
  }
  if (length(breaks)>0)  names(breaks) = break.me
  
  return(breaks)
}


# expect_true(is.na(flexplot_axis_given(formula(y~1))$given))
# expect_true(length(flexplot_axis_given(formula(y~x+z))$axis) ==2)
# expect_true(length(flexplot_axis_given(formula(y~x+z|z))$given) ==1)
flexplot_axis_given = function(formula){
  given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))
  given = gsub(" ", "", given)		
  given = unlist(strsplit(given, "+", fixed=T))	
  axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
  axis = gsub(" ", "", axis)			
  axis = unlist(strsplit(axis, "+", fixed=T))	
  list(given=given, axis=axis)
}

#expect_true(nrow(flexplot_delete_na(exercise_data, "muscle.gain.missing"))==167)
#expect_true(nrow(flexplot_delete_na(exercise_data, "muscle.gain"))==200)
#expect_true(nrow(flexplot_delete_na(exercise_data, NULL))==200)
flexplot_delete_na = function(data, variables){

  if (length(variables)>=0){
      keepers = complete.cases(data[,variables,drop=FALSE])
      data = data[keepers,, drop=FALSE]
      return(data)
    } 

  return(data)
}


#expect_true(is.ordered(flexplot_convert_to_categorical(data %>% mutate(gender = as.numeric(gender)), "gender")$gender))
#expect_true(is.ordered(flexplot_convert_to_categorical(data %>% mutate(gender = as.numeric(gender)), c("therapy.type", "gender"))$gender))
#expect_false(is.ordered(flexplot_convert_to_categorical(data, axis=NULL)$gender))
flexplot_convert_to_categorical = function(data, axis){

  #### if they only have a few levels on the x axis, convert it to categorical
  if (length(axis)>0 & axis[1] != "1"){
    if (is.numeric(data[,axis[1]]) & length(unique(data[,axis[1]]))<5){
      data[,axis[1]] = factor(data[,axis[1]], ordered=T)
    }
    
    ### do the same for the second axis
    if (length(axis)>1){
      if (is.numeric(data[,axis[2]]) & length(unique(data[,axis[2]]))<5){
        data[,axis[2]] = factor(data[,axis[2]], ordered=T)
      }		
    }
  }
  return(data)
}



# uni = flexplot_bivariate_plot(weight.loss~1, data=exercise_data)$p
# expect_identical(uni, "ggplot(data=data, aes_string(outcome)) + geom_histogram(fill=\"lightgray\", col=\"black\", bins=min(30, round(levels/2))) + theme_bw() + labs(x=outcome)")
# uni2 = flexplot_bivariate_plot(therapy.type~1, data=exercise_data)$p
# expect_identical(uni2, "ggplot(data=data, aes_string(outcome)) + geom_bar() + theme_bw() + labs(x= outcome)")
# uni3 = flexplot_bivariate_plot(therapy.type~1, data=exercise_data %>% mutate(therapy.type = factor(therapy.type, ordered=T)))$p
# expect_identical(uni3, "ggplot(data=data, aes_string(outcome)) + geom_bar() + theme_bw() + labs(x= outcome)")
# chi = flexplot_bivariate_plot(therapy.type~gender, data=exercise_data %>% mutate(therapy.type = factor(therapy.type, ordered=T)))$p
# tst = "ggplot(data=data, aes_string(x=axis, y='Proportion', fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()"
# expect_identical(chi, tst)
# chi = flexplot_bivariate_plot(therapy.type~gender, data=exercise_data)$p
# tst = "ggplot(data=data, aes_string(x=axis, y='Proportion', fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()"
# expect_identical(chi, tst)
# bv = flexplot_bivariate_plot(weight.loss~gender, data=exercise_data)$p
# expect_identical(bv, "ggplot(data=data, aes_string(x=axis, y=outcome))")
# bv = flexplot_bivariate_plot(weight.loss~motivation, data=exercise_data)$p
# expect_identical(bv, "ggplot(data=data, aes_string(x=axis, y=outcome))")
flexplot_bivariate_plot = function(formula = NULL, data, prediction, outcome, predictors, axis, # variable types and stuff
                                    related, alpha, jitter, suppress_smooth, method, spread, plot.type  # arguments passed from flexplot
                                   ){
  
  jitter = match_jitter_categorical(jitter)
  if (is.null(formula)){
    list.na = list(outcome, predictors, axis, related, alpha, jitter, suppress_smooth, method, spread)
    isnull =  names(which(unlist(lapply(list.na, is.null))))
    if (length(isnull)>0){
      stop(paste0("You must either provide a formula OR all variables requested. It looks like you're missing the variable ", isnull))
    }
  } else {
    prep_vars = flexplot_prep_variables(formula, data=data)
    predictors = prep_vars$predictors; outcome = prep_vars$outcome; axis = prep_vars$axis; 
    related = prep_vars$related; alpha= prep_vars$alpha; jitter= prep_vars$jitter 
    suppress_smooth= prep_vars$suppress_smooth; method= prep_vars$method; spread= prep_vars$spread
  }
  
  #### histograms
  if (length(outcome)==1 & length(predictors)==0 | axis[1] == "1"){
    
    ### figure out how many levels for the variable
    levels = length(unique(data[,outcome]))	
    
    #### if numeric, do a histogram
    if (is.numeric(data[,outcome])){
      if (plot.type=="qq"){
        p = 'ggplot(data=data, aes_string(sample = outcome)) + stat_qq() + stat_qq_line() + theme_bw() + labs(x=outcome)'
      } else if (plot.type == "density") {
        p = 'ggplot(data=data, aes_string(outcome)) + geom_density() + theme_bw() + labs(x=outcome)'
      } else {
        p = 'ggplot(data=data, aes_string(outcome)) + geom_histogram(fill="lightgray", col="black", bins=min(30, round(levels/2))) + theme_bw() + labs(x=outcome)'
      }
    } else {
      p = 'ggplot(data=data, aes_string(outcome)) + geom_bar() + theme_bw() + labs(x= outcome)'		
    } 
    points = "xxxx"
    fitted = "xxxx"		
    
  ### BIVARIATE PLOTS
  } else if (length(outcome)==1 & length(axis)==1 & !related){

    #### if both are categorical, do chi square
    if (!is.numeric(data[[outcome]]) & !is.numeric(data[[axis]])){
      
      p = "ggplot(data=data, aes_string(x=axis, y='Proportion', fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()"
      points = "xxxx"
      fitted = "xxxx"
      
    } else {
      
      p = 'ggplot(data=data, aes_string(x=axis, y=outcome))'
      points = points.func(axis.var=axis, data=data, jitter=jitter)
      if (plot.type == "boxplot"){
        fitted = 'geom_boxplot(alpha=.1)'
      } else if (plot.type == "violin"){
        fitted = 'geom_violin(alpha=.1)'
      } else if (plot.type == "line") {
        fitted = 'geom_line()'
      } else {
        fitted = fit.function(outcome, axis, data=data, suppress_smooth=suppress_smooth, method=method, spread=spread)		
      }
    }	
    
   } else if (length(axis)>1){

    ### if they supply predictions, do not vary color
    if (!is.null(prediction)){
      p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2], shape=axis[2])) + labs(color= axis[2], shape= axis[2])'
      
    } else {
      if (is.numeric(data[,axis[2]])){
        axis[2] = paste0(axis[2], "_binned"); axis2_binned = axis[2]
        p = paste0('ggplot(data=data, aes(x=', predictors[1], ', ', y=outcome, 
                   ', color=', axis2_binned, ', linetype = ', axis2_binned, 
                   ', shape=', axis2_binned, ')) + labs(color= "', axis2_binned, '", linetype= "', axis2_binned, '", shape= "', axis2_binned, '")')
      } else {
        # if they're trying to plot more than 10 symbols...
        if (length(unique(data[,axis[2]]))>6) {
          message("It looks like you're trying to plot more than 6 colors/lines/symbols.\nI gotta give it to you...you're ambitious. Alas, I can't do that, so I'm removing the colors/lines/symbols.\n I hope we can still be friends.")
          p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2]))'
        } else {
          p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2], linetype = axis[2], shape=axis[2])) + labs(color= axis[2], linetype= axis[2], shape= axis[2])'
        }
      }
      ### remove the default color if they have categorical variables		
    }
    points = points.func(axis.var=axis, data=data, jitter=jitter)
    fitted = fit.function(outcome, predictors=axis[1], data=data, suppress_smooth=suppress_smooth, method=method, spread=spread, mean.line=TRUE)
    
    
    ### remove the default color if they have something in the second axis
    if (!is.numeric(data[,axis[2]])){
      fitted = gsub(", color = '#bf0303'", "", fitted, fixed=T)
      fitted = gsub(', color = "#bf0303"', "", fitted, fixed=T)
    }	
  }
  
  list(p=p, points=points, fitted=fitted)
}


#### flexplot function for paneling
flexplot_panel_variables = function(given, break.me){
  
  if (is.na(given[1])) return("xxxx")
  
  #### prep the given variables to be stringed together
  given_binned = given
  
  if (length(break.me)>0){
    given_binned[given_binned%in%break.me] = paste0(given_binned[given_binned%in%break.me], "_binned")
  }	

  if (given[1]=="") {
    given.as.string = paste0(given_binned[2], "~.")
  } else {
    given.as.string = ifelse(length(given)>1 & !is.na(given_binned[1]),paste0(rev(given_binned), collapse="~"), paste0("~",given_binned))
  }

  facets = paste0('facet_grid(as.formula(', given.as.string, '),labeller = custom.labeler)')			
  return(facets)
}



#