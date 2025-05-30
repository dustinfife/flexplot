

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



# expect_true(length(names(flexplot_prep_variables(weight.loss~therapy.type, data=exercise_data)))==23)
# expect_true(length(flexplot_prep_variables(weight.loss~therapy.type + motivation, data=exercise_data)$variables)==3)
# this function takes all the arguments needed for the rest of the --------
# function and stores them as a list --------------------------------------
flexplot_prep_variables = function(formula, data, breaks=NULL, related=F, labels=NULL, bins=3, 
                                   jitter=NULL, suppress_smooth=F, method="loess", spread=c('quartiles', 'stdev', 'sterr'), 
                                   alpha=.99977, prediction=NULL){

  spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
  
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  flexplot_errors(variables = variables, data = data, axis=axis)
  
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
       levels=levels, data=data, break.me=break.me, breaks=breaks, formula = formula, data = data,
       related = related, labels=labels, bins=bins, breaks=breaks, jitter=jitter, suppress_smooth=suppress_smooth,
       method = method, spread = spread, alpha = alpha, prediction = prediction)
}

#flexplot_random_names(10, data.names = c("h", "b"))
# flexplot_random_names = function(data.names=NULL, n=10) {
#   if (!is.null(data.names[1])){
#     nm = data.names[1]
#     while (nm %in% data.names) {
#       nm = paste(sample(LETTERS[1:26], size=n, replace=T), collapse="")    
#     }
#     return (nm)
#   }
#   
#   paste(sample(LETTERS[1:26], size=n, replace=T), collapse="")    
# 
# }  



### prep data for association plot
modify_association_plot_data = function(data, formula, outcome) {

  variables = all.vars(formula, unique=FALSE)
  predictors = variables[-1]
  
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  
  # this needs to be here when using visualize (since formula is null and it will
  # say outcome and axis are not numeric (because they're null))
  if (is.na(axis[1]) | axis[1] == "1") return(data)
  if (all(is.na(data[,outcome])) | all(is.na(data[,axis]))) return(data)
  
  #maybe best to do log linear model (poisson?)
  if (!is.numeric(data[[outcome]]) & !is.numeric(data[[axis[1]]])) {
    m = as.data.frame(table(data[,c(predictors, outcome)])); names(m)[1:(ncol(m)-1)] = c(predictors, outcome)
    loglin = glm(make.formula("Freq", c(predictors, outcome)), data=m, family=poisson)
    predicted = predict(loglin, type="response")
    final.pred = (m$Freq - predicted)/predicted
    m$Freq = final.pred
    names(m)[names(m)=="Freq"] = "Proportion"
    return(m)
  }
  
  return(data)
}


# expect_true(is.factor(modify_univariate_data_numeric(data=data.frame(a=1:4), "1", "a")$a))
# expect_false(is.factor(modify_univariate_data_numeric(data=data.frame(a=1:5), "1", "a")$a))
# expect_false(is.factor(modify_univariate_data_numeric(data=data.frame(a=rnorm(111)), "1", "a")$a))
modify_univariate_data_numeric = function(data, axis, outcome) {
  if (axis[1] == "1" & is.numeric(data[,outcome]) & length(unique(data[,outcome]))<5){
    data[,outcome] = factor(data[,outcome], ordered=TRUE)
    return(data)
  }
  return(data)
}


# d = data.frame(group = rep(c(1,2), times=5), outcome = rnorm(10))
# modify_related_data(d, T, "group", "outcome", variables = c("group", "outcome"))
# modify_related_data(d, F, "group", "outcome", variables = c("group", "outcome"))
# d = data.frame(group = rep(c(1,2,3), times=5), outcome = rnorm(15))
# expect_error(modify_related_data(d, T, "group", "outcome", variables = c("group", "outcome")))
# d = data.frame(group = rep(c(1,2), times=5), outcome = rnorm(10), predictor = rnorm(10))
# expect_error(modify_related_data(d, T, "group", "outcome", variables = c("group", "outcome", "predictor")))
# d = data.frame(group = sample(c(1,2), size=18, replace=T, prob=c(.8, .3)), outcome = rnorm(18))
# expect_error(modify_related_data(d, T, "group", "outcome", variables = c("group", "outcome")))
modify_related_data = function(data, related, axis, outcome, variables) {
  
  if (!related) return(data)
  
  #### extract levels of the predictors
  levs = unique(data[,axis[1]])
  
  #### create difference scores
  g1 = data[data[, axis[1]]==levs[1], outcome]
  g2 = data[data[, axis[1]]==levs[2], outcome]		
  
  ### error checking
  if (length(variables)!=2) stop("Currently, the 'related' option is only available when there's a single predictor.")
  if (length(levs)!=2) stop("Sorry, I can only accept two levels of the grouping variable when related=T.")
  if (length(g1) != length(g2)) stop("Sorry, the length of the two groups are not the same. I can only create difference scores when the group sizes are identical.")
  
  lab = paste0("Difference (",levs[2], "-", levs[1], ')')
  data = data.frame(Difference=g2-g1)
  attr(data, "levels") = levs
  data[,variables] = NA
  return(data)
}


# expect_error(flexplot_modify_data(data=exercise_data, variables = "weight.loss"))  ### missing all variables
# expect_true(!is.tibble(flexplot_modify_data(formula = weight.loss~therapy.type, data=exercise_data %>% select(weight.loss, therapy.type))))### data as tibble
# expect_equal(flexplot_modify_data(therapy.type~gender, data=exercise_data)$Proportion[1], .12745098)  ### association plot data
# expect_error(flexplot_modify_data(weight.loss~gender + motivation, data=exercise_data, related=T))
# expect_error(flexplot_modify_data(formula = weight.loss~gender, data=exercise_data, related=T))
# expect_true(all(c("motivation_binned", "income_binned") %in% names(flexplot_modify_data(weight.loss~therapy.type + motivation | income, data=exercise_data))))
# expect_true(all(c("income_binned") %in% names(flexplot_modify_data(weight.loss~therapy.type + gender | income, data=exercise_data))))
flexplot_modify_data = function(formula = NULL, data, related = FALSE, variables = NULL, outcome = NULL, method = NULL, 
                                axis = NULL, given=NULL, labels = NULL, bins = NULL, breaks=NULL, break.me=NULL, 
                                spread=c('quartiles', 'stdev', 'sterr'), pred.data=FALSE){
  
  if (is.null(data)) return(data) 

  # make all variables into objects from the formula
  # (I believe this is only to make it easier to test, so I don't have to provide so many objects)
  if (!is.null(formula) & is.null(variables)) {
    prep_vars = flexplot_prep_variables(formula, data=data)
    variables = prep_vars$variables; outcome = prep_vars$outcome; axis = prep_vars$axis; given = prep_vars$given
    break.me = prep_vars$break.me; breaks = prep_vars$breaks; predictors = prep_vars$predictors; spread = prep_vars$spread
  }
  
  if (pred.data) {
    outcome = "prediction"
    variables[1] = "prediction"
    data[,"model"] = factor(data[,"model"])
  }
  
  ### remove missing values
  data = flexplot_delete_na(data, variables)

  ### convert variables with < 5 categories to ordered factors
  data = flexplot_convert_to_categorical(data, axis, pred.data)

  ### prevent univariates from binning numeric variables with <5 levels
  data = modify_univariate_data_numeric(data=data, axis=axis, outcome=outcome)
  
  # prepare data for related test
  data = modify_related_data(data=data, related=related, axis=axis, outcome=outcome, variables=variables)
  
  data = bin_variables(data=data, bins=bins, labels=labels, break.me=break.me, breaks=breaks)
  
  # prepare data for association plot
  data = modify_association_plot_data(data=data, formula = formula, outcome = outcome)
  
  # make sure method = 'logistic' under the right circumstances
  method = identify_method(data, outcome, axis, method)

  # convert data for logistic regression
  data = factor.to.logistic(data,outcome, method)
  
  #### reorder axis 1 it's not already ordered
  if(axis[1] != "1"){
    #### order by medians for numeric outcomes
    if (!is.numeric(data[,axis[1]]) & is.numeric(data[,outcome]) & !is.ordered(data[, axis[1]]) & !related){
      if (spread[1]=="quartiles"){ fn = "median"} else {fn = "mean"}
      ord = aggregate(data[,outcome]~data[, axis[1]], FUN=fn, na.rm=T)
      ord = ord[order(ord[,2], decreasing=T),]
      data[,axis[1]] = factor(data[, axis[1]], levels=ord[,1])
    }
    #### order by frequency for categorical outcomes
  } else if (!is.numeric(data[,outcome]) & !is.ordered(data[,outcome])){
    sizes = table(data[,outcome])
    ord = order(sizes, decreasing = T)
    data[,outcome] = factor(data[, outcome], levels=names(sizes)[ord])
  }

  ### reorder levels of given 2
  if (length(given)>1){ 
      ### for categorical variables, they're not binned, so we have to include the option where they're not
      if (is.numeric(data[,given[2]]) & length(unique(data[,given[2]]))>bins) data[,paste0(given[2], "_binned")] = forcats::fct_rev(data[,paste0(given[2], "_binned")])  
  }
  return(data)
  
  
}


# expect_error(flexplot_errors("hello", exercise_data, method="logistic", axis="hello"))
# expect_error(flexplot_errors(c("weight.loss", "therapy.type"), exercise_data, method="logistic", axis="hello"))
# expect_error(flexplot_errors(c("gender", "therapy.type"), exercise_data, method="logistic", axis="therapy.type"))
# expect_error(flexplot_errors(c("weight.loss", "therapy.type"), NULL, method="logistic", axis="hello"))
flexplot_errors = function(variables, data, axis){
  
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

}

check_error_for_logistic = function(variables, data, method=method, axis) {
  
  #### give an error if they try to visualize logistic with a categorical x axis
  if (method != "logistic" | length(variables)<1) return(NULL)
  if (is.numeric(data[,axis[1]])) return(NULL)
  stop(paste0("\nOh wise user of flexplot, you have encountered one of the FEW limitations of flexplot. Sorry, but you cannot plot a logistic curve when a categorical variable is on the x-axis. Sorry! Either remove the logistic request or put a numeric variable on the x axis. \n
				Best of luck on your statistical journeys."))
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
    # for when there's a numeric variable less than the number of bins, do NOT bin it!
    is_numeric = unlist(lapply(data[,non.axis.one], function(x) {is.numeric(x) & length(unique(x))>bins}))
    is.given   = (non.axis.one %in% given) | (second.axis %in% non.axis.one)
    break.me = non.axis.one[is_numeric & is.given]	
  } else {
    break.me = non.axis.one[is.numeric(data[,non.axis.one]) & length(unique(data[,non.axis.one]))>bins & ((non.axis.one %in% given) | (second.axis %in% non.axis.one))]	
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
flexplot_convert_to_categorical = function(data, axis, pred=FALSE){

  # do nothing if they're doing a histogram
  if (length(axis)==0 | axis[1] == "1") return(data)
  
  # if x axis has less than 5 levels, convert to ordered factor
  data = convert_if_less_than_five(data, axis[1])
    
  # return data if they only have one axis
  if (length(axis) == 1) return(data)
  
  # if axis 2 has less than 5 levels, also convert to ordered factor
  data = convert_if_less_than_five(data, axis[2], check_pred = TRUE, pred = pred)
      
  return(data)
}

convert_if_less_than_five = function(data, col, check_pred = FALSE, pred = NULL) {
  # Check if conversion conditions are met.
  if (is.numeric(data[, col]) && length(unique(data[, col])) < 5 && (!check_pred || !pred)) {
    data[, col] <- factor(data[, col], ordered = TRUE)
  }
  data
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
                                    related, alpha, jitter, suppress_smooth, method, spread, plot.type, bins  # arguments passed from flexplot
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
  
  # fail if they try to model more than one DV (or less than one)
  if (length(outcome)!=1) stop("Sorry, friend, but you must have one and only one outcome variable.")
  
  #### histograms
  if (length(predictors)==0 | axis[1] == "1"){
    p = flexplot_histogram(data, outcome, plot.type, bins)
    points = "xxxx"
    fitted = "xxxx"
    return(list(p=p, points=points, fitted=fitted))
  } 
  
  ### RELATED T-TEST
  if (related){		
    if (length(axis)>1) stop("Sorry, my friend, you can't do a related plot when you have more than one predictor variable.")
    return(flexplot_related(data, jitter, plot.type, spread))
  } 
  
  ### BIVARIATE PLOTS
  
  if (length(axis)==1){
    return(flexplot_bivariate_string(data, outcome, axis, jitter, plot.type, suppress_smooth, spread, method))
  } 
  
  ### MULTIVARIATE PLOTS
  p = flexplot_multivariate_aes(data, outcome, prediction, axis)
  points = points.func(axis.var=axis, data=data, jitter=jitter)
  fitted = fit.function(outcome, predictors=axis[1], data=data, suppress_smooth=suppress_smooth, 
                        method=method, spread=spread, mean.line=TRUE)
    
  
  ### remove the default color if they have something in the second axis
  if (!is.numeric(data[,axis[2]])){
    fitted = gsub(", color = '#bf0303'", "", fitted, fixed=T)
    fitted = gsub(', color = "#bf0303"', "", fitted, fixed=T)
  }	

  
  list(p=p, points=points, fitted=fitted)
}


calculate_bins_for_histograms = function(bins, levels) {
  if (bins != 3) return(bins)
  return(min(30, round(levels/2)))
}



flexplot_modify_prediction = function(prediction, axis, num.models, break.me, bins, labels, breaks, predictors){

    
  if (!is.na(axis[2]) & length(num.models)>1){
    stop("Sorry. I can't plot the model(s) lines when there are already lines in the plot. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z), or choose to display only one model")
  }
  
  
  #### bin the predictions, where needed

  if (length(break.me)>0){
    for (i in 1:length(break.me)){
      ### find that variable in the model and bin it
      prediction[[break.me[i]]] = bin.me(break.me[i], prediction, bins, labels[i], breaks[[break.me[i]]])
      
    }
    ### now average fit within bin
    groups = c("model", paste0(break.me, "_binned"), predictors[-which(predictors%in%break.me)])
    prediction = prediction %>% group_by_at(groups) %>% summarize(prediction = mean(prediction)) %>% as.data.frame
  }

  
  return(prediction)
}




check_same_variables_in_prediction = function(formula, prediction=NULL) {
  if (is.null(prediction)) return(NULL)
  variables_in_formula = all.vars(formula, unique=FALSE)[-1]
  variables_in_dataset = names(prediction)
  not_there = which(!(variables_in_formula %in% variables_in_dataset))
  if (length(not_there)<1) return(NULL)
  missing_vars = paste0(variables_in_formula[not_there], collapse=",")
  stop(paste0("The variable(s) '", missing_vars, "' are not in your prediction dataset."))
}
#