label_avp_axis = function(formula) {
  terms = strsplit(deparse(formula), "~")[[1]]
  return(paste0(terms[1], "|", terms[2]))
}

make_avp_formula = function(formula, lm_formula=NULL, x=NULL) {
  
  if (!is.null(lm_formula)){
    axis_label = label_avp_axis(lm_formula)
    
    # substitute formula
    y_var = all.vars(formula)[1]
    # remove white space from entire formula
    newf = gsub(" ", "", deparse(formula))
    formula = formula(gsub(paste0(y_var, "~"), "residuals~", newf))
    return(list(lm_formula, formula, axis_label))
  }
  
  #pick out the variables
  variables = all.vars(formula)
  outcome = variables[1]
  predictors = variables[-1]
  res.variable = find_variable_of_interest(predictors, x)
  remaining.vars = predictors[which(!(predictors %in% res.variable))]
  
  #### model the first chosen variable
  lm_formula = make.formula(outcome, remaining.vars)
  formula = make.formula("residuals", res.variable)
  list(lm_formula, formula, paste0(outcome, " | ", paste0(remaining.vars, collapse=", ")))
}

# this function ensures all variables in lm_formula are there in formula
check_variables_in_lm = function(formula, lm_formula, check_both = FALSE){
  
  if (is.null(lm_formula)) return(NULL)
  
  # make sure the outcome variable is the same
  if (all.vars(formula)[1] != all.vars(lm_formula)[1]) {
    msg = paste0("Your outcome variable for your model (", all.vars(lm_formula)[1], 
                 ") is not the same as your outcome variable for your avp (", all.vars(formula)[1], ")")
    stop(msg)
  }
  
  # make sure it's a valid formula
  if (!rlang::is_bare_formula(lm_formula)) {
    stop("You need to specify a valid formula for added variable plots")
  }
  
  if (check_both) {
    #make sure all the variables in lm are the same as avp
    if (!all(all.vars(formula) %in% all.vars(lm_formula) )) {
      msg = paste0("One or more of the variables provided in the lm formula (", deparse(lm_formula),
                   ") don't match your plotting formula (", deparse(formula), ")")
      stop(msg)
    }
  }
  return(NULL)
}

find_variable_of_interest = function(predictors, x=NULL) {
  
  if (is.null(x)) return(predictors[length(predictors)])
  
  # make sure x is in the data
  if (is.numeric(x) & x>length(predictors)) stop("Oops! You're asking for a variable position that is larger than the number of predictors in your formula! Pick a smaller number for x.")
  if (is.numeric(x)) return(predictors[x])
  if (x %!in% predictors) stop(paste0("Oops! I can't find the variable ", x, " in your formula"))
  return(x)
}

prep_data_for_avp = function(data, variables) {
  # convert from tibble
  if (tibble::is_tibble(data)){
    data = data.frame(data)
  }
  
  #### remove missing data
  miss.vals = sapply(data[,variables], function(x) sum(is.na(x)))
  miss.vals = miss.vals[miss.vals>0]
  if (length(miss.vals)!=0){
    warning("Note: I'm removing missing values so the function will work.")
    data = na.omit(data[,variables])
  }
  return(data)
}

