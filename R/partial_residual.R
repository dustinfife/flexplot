#' Partial Residual Plot
#'
#' @param plot_formula a flexplot-style formula for plotting
#' @param lm_formula Optional. A lm-style formula for the model being fit
#' @param model Optional. A lm model
#' @param data The dataset
#' @param added_term a formula, which specifies which terms should be de-residualized. By default,
#' it will use the residuals from `model`. 
#' @param suppress_model A boolean. Should the model be suppressed? Defaults to T. 
#' @param ... Other arguments passed to flexplot
#'
#' @return a partial residual plot
#' @export
#'
#' @examples
#' partial_residual_plot(weight.loss~therapy.type, 
#'    lm_formula = weight.loss~therapy.type + motivation, 
#'    data=exercise_data)
partial_residual_plot = function(plot_formula, lm_formula=NULL, model=NULL, data, 
                                 added_term = NULL, suppress_model=F, ...) {

  # error messages and data checking
  if (is.null(lm_formula) & is.null(model)) stop("You must provide either a lm formula or a model")
  if (is.null(lm_formula)) lm_formula = formula(model)
  if (!is.null(added_term)) check_all_variables_exist_in_data(all.vars(added_term), data)
  check_all_variables_exist_in_data(all.vars(plot_formula), data)
  check_all_variables_exist_in_data(all.vars(lm_formula), data)
  check_variables_in_lm(plot_formula, lm_formula, check_both = TRUE)

  # remove missing data
  variables = all.vars(lm_formula)
  data = prep_data_for_avp(data, variables)
  
  if (is.null(model)) model = lm(lm_formula, data=data)

  # compute the partial residuals
  residual = partial_residual(model, added_term) 
  # replace original dv with residual
  data[,all.vars(lm_formula)[1]] = residual
  
  ## create plot so we can get the "binned" variables (if they exist)
  plot_data = flexplot(plot_formula, data=data, suppress_smooth=T, ...) 
  
  # if model is provided, use model to generate predictions
  if (!is.null(model) & !suppress_model) {

    # identify variables with _binned in the name
    binned_vars = grep("_binned", names(plot_data$data), fixed=T, value=T)
    unbinned_name = gsub("_binned", "", binned_vars)
    
    # select all variables in the plot
    all_variables = all.vars(plot_formula)                # all variables in flexplot formula
    all_model_variables = all.vars(formula(model))        # all variables in model 
    not_plotted_vars = 
      all_model_variables[!all_model_variables 
                          %in% all_variables]        # variables in model, but not plot
    
    # merge the means with the dataset and replace the original variable with the binned mean
    k = plot_data$data %>% 
      group_by(across(all_of(binned_vars))) %>% 
      summarize_at(unbinned_name, mean) %>% 
      full_join(plot_data$data, by=binned_vars, suffix = c("", ".y")) %>% 
      mutate_at(not_plotted_vars, mean) %>% 
      data.frame 
    

    # 5. identify which components go into the model
    # just use predict on the plot data
    if (!is.null(added_term)) {
      terms_2_predict = return_matching_terms(added_term, model)
      k$predict = rowSums(predict(model, newdata=k, terms = terms_2_predict, type = "terms"))
    } else {
      k$predict = 0#predict(model, newdata=k)
    }
    
    # fix the intercepts by making the means of prediction/residuals the same
    k$predict = k$predict - (mean(k$predict) - mean(data[,all.vars(lm_formula)[1]]))
    
    # color line depending on if there's a group aesthetic
    if (is.null(plot_data$mapping$colour)) fitted_line = geom_line(data=k, aes(y=predict), colour="#8F0000", size=1.5) else fitted_line = geom_line(data=k, aes(y=predict)) 
    
  } else {
    fitted_line = geom_blank()
  }

  # plot it
  y_label = paste0(paste0(lm_formula)[2], " ~ ", paste0(lm_formula)[3])
  
  ## suppress smooth if they leave it on defaults

  args = list(...)
  if (any(c("suppress_smooth", "method") %in% names(args))) return(flexplot(plot_formula, data=data,...) + fitted_line + labs(y=y_label))
  return(plot_data + fitted_line + labs(y=y_label))
  
}

# model = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
# all(return_matching_terms(~therapy.type*motivation, model)==c("motivation", "therapy.type", "motivation:therapy.type"))
# all(return_matching_terms(~therapy.type+motivation, model)==c("therapy.type","motivation"))
# all(return_matching_terms(~motivation*therapy.type, model)==c("motivation", "therapy.type", "motivation:therapy.type"))
return_matching_terms = function(added_terms, model) {
  # extract terms as a vector
  terms_added = attr(terms(added_terms), "term.labels")
  terms_model = attr(terms(model), "term.labels")
  interaction_components = grep(":", terms_added)
  # return "terms_added" if there are no interactions
  if (length(interaction_components)==0) return(terms_added)
  # return "terms_added" if all terms specified are in the model
  if (all(terms_added %in% terms_model)) return(terms_added)
  # loop through all interaction components and flip then check
  reordered_added_terms = terms_added %>% purrr::map_chr(reorder_interaction_terms)
  reordered_model_terms = terms_model %>% purrr::map_chr(reorder_interaction_terms)
  matching_terms = reordered_model_terms %in% reordered_added_terms
  return(terms_model[matching_terms])
  
}
# reorder_interaction_terms("a:c:b")=="a:b:c"
# reorder_interaction_terms("a")=="a"
reorder_interaction_terms = function(term) {
  # if it's not an interaction, return the term
  if (length(grep(":", term))==0) return (term)
  return(strsplit(term, ":", fixed=T) %>% 
           unlist %>% 
           sort %>% 
           paste0(collapse=":"))
}
#model = lm(weight.loss~therapy.type * motivation + health, data=exercise_data)
#term = ~therapy.type*motivation
#term = c("motivation", "therapy.type")
partial_residual = function(model, term=NULL) {

  # extract model residuals  
  res = residuals(model)
  if (is.null(term)) return(res)
  # get data
  data = extract_data_from_fitted_object(model)
  matrix_coded = terms_to_modelmatrix(term, data)
  # if the user specifies a*b but in the model it was b*a, there will be an error. Fix that
  # just identify which columns are identical
  keep_columns = keep_duplicates(model.matrix(model), matrix_coded)
  betas_of_interest = matrix(coef(model)[dimnames(keep_columns)[[2]]], 
                             nrow=nrow(matrix_coded), ncol=ncol(keep_columns),
                             byrow=T)
  # return it
  if (ncol(matrix_coded)>1) return(res + rowSums(betas_of_interest*keep_columns) - coef(model)[1])
  return(res + betas_of_interest*matrix_coded- coef(model)[1])
  
}


terms_to_modelmatrix = function(term, data) {
  
  # convert terms to formula
  if (typeof(term)!= "language") term = formula(paste0("~", paste0(term, collapse="+")))
  
  # create model matrix
  matrix_coded = model.matrix(term, data=data)
  return(matrix_coded)
}

get_same_columns = function(original_model, new_model, return_unique = T) {
  columns_to_keep = which(duplicated(as.list(data.frame(original_model,new_model)), fromLast=TRUE))
  if (return_unique) return(original_model[,columns_to_keep])
  
  # return those not duplicated
  v = as.list(data.frame(original_model,new_model))
  return(keep_singles(v))
}

data_columns_as_list = function(...) {
  return(as.list(data.frame(...)))
}
# from https://stackoverflow.com/questions/37381174/r-removing-duplicate-elements-in-a-vector
keep_singles = function(...){
  v = data_columns_as_list(...)
  keep = which(!(v %in% v[duplicated(v)]))
  return(colnames(list(...)[[1]])[keep]) 
}
keep_duplicates = function(...){
  v = data_columns_as_list(...)
  keep = which(duplicated(v, fromLast=TRUE))
  return(list(...)[[1]][,keep])
}


return_term_location = function(model, term) {
  
  if (is.null(term)) stop("You must provide a term")
  variables = all.vars(formula(model))
  
  # identify which terms are not in the model
  notthere = which(!(term %in% variables))
  if (length(notthere)>0) stop(paste0("The term(s): ", paste0(term[notthere], collapse=", "), " are not in the model"))
  return(which(variables %in% term))
}
