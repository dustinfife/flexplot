#' Partial Residual Plot
#'
#' @param plot_formula a flexplot-style formula for plotting
#' @param lm_formula Optional. A lm-style formula for the model being fit
#' @param model Optional. A lm model
#' @param data The dataset
#' @param added_term a formula, which specifies which terms should be de-residualized
#' @param ... Other arguments passed to flexplot
#'
#' @return a partial residual plot
#' @export
#'
#' @examples
#' partial_residual_plot(weight.loss~therapy.type, lm_formula = weight.loss~therapy.type + motivation, 
#'    data=exercise_data)
partial_residual_plot = function(plot_formula, lm_formula=NULL, model=NULL, data, 
                                 added_term = NULL, ...) {

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
  if (!is.null(added_term)) 
    residual = partial_residual(model, added_term) 
  else 
    residual = partial_residual(model, term=all.vars(plot_formula)[-1])
  
  # replace original dv with residual
  data[,all.vars(lm_formula)[1]] = residual
  
  # plot it
  y_label = paste0(paste0(lm_formula)[2], " ~ ", paste0(lm_formula)[3])
  flexplot(plot_formula, data=data,...) +
    labs(y=y_label)
  
}


#model = lm(weight.loss~therapy.type * motivation + health, data=exercise_data)
#term = ~therapy.type*motivation
#term = c("motivation", "therapy.type")
partial_residual = function(model, term=NULL) {
  
  # extract model residuals  
  res = residuals(model)
  
  # get data
  data = extract_data_from_fitted_object(model)
  
  # convert terms to formula
  if (typeof(term)!= "language") term = formula(paste0("~", paste0(term, collapse="+")))
  
  # create model matrix
  matrix_coded = model.matrix(term, data=data)
  
  # if the user specifies a*b but in the model it was b*a, there will be an error. Fix that
  # just identify which columns are identical
  keep_columns = get_same_columns(model.matrix(model), matrix_coded)

  betas_of_interest = matrix(coef(model)[dimnames(keep_columns)[[2]]], 
                             nrow=nrow(matrix_coded), ncol=ncol(matrix_coded),
                             byrow=T)
  
  # return it
  if (ncol(matrix_coded)>1) return(rowSums(res + betas_of_interest*matrix_coded) - coef(model)[1])
  return(res + betas_of_interest*matrix_coded)
  
}

get_same_columns = function(original_model, new_model) {
  columns_to_keep = which(duplicated(as.list(data.frame(original_model,new_model)), fromLast=TRUE))
  return(original_model[,columns_to_keep])
}


return_term_location = function(model, term) {
  
  if (is.null(term)) stop("You must provide a term")
  variables = all.vars(formula(model))
  
  # identify which terms are not in the model
  notthere = which(!(term %in% variables))
  if (length(notthere)>0) stop(paste0("The term(s): ", paste0(term[notthere], collapse=", "), " are not in the model"))
  return(which(variables %in% term))
}
