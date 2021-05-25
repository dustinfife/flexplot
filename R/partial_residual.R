#' Partial Residual Plot
#'
#' @param plot_formula a flexplot-style formula for plotting
#' @param lm_formula Optional. A lm-style formula for the model being fit
#' @param model Optional. A lm model
#' @param data The dataset
#' @param added_term a formula, which specifies which terms should be de-residualized. By default,
#' it will use the residuals from `model`. 
#' @param ... Other arguments passed to flexplot
#'
#' @return a partial residual plot
#' @export
#'
#' @examples
#' partial_residual_plot(weight.loss~therapy.type, lm_formula = weight.loss~therapy.type + motivation, 
#'    data=exercise_data)
partial_residual_plot = function(plot_formula, lm_formula=NULL, model=NULL, data, 
                                 added_term = NULL, suppress_model=F, ...) {

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

  # if model is provided, use compare.fits to get actual fitted values
  if (!is.null(model)) {
    preds = suppressMessages(compare.fits(plot_formula, data=data, model1=model, return.preds=T))
    # recenter predictions
    preds$prediction = rescale(preds$prediction, mean(data[,variables[1]]), sd(data[,variables[1]]))
    if (is.null(added_term)) model_matrix = model.matrix(model) else model_matrix = terms_to_modelmatrix(added_term, data)
    columns_to_subract = keep_singles(model.matrix(model), model_matrix)
    if (length(columns_to_subract)>1)
      preds$prediction = preds$prediction - (sum(coef(model)[columns_to_subract]*colMeans(model.matrix(model)[,columns_to_subract])) + coef(model)[1])
    else if (length(columns_to_subract)==1)
      preds$prediction = preds$prediction - (sum(coef(model)[columns_to_subract]*mean(model.matrix(model)[,columns_to_subract])) + coef(model)[1])
    else if (!is.null(added_term))
      preds$prediction = preds$prediction - mean(data[,variables[1]])+ coef(model)[1]
    else 
      preds$prediction = 0 #preds$prediction
  }

  # replace original dv with residual
  data[,all.vars(lm_formula)[1]] = residual

 
  
  # plot it
  y_label = paste0(paste0(lm_formula)[2], " ~ ", paste0(lm_formula)[3])
  
  ## suppress smooth if they leave it on defaults
  #browser()
  args = list(...)
  if (any(c("suppress_smooth", "method") %in% names(args))) {
    if (suppress_model) return(flexplot(plot_formula, data=data, ...) + labs(y=y_label))
    return(flexplot(plot_formula, data=data, prediction = preds, ...) + labs(y=y_label))
  }
  
  if (suppress_model) return(flexplot(plot_formula, data=data, suppress_smooth=T,...) + labs(y=y_label))
  flexplot(plot_formula, data=data, prediction = preds, suppress_smooth=T,...) +
    labs(y=y_label)
  
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
  head(res + rowSums(betas_of_interest*keep_columns) - coef(model)[1])
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
