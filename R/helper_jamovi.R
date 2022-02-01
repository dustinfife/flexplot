#### write formula for flexplot
jamovi_formula = function(out, preds=NULL, given=NULL) {
  
  # escape the spaces if necessary
  out   = escape_spaces_in_formulas(out)
  preds = escape_spaces_in_formulas(preds)
  given = escape_spaces_in_formulas(given)
  
  # construct formula for univariate plot
  if (is.null(preds) & is.null(given) & !is.null(out)) {
    formula = as.formula(paste0(out, "~1"))
    return(as.formula(formula))
  }
  
  # construct formula for no givens
  if (is.null(given) & !is.null(out)) {
    formula = paste0(out, "~", paste0(preds, collapse="+"))
    return(as.formula(formula))
  }

  formula <- paste0(out, "~", 
                    paste0(preds, collapse="+"), "|", 
                    paste0(given, collapse="+"))		            	
  return(as.formula(formula))
}

escape_spaces_in_formulas = function(out=NULL) {
  if (is.null(out)) return(NULL)
  escaped_items = lapply(out, function(x) {
    if (x == make.names(x)){
      return(x)
    } 
    
    paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`")
  })
  
  return(unlist(escaped_items))
}

get_fitted_line = function(line) {
  if (is.null(line)) return("loess")
  if (line=="Loess") return("loess")
  if (line=="Regression") return("lm")
  if (line=="Logistic") return("logistic")
  if (line=="Polynomial") return("polynomial")
  if (line=="Cubic") return("cubic")	
  if (line=="Robust") return("rlm")
  if (line=="Time Series") return("loess")
  return("loess")
}

# create function that does if/else, but with NULLs allowed
ifelse_null = function(object, statement, true_option=object, false_option=NULL) {
  # check if there's an error when selecting objects of the arguments
  # (it will be an error if the object isn't found)
  if (class(try(as.list(environment()), silent=T)) == "try-error") return(NULL)
  if (is.null(object)) return(false_option)
  if (statement) return(true_option)
  return(false_option)
}
