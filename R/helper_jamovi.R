# GUI variable selections are literal identifiers, including names resembling R.
jamovi_formula = function(out, preds=NULL, given=NULL) {
  lhs = formula_component(out, response=TRUE)
  rhs = if (length(preds)) formula_component(lapply(preds, as.name)) else 1
  if (length(given)) rhs = call("|", rhs, formula_component(lapply(given, as.name)))
  structure(call("~", lhs, rhs), class="formula", .Environment=parent.frame())
}

# Retained for callers needing identifier text. R's serializer escapes both
# backslashes and backticks; hand-escaping just backticks is insufficient.
escape_spaces_in_formulas = function(out=NULL) {
  if (is.null(out)) return(NULL)
  vapply(out, formula_identifier_text, character(1), USE.NAMES=FALSE)
}

formula_identifier_text = function(x) {
  paste(deparse(as.name(x), backtick=TRUE, width.cutoff=500L), collapse="")
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
