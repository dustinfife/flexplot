##' \code{make.formula} is a function that easily converts a set of strings into a formula. It requires two arguments: a single response variable,
##' and a vector of strings. See examples. 
##'
##' @title Convert strings to a formula
##' @param response a literal column name, or an explicit language object or one-sided formula for a trusted expression.
##' @param predictors literal column names, combined additively. Character strings are never interpreted as expressions, except a single "0" or "1" retains its intercept convention. Supply symbols for columns named "0" or "1". Trusted expressions can be supplied as a one-sided formula, a call, or a list of symbols/calls.
##' @param random a string that indicates the random component in an \code{lmer}-like object (e.g., "(1|group)"). Defaults
##' to NULL. Character random-effects syntax permits names and operators only; trusted transformations require an explicit one-sided formula or call.
##' @return a formula object
##' @export
##' @author Dustin Fife
##' @examples
##' k = data.frame(matrix(rnorm(100), ncol=5))
##' names(k) = LETTERS[1:5]
##' formula = make.formula("A", LETTERS[2:5])
##' formula
##' lm(formula, data=k)
##' #do a random model
##' make.formula("A", LETTERS[2:5], random="(1|group)")
make.formula = function(response, predictors, random=NULL) {
  lhs = formula_component(response, response=TRUE)
  rhs = formula_component(predictors)
  if (!is.null(random)) rhs = call("+", rhs, formula_random_component(random))
  structure(call("~", lhs, rhs), class="formula", .Environment=parent.frame())
}

# Character inputs are identifiers, never expressions. Formula/call inputs are
# an explicit opt-in for expressions supplied by trusted R callers.
formula_component = function(x, response=FALSE) {
  if (inherits(x, "formula")) {
    if (length(x) != 2L) stop("Use a one-sided formula for a formula component.", call.=FALSE)
    return(x[[2]])
  }
  if (is.language(x)) return(x)
  if (response) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop("response must be one column name or an explicit language object.", call.=FALSE)
    }
    return(as.name(x))
  }
  if (length(x) == 0L) return(1)
  # Retain historical intercept-only conventions for ordinary R callers.
  # GUI selections use symbols instead, so columns named 0 or 1 stay literal.
  if (is.character(x) && length(x) == 1L && x %in% c("0", "1")) return(as.numeric(x))
  if (!is.character(x) && !is.list(x)) {
    stop("predictors must be column names or explicit language objects.", call.=FALSE)
  }
  parts = lapply(x, formula_component, response=TRUE)
  Reduce(formula_add_component, parts)
}

formula_add_component = function(lhs, rhs) call("+", lhs, rhs)

formula_random_component = function(random) {
  if (is.language(random)) return(formula_component(random))
  if (!is.character(random) || length(random) != 1L || is.na(random)) {
    stop("random must be one random-effects string or an explicit formula/call.", call.=FALSE)
  }
  # The random argument historically accepts lmer syntax, not a column name.
  # Parse without evaluation, then permit only the random-effects grammar.
  expr = stats::as.formula(paste("~", random))[[2]]
  if (!formula_random_syntax(expr)) {
    stop("Unsupported random-effects string. Use names and random-effects operators; ",
         "supply a one-sided formula for trusted transformations.", call.=FALSE)
  }
  expr
}

formula_random_syntax = function(expr) {
  if (is.symbol(expr) || is.numeric(expr)) return(TRUE)
  if (!is.call(expr) || !is.symbol(expr[[1]])) return(FALSE)
  if (!as.character(expr[[1]]) %in% c("(", "+", "-", "*", "/", ":", "^", "|", "||")) return(FALSE)
  all(vapply(as.list(expr)[-1], formula_random_syntax, logical(1)))
}
