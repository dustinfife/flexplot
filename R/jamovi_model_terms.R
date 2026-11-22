# Model terms from jamovi are lists of literal variable names, not R code.
jamovi_glm_interaction = function(lhs, rhs) call(":", lhs, rhs)

jamovi_glm_terms = function(formula) {
  terms = stats::terms(formula)
  variables = as.list(attr(terms, "variables"))[-1]
  if (length(formula) != 2L || !all(vapply(variables, is.symbol, logical(1)))) {
    stop("Use a one-sided model-terms formula with predictor names and interactions only.", call.=FALSE)
  }
  if (attr(terms, "intercept") != 1L) {
    stop("The jamovi model-term controls include an intercept; no-intercept formulas are not supported.", call.=FALSE)
  }
  factors = attr(terms, "factors")
  if (!length(factors)) return(list())
  names = vapply(variables, as.character, character(1))
  lapply(seq_len(ncol(factors)), function(i) names[factors[, i] != 0])
}

jamovi_glm_term = function(term, predictors) {
  if (!is.character(term) || !length(term) || anyNA(term) ||
      any(!term %in% predictors) || anyDuplicated(term)) {
    stop("Model terms must use the selected predictor variables.", call.=FALSE)
  }
  Reduce(jamovi_glm_interaction, lapply(term, as.name))
}

jamovi_glm_formula = function(outcome, predictors, terms) {
  if (is.null(terms)) terms = as.list(predictors)
  if (!is.list(terms)) stop("Model terms must be a list of terms.", call.=FALSE)
  make.formula(as.name(outcome), lapply(terms, jamovi_glm_term, predictors=predictors))
}

# Shared fixed-effect formulas: quadratic selections are literal numeric names.
jamovi_fixed_formula = function(options, data, terms=options$modelTerms, quadratic=options$quadratic) {
  formula = jamovi_glm_formula(options$out, options$preds, terms)
  if (is.null(quadratic)) quadratic = character()
  for (variable in quadratic) {
    if (!variable %in% options$preds || !is.numeric(data[[variable]])) {
      stop("Quadratic terms require selected numeric predictors.", call.=FALSE)
    }
    formula[[3]] = call("+", formula[[3]], call("I", call("^", as.name(variable), 2)))
  }
  formula
}
