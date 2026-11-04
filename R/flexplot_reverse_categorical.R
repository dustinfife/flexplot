flexplot_additive_conditioning = function(expr) {
  if (is.symbol(expr)) return(TRUE)
  if (!is.call(expr) || !identical(expr[[1]], as.name("+"))) return(FALSE)
  all(vapply(as.list(expr)[-1], flexplot_additive_conditioning, logical(1)))
}

flexplot_reverse_categorical = function(formula, data, prediction=NULL,
                                        related=FALSE, allow_reverse=TRUE) {
  if (is.null(data) || length(formula) != 3) return(formula)

  outcome_vars = all.vars(formula[[2]])
  rhs = formula[[3]]
  conditioned = is.call(rhs) && identical(rhs[[1]], as.name("|"))
  axis_expr = if (conditioned) rhs[[2]] else rhs
  axis_vars = all.vars(axis_expr)
  if (length(outcome_vars) == 0 || length(axis_vars) == 0) return(formula)

  outcome = data[[outcome_vars[1]]]
  predictor = data[[axis_vars[1]]]
  if (!is.factor(outcome) && !is.character(outcome)) return(formula)
  if (length(unique(stats::na.omit(outcome))) < 3 || !is.numeric(predictor)) {
    return(formula)
  }

  if (!allow_reverse || !is.null(prediction)) {
    stop("Multinomial probability plots are not yet supported for categorical outcomes ",
         "with numeric predictors. Axis reversal is only available for descriptive plots, ",
         "without model predictions or compare.fits().", call.=FALSE)
  }
  if (related) {
    stop("Automatic axis reversal is not supported with related=TRUE.", call.=FALSE)
  }
  if (!is.symbol(formula[[2]]) || !is.symbol(axis_expr)) {
    stop("Automatic axis reversal requires a single categorical outcome and a single numeric predictor. ",
         "Second-axis variables, interactions, and unresolved transformations are not supported.",
         call.=FALSE)
  }
  if (conditioned && (!flexplot_additive_conditioning(rhs[[3]]) ||
                      length(all.vars(rhs[[3]])) > 2)) {
    stop("Automatic axis reversal supports at most two additive conditioning variables.",
         call.=FALSE)
  }

  original_outcome = formula[[2]]
  formula[[2]] = axis_expr
  if (conditioned) {
    rhs[[2]] = original_outcome
    formula[[3]] = rhs
  } else {
    formula[[3]] = original_outcome
  }

  message("Multinomial probability plots are not yet supported. The axes were reversed ",
          "to display the numeric predictor's distribution across outcome categories.")
  formula
}
