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

jamovi_glm_fit = function(formula, data, label) {
  model = tryCatch(stats::lm(formula, data=data, na.action=stats::na.fail),
                   error=function(error) stop(label, ": ", conditionMessage(error), call.=FALSE))
  if (model$rank < length(stats::coef(model)) || stats::df.residual(model) <= 0L) {
    stop(label, " is not estimable. Check redundant terms, empty factor combinations, ",
         "and the number of complete observations.", call.=FALSE)
  }
  model
}

jamovi_glm_comparison = function(options, data) {
  formula1 = jamovi_glm_formula(options$out, options$preds, options$modelTerms)
  formula2 = jamovi_glm_formula(options$out, options$preds, options$comparisonTerms)
  variables = union(all.vars(formula1), all.vars(formula2))
  eligible = data[stats::complete.cases(data[, variables, drop=FALSE]), , drop=FALSE]
  if (!nrow(eligible)) stop("No complete observations are available for both models.", call.=FALSE)
  model1 = jamovi_glm_fit(formula1, eligible, "Model 1")
  model2 = jamovi_glm_fit(formula2, eligible, "Model 2")

  predictors = setdiff(variables, options$out)
  x = options$comparisonX
  if (!length(x)) {
    numeric = predictors[vapply(eligible[predictors], is.numeric, logical(1))]
    x = if (length(numeric)) numeric[1] else predictors[1]
  }
  panels = options$comparisonPanels
  if (length(x) != 1L || is.na(x) || !x %in% predictors ||
      length(panels) > 2L || any(!panels %in% predictors) ||
      anyDuplicated(c(x, panels))) {
    stop("Choose one comparison x variable and up to two different panel variables ",
         "from the predictors used by either model.", call.=FALSE)
  }
  rhs = as.name(x)
  if (length(panels)) rhs = call("|", rhs, formula_component(lapply(panels, as.name)))
  plot_formula = make.formula(as.name(options$out), rhs)
  comparison = model.comparison(model1, model2)
  # Keep the F-test numeric: model.comparison formats its rounded p-value as text.
  p = get_p_value(model1, model2, check_nested(model1, model2))
  list(model1=model1, model2=model2, formula=plot_formula, data=eligible,
       statistics=comparison$statistics, p=if (length(p)) p[1] else NA_real_)
}

jamovi_glm_comparison_table = function(table, state) {
  statistics = state$statistics
  for (i in seq_len(2L)) {
    row = list(model=paste("Model", i), n=stats::nobs(state[[paste0("model", i)]]))
    for (name in c("aic", "bic", "bayes.factor", "rsq", "adj.rsq")) {
      row[[name]] = if (name %in% names(statistics)) as.numeric(statistics[i, name]) else NA_real_
    }
    row$p = if (i == 1L) state$p else NA_real_
    table$addRow(rowKey=i, values=row)
  }
  formulas = vapply(list(state$model1, state$model2), function(model)
    paste(deparse(stats::formula(model)), collapse=" "), character(1))
  table$addFootnote(rowNo=1, col="model", note=paste("Model 1:", formulas[1], "Model 2:", formulas[2]))
  table$addFootnote(rowNo=1, col="n", note="Both models use the same complete observations across their combined variables.")
  table$addFootnote(rowNo=1, col="bayes.factor", note="Approximate Bayes factors from BIC, as reported by model.comparison().")
  table$addFootnote(rowNo=1, col="p", note="F-test for nested models only; unavailable for nonnested or equivalent models.")
}

jamovi_glm_comparison_plot = function(state) {
  model1 = state$model1
  model2 = state$model2
  compare_fits(state$formula, data=state$data, model1=model1, model2=model2)
}
