# The generalized-model jamovi adapter; statistical fitting is delegated to glm.
jamovi_glm_generalized_data = function(data, formula, family) {
  variables = all.vars(formula)
  data = data[stats::complete.cases(data[, variables, drop=FALSE]), , drop=FALSE]
  if (!nrow(data)) stop("No complete observations are available for this model.", call.=FALSE)
  outcome = variables[1]
  response = data[[outcome]]
  if (family == "binomial") {
    if (is.character(response)) response = factor(response)
    if (is.factor(response)) {
      response = droplevels(response)
      if (nlevels(response) != 2L) {
        stop("Logistic regression requires exactly two observed outcome levels.", call.=FALSE)
      }
    } else if (!(is.numeric(response) || is.logical(response)) ||
               !all(response %in% c(0, 1)) || length(unique(response)) != 2L) {
      stop("Logistic regression requires a numeric 0/1 outcome or a two-level categorical outcome, with both levels observed.", call.=FALSE)
    }
    data[[outcome]] = response
    return(data)
  }
  if (!is.numeric(response) || any(!is.finite(response))) {
    stop("The selected distribution requires a finite numeric outcome. For a two-level categorical outcome, choose Binomial / Logistic.", call.=FALSE)
  }
  if (family %in% c("poisson", "negativebinomial") && (any(response < 0 | response != floor(response)) || !any(response > 0))) {
    stop("Count regression requires nonnegative integer counts, including at least one positive count.", call.=FALSE)
  }
  if (family %in% c("Gamma", "gammaLog") && any(response <= 0)) {
    stop("Gamma regression requires strictly positive outcomes.", call.=FALSE)
  }
  if (family == "gaussian" && length(unique(response)) < 2L) {
    stop("Gaussian regression requires an outcome that varies.", call.=FALSE)
  }
  data
}

jamovi_glm_generalized_formula = function(options, data, terms=options$modelTerms, quadratic=options$quadratic) {
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

jamovi_glm_generalized_fit = function(options, data, formula=jamovi_glm_generalized_formula(options, data)) {
  eligible = jamovi_glm_generalized_data(data, formula, options$family)
  family = switch(options$family, gaussian=stats::gaussian(link="identity"),
    binomial=stats::binomial(link="logit"), poisson=stats::poisson(link="log"),
    gammaLog=stats::Gamma(link="log"), Gamma=stats::Gamma(link="inverse"))
  warnings = new.env(parent=emptyenv())
  warnings$messages = character()
  model = withCallingHandlers(
    if (options$family == "negativebinomial") MASS::glm.nb(formula, data=eligible, link=log, na.action=stats::na.fail) else
      stats::glm(formula, data=eligible, family=family, na.action=stats::na.fail),
    warning=function(warning) {
      warnings$messages = unique(c(warnings$messages, conditionMessage(warning)))
      invokeRestart("muffleWarning")
    })
  if (model$rank < length(stats::coef(model))) {
    stop("The model is not estimable because its terms are rank deficient. Remove redundant terms or check empty factor combinations.", call.=FALSE)
  }
  if (!isTRUE(model$converged) ||
      (options$family %in% c("gaussian", "Gamma", "gammaLog") && stats::df.residual(model) <= 0L) ||
      any(!is.finite(stats::coef(model)))) {
    stop("The model is not estimable or did not converge. Check redundant terms, sparse categories, separation, and the number of complete observations.", call.=FALSE)
  }
  if (inherits(model, "negbin") && (!is.finite(model$theta) || model$theta <= 0)) {
    stop("Negative Binomial dispersion could not be estimated.", call.=FALSE)
  }
  list(model=model, data=eligible, warnings=warnings$messages, omitted=nrow(data)-nrow(eligible))
}

jamovi_glm_generalized_plot = function(state, x=NULL, panels=NULL, group=NULL, style="auto", se=TRUE, scale="probability") {
  model = state$model
  formula = jamovi_model_layout(list(model), state$data, x, panels, group)
  if (!length(all.vars(formula)[-1])) {
    stop("Choose exactly one fitted-model predictor for the plot x variable; this model has no predictors.", call.=FALSE)
  }
  xvar = flexplot_axis_given(formula)$axis[1]
  display = jamovi_glm_visualization(state$data, xvar, model$family$family == "binomial", style, scale)
  logit = model$family$family == "binomial" && display$scale == "logit"
  if (display$overlay) {
    plot = visualize_binomial_model(model, formula, state$data, n_bins=10, overlay_type="dot", se=se, scale=if(logit) "logit" else "probability")
    return(if(logit) jamovi_glm_logit_display(plot,all.vars(formula)[1],transform=FALSE) else plot)
  }
  plot = compare_fits(formula, state$data, model1=model, pred.type="response", report.se=se)
  if (model$family$family == "Gamma" && model$family$link == "inverse") {
    for (layer in plot$layers) {
      if (!is.data.frame(layer$data) || !"prediction" %in% names(layer$data)) next
      variables = all.vars(formula)[-1]
      coordinates = unique(layer$data[, variables, drop=FALSE])
      for (i in seq_len(nrow(coordinates))) {
        profile = state$data
        for (variable in variables) {
          value = coordinates[[variable]][i]
          if (is.numeric(profile[[variable]]) && is.factor(value)) value = as.numeric(as.character(value))
          profile[[variable]] = value
        }
        eta = stats::predict(model, newdata=profile, type="link")
        if (any(!is.finite(eta) | eta <= 0)) {
          stop("The requested prediction profiles leave the positive domain of the Gamma inverse link. Reconsider the model or plotting layout.", call.=FALSE)
        }
      }
      break
    }
  }
  jamovi_model_intervals(plot, se=se, model=model)
}

glmgeneralizedClass = if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
  "glmgeneralizedClass", inherit=glmgeneralizedBase,
  private=list(
    .init=function() jamovi_glm_reporting_reset(self$results),
    .run=function() {
      jamovi_glm_reporting_reset(self$results)
      if (!length(self$options$out)) {
        jamovi_diagnostic(self$results$waiting, if (!length(self$options$preds))
          "Select an outcome and predictors." else "Select an outcome variable.")
        return(invisible(NULL))
      }
      if (!length(self$options$preds)) {
        jamovi_diagnostic(self$results$waiting, "Select at least one predictor.")
        return(invisible(NULL))
      }
      if (!is.null(self$options$modelTerms) && !length(self$options$modelTerms) && !length(self$options$quadratic)) {
        jamovi_diagnostic(self$results$waiting, "No Model 1 terms are selected. Add a model term or a squared term.")
        return(invisible(NULL))
      }
      comparison = NULL
      state = tryCatch({
        if (self$options$compare) {
          comparison = jamovi_glm_generalized_comparison(self$options, self$data)
          comparison$primary
        } else jamovi_glm_generalized_fit(self$options, self$data)
      }, error=identity)
      if (inherits(state, "error")) {
        self$results$waiting$setVisible(FALSE)
        jamovi_diagnostic(self$results$explanation, paste0(
          if (self$options$compare) "Model comparison is unavailable: " else "Model 1 could not be fitted: ",
          conditionMessage(state)), error=TRUE)
        return(invisible(NULL))
      }
      if (!is.null(comparison)) {
        self$results$comparison$setVisible(TRUE)
        jamovi_glm_generalized_comparison_table(self$results$comparison, comparison)
        self$results$comparisonplot$setState(comparison)
        self$results$comparisonplot$setVisible(self$options$graphic)
        if (self$options$graphic) {
          roles = flexplot_axis_given(comparison$formula)
          guide = jamovi_glm_plot_guide(list(model=comparison$model1, models=list(comparison$model1, comparison$model2), data=comparison$data),
            roles$axis[1], roles$given[!is.na(roles$given)], style=self$options$plotStyle, scale=self$options$plotScale)
          self$results$comparisonPlotExplanation$setContent(guide)
          self$results$comparisonPlotExplanation$setVisible(nzchar(guide))
        }
      }
      model = state$model
      summary = summary(model)
      state$predictors = self$options$preds
      # Retain fit measures in state for consumers; they are not default report tables.
      state$fitStats = c(N=stats::nobs(model), `Residual df`=stats::df.residual(model),
        Deviance=model$deviance, `Null deviance`=model$null.deviance,
        `Log likelihood`=as.numeric(stats::logLik(model)), AIC=stats::AIC(model),
        BIC=stats::BIC(model), Dispersion=summary$dispersion)
      if (inherits(model, "negbin")) state$fitStats = c(state$fitStats,
        `Size (theta)`=model$theta, `SE (theta)`=model$SE.theta)
      self$results$plot$setState(state)
      jamovi_glm_estimates(self$results, state, includeP=self$options$includeP)
      self$results$waiting$setVisible(FALSE)
      self$results$coefficients$setVisible(self$options$showEstimates)
      self$results$irt$setVisible(self$options$showIRT && self$results$irt$rowCount > 0L)
      self$results$probabilities$setVisible(self$options$showProbabilities && self$results$probabilities$rowCount > 0L)
      self$results$plot$setVisible(self$options$graphic)
      if (self$options$graphic) {
        guide = tryCatch(jamovi_glm_plot_guide(state, self$options$plotX,
          self$options$plotPanels, self$options$plotGroup, self$options$plotStyle, self$options$plotScale), error=function(error) "")
        self$results$plotExplanation$setContent(guide)
        self$results$plotExplanation$setVisible(nzchar(trimws(guide)))
      }
    },
    .comparisonplot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        print(jamovi_glm_generalized_comparison_plot(image$state, self$options$se, self$options$plotStyle, self$options$plotScale))
        TRUE
      }, error=function(error) {
        jamovi_glm_plot_error(image, error)
      })
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_glm_generalized_plot(image$state, self$options$plotX, self$options$plotPanels,
          self$options$plotGroup, self$options$plotStyle, self$options$se, self$options$plotScale)
        print(plot)
        TRUE
      }, error=function(error) {
        jamovi_glm_plot_error(image, error)
      })
    }
  ))
