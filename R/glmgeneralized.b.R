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
    stop("The selected family requires a finite numeric outcome.", call.=FALSE)
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

jamovi_glm_generalized_fit = function(options, data) {
  formula = jamovi_glm_formula(options$out, options$preds, options$modelTerms)
  for (variable in options$quadratic) {
    if (!variable %in% options$preds || !is.numeric(data[[variable]])) {
      stop("Quadratic terms require selected numeric predictors.", call.=FALSE)
    }
    formula[[3]] = call("+", formula[[3]], call("I", call("^", as.name(variable), 2)))
  }
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
  if (!isTRUE(model$converged) || model$rank < length(stats::coef(model)) ||
      (options$family %in% c("gaussian", "Gamma", "gammaLog") && stats::df.residual(model) <= 0L) ||
      any(!is.finite(stats::coef(model)))) {
    stop("The model is not estimable or did not converge. Check redundant terms, sparse categories, separation, and the number of complete observations.", call.=FALSE)
  }
  if (inherits(model, "negbin") && (!is.finite(model$theta) || model$theta <= 0)) {
    stop("Negative Binomial dispersion could not be estimated.", call.=FALSE)
  }
  list(model=model, data=eligible, warnings=warnings$messages, omitted=nrow(data)-nrow(eligible))
}

jamovi_glm_generalized_plot = function(state, x=NULL, panels=NULL, group=NULL, style="auto", se=TRUE) {
  model = state$model
  formula = jamovi_model_layout(list(model), state$data, x, panels, group)
  if (!length(all.vars(formula)[-1])) {
    stop("Choose exactly one fitted-model predictor for the plot x variable; this model has no predictors.", call.=FALSE)
  }
  xvar = flexplot_axis_given(formula)$axis[1]
  continuous = !is.na(xvar) && xvar %in% names(state$data) && is.numeric(state$data[[xvar]]) &&
    length(unique(state$data[[xvar]][!is.na(state$data[[xvar]])])) >= 10L
  if (model$family$family == "binomial" && continuous && style == "auto") {
    plot = visualize_binomial_model(model, formula, state$data, n_bins=10, overlay_type="dot", se=se)
    return(plot)
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
    .run=function() {
      if (!length(self$options$out) || !length(self$options$preds)) return(invisible(NULL))
      state = tryCatch(jamovi_glm_generalized_fit(self$options, self$data), error=identity)
      if (inherits(state, "error")) {
        for (name in c("coefficients", "fit", "likelihood", "plot")) self$results[[name]]$setError(conditionMessage(state))
        return(invisible(NULL))
      }
      state$predictors = self$options$preds
      self$results$plot$setState(state)
      jamovi_glm_estimates(self$results, state)
      model = state$model
      summary = summary(model)
      coefficients = summary$coefficients
      uses_t = colnames(coefficients)[3] == "t value"
      critical = if (uses_t) stats::qt(.975, stats::df.residual(model)) else stats::qnorm(.975)
      for (i in seq_len(nrow(coefficients))) {
        self$results$coefficients$addRow(rowKey=i, values=list(term=rownames(coefficients)[i],
          estimate=coefficients[i, 1], se=coefficients[i, 2],
          lower=coefficients[i, 1]-critical*coefficients[i, 2],
          upper=coefficients[i, 1]+critical*coefficients[i, 2],
          statistic=coefficients[i, 3], p=coefficients[i, 4]))
      }
      scale = if (model$family$link == "identity") "outcome" else model$family$link
      note = paste("Estimates are on the", scale, "scale. Intervals and tests use",
        if (uses_t) "the residual-df t distribution." else "the normal (z) distribution.")
      self$results$coefficients$setNote("inference", note)
      if (self$options$family == "binomial") {
        response = stats::model.response(stats::model.frame(model))
        success = if (is.factor(response)) levels(response)[2] else "1"
        self$results$coefficients$setNote("success", paste("Success event:", success))
      }
      measures = c(N=stats::nobs(model), `Residual df`=stats::df.residual(model),
        Deviance=model$deviance, `Null deviance`=model$null.deviance,
        `Log likelihood`=as.numeric(stats::logLik(model)), AIC=stats::AIC(model),
        BIC=stats::BIC(model), Dispersion=summary$dispersion)
      if (inherits(model, "negbin")) measures = c(measures, `Size (theta)`=model$theta,
                                                              `SE (theta)`=model$SE.theta)
      for (i in seq_along(measures)) {
        table = if (names(measures)[i] %in% c("Log likelihood", "AIC", "BIC")) self$results$likelihood else self$results$fit
        table$addRow(rowKey=i, values=list(measure=names(measures)[i], value=unname(measures[i])))
      }
      self$results$fit$setNote("family", paste("Family:", model$family$family, " Link:", model$family$link))
      self$results$fit$setNote("missing", paste(state$omitted, "observations omitted because model variables were missing."))
      if (length(state$warnings)) self$results$fit$setNote("warnings", paste("Model warnings:", paste(state$warnings, collapse="; ")))
      if (self$options$graphic) {
        guide = tryCatch(jamovi_glm_plot_guide(state, self$options$plotX,
          self$options$plotPanels, self$options$plotGroup, self$options$plotStyle), error=function(error) "")
        self$results$plotExplanation$setContent(guide)
      }
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_glm_generalized_plot(image$state, self$options$plotX, self$options$plotPanels,
          self$options$plotGroup, self$options$plotStyle, self$options$se)
        print(plot)
        TRUE
      }, error=function(error) {
        image$setError(conditionMessage(error))
        FALSE
      })
    }
  ))
