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

# Compatibility entry point retained for existing GLM callers.
jamovi_glm_generalized_formula = function(options, data, terms=options$modelTerms, quadratic=options$quadratic) {
  jamovi_fixed_formula(options, data, terms, quadratic)
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

jamovi_glm_generalized_render = function(state, x=NULL, panels=NULL, group=NULL, style="auto", se=TRUE, scale="probability", ghost.line=NULL, ghost.reference=NULL) {
  model = state$model
  formula = jamovi_model_layout(list(model), state$data, x, panels, group)
  if (!length(all.vars(formula)[-1])) {
    stop("Choose exactly one fitted-model predictor for the plot x variable; this model has no predictors.", call.=FALSE)
  }
  xvar = flexplot_axis_given(formula)$axis[1]
  display = jamovi_glm_visualization(state$data, xvar, model$family$family == "binomial", style, scale)
  logit = model$family$family == "binomial" && display$scale == "logit"
  if (display$overlay) {
    plot = visualize_binomial_model(model, formula, state$data, n_bins=10, overlay_type="dot", se=se, scale=if(logit) "logit" else "probability", ghost.line=ghost.line, ghost.reference=ghost.reference)
    return(if(logit) jamovi_glm_logit_display(plot,all.vars(formula)[1],transform=FALSE) else plot)
  }
  plot = compare_fits(formula, state$data, model1=model, pred.type="response", report.se=se, ghost.line=ghost.line, ghost.reference=ghost.reference)
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

jamovi_glm_label_info = function(state, options, comparison=FALSE) {
  model = if (comparison) state$model1 else state$model
  formula = if (comparison) state$formula else jamovi_model_layout(list(model),state$data,
    options$plotX,options$plotPanels,options$plotGroup)
  display = jamovi_glm_visualization(state$data,flexplot_axis_given(formula)$axis[1],
    model$family$family=="binomial",
    if (comparison) options$comparisonPlotStyle else options$plotStyle,
    if (comparison) options$comparisonPlotScale else options$plotScale)
  meaning = paste(model$family$family,model$family$link,
    if (model$family$family=="binomial") display$scale else "response",display$overlay,sep=":")
  jamovi_plot_label_context(model,formula,if (comparison) "comparison" else "model",meaning)
}

glmgeneralizedClass = if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
  "glmgeneralizedClass", inherit=glmgeneralizedBase,
  private=list(
    .init=function() jamovi_glm_reporting_reset(self$results),
    .run=function() {
      jamovi_output_migrate(self$options, "glmgeneralized")
      jamovi_plot_sync_labels(self$options)
      jamovi_plot_sync_labels(self$options,rows="comparisonPlotLabels",retained="comparisonPlotLabelStore")
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
      jamovi_model_specification(self$results$specification,
        if (is.null(comparison)) list(state$model) else list(comparison$model1, comparison$model2))
      if (!is.null(comparison)) {
        self$results$comparison$setVisible(self$options$comparisonStatistics)
        jamovi_glm_generalized_comparison_table(self$results$comparison, comparison)
        self$results$comparisonplot$setState(comparison)
        self$results$comparisonplot$setVisible(self$options$comparisonGraphic)
        if (self$options$comparisonGraphic) {
          roles = flexplot_axis_given(comparison$formula)
          guide = jamovi_glm_plot_guide(list(model=comparison$model1, models=list(comparison$model1, comparison$model2), data=comparison$data),
            roles$axis[1], roles$given[!is.na(roles$given)], style=self$options$comparisonPlotStyle, scale=self$options$comparisonPlotScale)
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
      jamovi_glm_visible_notes(self$results)
      warnings = unique(c(state$warnings,comparison$warnings))
      if (length(warnings)) jamovi_diagnostic(self$results$explanation,
        paste(warnings,collapse="; "))
      self$results$plot$setVisible(self$options$graphic)
      if (self$options$graphic) {
        guide = tryCatch(jamovi_glm_plot_guide(state, self$options$plotX,
          self$options$plotPanels, self$options$plotGroup, self$options$plotStyle, self$options$plotScale), error=function(error) "")
        self$results$plotExplanation$setContent(guide)
        self$results$plotExplanation$setVisible(nzchar(trimws(guide)))
        # Publish actual plot elements in the host options response. Rendering
        # remains in the existing callbacks; this introduces no fit cache.
        plot = suppressWarnings(tryCatch(jamovi_glm_generalized_plot(state,self$options$plotX,self$options$plotPanels,
          self$options$plotGroup,self$options$plotStyle,self$options$se,self$options$plotScale,
          ghostLines=self$options$ghostLines),error=function(error) NULL))
        if (!is.null(plot)) {
          info = jamovi_glm_label_info(state,self$options)
          jamovi_plot_sync_labels(self$options,plot,info$layout,info$context)
        }
      }
      if (!is.null(comparison) && self$options$comparisonGraphic) {
        plot = suppressWarnings(tryCatch(jamovi_glm_generalized_comparison_plot(comparison,self$options$comparisonSe,
          self$options$comparisonPlotStyle,self$options$comparisonPlotScale),error=function(error) NULL))
        if (!is.null(plot)) {
          info = jamovi_glm_label_info(comparison,self$options,TRUE)
          jamovi_plot_sync_labels(self$options,plot,info$layout,info$context,
            rows="comparisonPlotLabels",retained="comparisonPlotLabelStore")
        }
      }
    },
    .comparisonplot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_glm_generalized_comparison_plot(image$state, self$options$comparisonSe, self$options$comparisonPlotStyle, self$options$comparisonPlotScale)
        info = jamovi_glm_label_info(image$state,self$options,TRUE)
        print(jamovi_plot_apply_labels(plot,self$options$comparisonPlotLabels,info$context,info$layout))
        TRUE
      }, error=function(error) {
        jamovi_glm_plot_error(image, error)
      })
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_glm_generalized_plot(image$state, self$options$plotX, self$options$plotPanels,
          self$options$plotGroup, self$options$plotStyle, self$options$se, self$options$plotScale,
          ghostLines=self$options$ghostLines)
        guide = jamovi_glm_plot_guide(image$state,self$options$plotX,self$options$plotPanels,
          self$options$plotGroup,self$options$plotStyle,self$options$plotScale)
        note = attr(plot,"jamovi_ghost_note")
        if (length(note)) guide = paste0(guide,"<p>",jamovi_mixed_escape(note),"</p>")
        self$results$plotExplanation$setContent(guide)
        self$results$plotExplanation$setVisible(nzchar(guide))
        info = jamovi_glm_label_info(image$state,self$options)
        print(jamovi_plot_apply_labels(plot,self$options$plotLabels,info$context,info$layout))
        TRUE
      }, error=function(error) {
        jamovi_glm_plot_error(image, error)
      })
    }
  ))
