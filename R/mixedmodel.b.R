jamovi_mixed_escape = function(text) {
  text = gsub("&", "&amp;", text, fixed=TRUE)
  text = gsub("<", "&lt;", text, fixed=TRUE)
  gsub(">", "&gt;", text, fixed=TRUE)
}

# jamovi's one-group Gaussian mixed-model adapter. Formulas use literal symbols.
jamovi_mixed_formula = function(options, data) {
  if (length(options$cluster) != 1L || !options$cluster %in% names(data) ||
      identical(options$out, options$cluster)) {
    stop("Select exactly one cluster variable, different from the outcome.", call.=FALSE)
  }
  slopes = options$randomSlopes
  if (any(!slopes %in% options$preds) || anyDuplicated(slopes)) {
    stop("Random slopes must use selected predictor variables.", call.=FALSE)
  }
  if (!options$randomIntercept && !length(slopes)) {
    stop("Select a random intercept or at least one random slope.", call.=FALSE)
  }
  terms = options$modelTerms
  if (identical(options$interactions, FALSE) && !is.null(terms)) terms = terms[lengths(terms) == 1L]
  quadratic = if (identical(options$polynomials, FALSE)) NULL else options$quadratic
  fixed = jamovi_glm_generalized_formula(options, data, terms=terms, quadratic=quadratic)
  random = if (options$randomIntercept) 1 else 0
  for (variable in slopes) random = call("+", random, as.name(variable))
  formula = fixed
  formula[[3]] = call("+", fixed[[3]], call("(", call("|", random, as.name(options$cluster))))
  list(formula=formula, fixed=fixed, random=call("|", random, as.name(options$cluster)))
}

jamovi_mixed_fit = function(options, data) {
  specification = jamovi_mixed_formula(options, data)
  variables = all.vars(specification$formula)
  eligible = data[stats::complete.cases(data[, variables, drop=FALSE]), , drop=FALSE]
  if (!nrow(eligible)) stop("No complete observations are available for this model.", call.=FALSE)
  outcome = eligible[[options$out]]
  if (!is.numeric(outcome) || any(!is.finite(outcome)) || length(unique(outcome)) < 2L) {
    stop("Mixed Model requires a finite numeric outcome that varies.", call.=FALSE)
  }
  eligible[[options$cluster]] = droplevels(factor(eligible[[options$cluster]]))
  if (nlevels(eligible[[options$cluster]]) < 2L) stop("The cluster variable needs at least two observed groups.", call.=FALSE)
  warnings = new.env(parent=emptyenv())
  warnings$messages = character()
  model = withCallingHandlers(lme4::lmer(specification$formula, data=eligible,
    REML=options$estimation == "REML", na.action=stats::na.fail),
    warning=function(warning) {
      warnings$messages = unique(c(warnings$messages, conditionMessage(warning)))
      invokeRestart("muffleWarning")
    }, message=function(message) {
      warnings$messages = unique(c(warnings$messages, conditionMessage(message)))
      invokeRestart("muffleMessage")
    })
  if (length(attr(lme4::getME(model, "X"), "col.dropped"))) {
    stop("The fixed effects are rank deficient. Remove redundant terms; no simplified model is reported.", call.=FALSE)
  }
  warnings$messages = unique(trimws(c(warnings$messages, model@optinfo$conv$lme4$messages)))
  warnings$messages = warnings$messages[!grepl("boundary \\(singular\\) fit", warnings$messages)]
  list(model=model, data=eligible, specification=specification, cluster=options$cluster,
    warnings=warnings$messages, singular=lme4::isSingular(model), omitted=nrow(data)-nrow(eligible))
}

jamovi_mixed_reset = function(results) {
  for (item in results$items) {
    item$setVisible(FALSE)
    item$setError("")
    item$setStatus("complete")
    if (inherits(item, "Table")) {
      item$deleteRows()
      for (key in names(item$notes)) item$setNote(key, NULL)
    }
    if (inherits(item, "Html")) item$setContent("")
  }
  results$plot$setState(NULL)
  results$comparisonplot$setState(NULL)
  results$waiting$setContent("<p>Select a numeric outcome and one cluster variable.</p>")
  results$waiting$setVisible(TRUE)
}

jamovi_mixed_report = function(results, state, includeP, showCorrelations=FALSE) {
  coefficients = summary(state$model)$coefficients
  fixed = data.frame(term=rownames(coefficients), estimate=coefficients[,1], se=coefficients[,2],
    lower=coefficients[,1]-stats::qnorm(.975)*coefficients[,2],
    upper=coefficients[,1]+stats::qnorm(.975)*coefficients[,2], t=coefficients[,3], row.names=NULL)
  column = results$fixed$getColumn("p")
  column$visible = includeP
  jamovi_glm_table_rows(results$fixed, fixed)
  results$fixed$setNote("intervals", "95% normal-Wald confidence intervals for fixed effects: estimate +/- 1.96 SE. These are not prediction intervals.")
  if (includeP) results$fixed$setNote("p", "P-values are unavailable from the current lme4 fit; no denominator degrees of freedom or p-value approximation is imposed.")
  components = as.data.frame(lme4::VarCorr(state$model))
  components = components[is.na(components$var2), , drop=FALSE]
  random = data.frame(group=components$grp, term=components$var1,
    variance=components$vcov, sd=components$sdcor)
  random$term[random$group == "Residual"] = "Residual"
  jamovi_glm_table_rows(results$random, random)
  for (column in results$correlations$columns) column$visible = identical(column$name, "term")
  correlation = attr(lme4::VarCorr(state$model)[[1]], "correlation")
  if (showCorrelations && nrow(correlation) > 1L) {
    diag(correlation) = 1
    for (i in seq_len(ncol(correlation))) {
      results$correlations$addColumn(paste0("c",i), title=colnames(correlation)[i], type="number")
    }
    values = as.data.frame(correlation)
    names(values) = paste0("c",seq_len(ncol(correlation)))
    values = cbind(data.frame(term=rownames(correlation)),values)
    jamovi_glm_table_rows(results$correlations,values)
    results$correlations$setVisible(TRUE)
  }
  diagnostics = character()
  if (isTRUE(state$singular)) diagnostics = c(diagnostics,
    "Singular fit detected. The estimated random-effects covariance matrix is rank-deficient. Interpret random-effect estimates cautiously.")
  if (length(state$warnings)) diagnostics = c(diagnostics, paste0("Convergence / fitting diagnostic: ",state$warnings))
  if (length(diagnostics)) {
    results$diagnostics$setContent(paste0("<p><strong>",paste(jamovi_mixed_escape(diagnostics),collapse="</strong></p><p><strong>"),"</strong></p>"))
    results$diagnostics$setVisible(TRUE)
  }
  formula_text = paste(deparse(stats::formula(state$model)), collapse=" ")
  results$specification$setContent(paste0("<p><strong>Model:</strong> ", jamovi_mixed_escape(formula_text),
    " (", if (lme4::isREML(state$model)) "REML" else "ML", ")</p>"))
  information = paste0("<p>", stats::nobs(state$model), " complete observations; ",
    nlevels(state$data[[state$cluster]]), " groups; ", state$omitted, " observations omitted.</p>")

  results$explanation$setContent(information)
}

jamovi_mixed_plot = function(state, x=NULL, panels=NULL, group=NULL, type="cluster", clusters=3) {
  layout = list(x=x,panels=panels,group=group,type=type,clusters=clusters)
  if (!is.null(state$displayPlot) && identical(state$displayLayout,layout)) return(state$displayPlot)
  if (!type %in% c("population", "cluster", "cas")) stop("Choose population-level, cluster-specific, or cluster-adjusted visualization.", call.=FALSE)
  if (length(clusters) != 1L || !is.finite(clusters) || clusters < 1 || clusters != as.integer(clusters)) {
    stop("The number of displayed clusters must be a positive integer.", call.=FALSE)
  }
  if (type != "cas" && !identical(extract_random_term(state$model), state$cluster)) {
    stop("The existing mixed-model plot path cannot handle this grouping-variable name. Use a simple grouping name for visualization.", call.=FALSE)
  }
  predictors = setdiff(all.vars(stats::formula(state$model))[-1], state$cluster)
  if (!length(predictors)) stop("This intercept-only model has no predictor for an x-axis visualization.", call.=FALSE)
  if (is.null(x)) x = predictors[1]
  if (type == "cluster" && is.null(panels) && is.null(group)) panels = state$cluster
  if (type == "cluster" && !state$cluster %in% c(x, panels, group)) {
    stop("For cluster-specific predictions, place the grouping variable in panels or color / symbol.", call.=FALSE)
  }
  if (identical(x, state$cluster)) stop("Use a model predictor on x; place the grouping variable in panels or color / symbol.", call.=FALSE)
  formula = jamovi_model_layout(list(state$model), state$data, x, panels, group)
  if (type == "cas") {
    # Reuse CAS adjustment, model curves, and supported layouts on the original fit.
    plot = cluster_adjusted_scatter(formula, object=state$model, adjust_re="all", smooth=FALSE)
    return(plot + ggplot2::labs(subtitle="Cluster-adjusted scatterplot (estimated random effects removed)"))
  }
  if (type == "population") {
    plot = compare_fits(formula, state$data, model1=state$model, re=FALSE,
      clusters=clusters, report.se=FALSE, pred.type="response")
  } else {
    # Select the existing conditional stream before rendering: otherwise the
    # additional population stream can be mistaken for a two-model comparison.
    prediction = compare_fits(formula, state$data, model1=state$model, re=TRUE,
      clusters=clusters, report.se=FALSE, pred.type="response", return.preds=TRUE)
    prediction = prediction[prediction$model == "random effects", , drop=FALSE]
    observed = state$data[state$data[[state$cluster]] %in% unique(prediction[[state$cluster]]), , drop=FALSE]
    attr(prediction, "flexplot_comparison") = TRUE
    plot = flexplot(formula, observed, prediction=prediction, suppress_smooth=TRUE, se=FALSE)
    plot = plot + return_lims_geom(all.vars(formula)[1], state$data, state$model)
  }
  label = if (type == "cluster") "Cluster-specific predictions (estimated random effects included)" else
    "Population-level predictions (random effects excluded)"
  plot + ggplot2::labs(subtitle=label)
}

mixedmodelClass = if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
  "mixedmodelClass", inherit=mixedmodelBase, private=list(
    .modelState=NULL,
    .comparisonState=NULL,
    .init=function() {
      if (is.null(private$.modelState)) private$.modelState = self$results$plot$state
      jamovi_mixed_reset(self$results)
    },
    .run=function() {
      previous = private$.modelState
      private$.modelState = NULL
      jamovi_mixed_sync_labels(self$options)
      jamovi_mixed_reset(self$results)
      if (!length(self$options$out) || !length(self$options$cluster)) return(invisible(NULL))
      fit_options = lapply(c("out","preds","interactions","polynomials","modelTerms","quadratic","cluster",
        "randomIntercept","randomSlopes","estimation"),self$options$get)
      if (!is.null(previous) && identical(previous$fitOptions,fit_options) && identical(previous$inputData,self$data)) {
        state = previous
      } else state = tryCatch(jamovi_mixed_fit(self$options, self$data), error=identity)
      self$results$waiting$setVisible(FALSE)
      if (inherits(state, "error")) {
        self$results$explanation$setVisible(TRUE)
        self$results$explanation$setError(conditionMessage(state))
        return(invisible(NULL))
      }
      state$fitOptions = fit_options
      state$inputData = self$data
      plot_failed = FALSE
      if (self$options$graphic && !self$options$compare) {
        layout = list(x=self$options$plotX,panels=self$options$plotPanels,group=self$options$plotGroup,
          type=self$options$predictionType,clusters=self$options$clusters)
        plot = tryCatch(do.call(jamovi_mixed_plot,c(list(state=state),layout)),error=identity)
        if (!inherits(plot,"error")) {
          state$displayPlot = plot
          state$displayLayout = layout
          jamovi_mixed_sync_labels(self$options,plot,layout)
        } else {
          state$displayPlot = NULL
          plot_failed = TRUE
          self$results$plot$setError(conditionMessage(plot))
        }
      }
      jamovi_mixed_report(self$results, state, self$options$includeP, self$options$showCorrelations)
      for (item in c("specification", "fixed", "random", "explanation")) self$results[[item]]$setVisible(TRUE)
      private$.modelState = state
      self$results$plot$setState(if (plot_failed) NULL else state)
      self$results$plot$setVisible(self$options$graphic && !self$options$compare)
      self$results$plotExplanation$setVisible(self$options$graphic && !self$options$compare)
      self$results$plotExplanation$setContent(paste0("<p>",
        if (self$options$predictionType == "cas")
          "Cluster-adjusted scatterplot: the existing CAS implementation removes all estimated random effects from the fitted observations and draws the original model's fixed-effects curve. All eligible fitted observations are shown; the displayed-cluster count does not apply. Displayed moderators use CAS representative values, and undisplayed covariates are observation-weighted marginalized." else paste0(
          if (self$options$predictionType == "cluster") "Cluster-specific predictions include the original model's estimated random effects." else
            "Population-level predictions exclude random effects.",
          " Curves use the existing marginal prediction method; undisplayed covariates retain the full eligible reference population.",
          " Up to ", self$options$clusters, " randomly selected clusters are displayed."),
        " No prediction confidence bands are calculated.</p>"))
      if (!self$options$compare) return(invisible(NULL))
      comparison_options = c(fit_options,lapply(c("comparisonFixed","comparisonTerms","comparisonInteractions","comparisonPolynomials",
        "comparisonQuadratic","comparisonRandomIntercept","comparisonRandomSlopes","comparisonEstimation"),self$options$get))
      comparison = private$.comparisonState
      if (is.null(comparison) || !identical(comparison$fitOptions,comparison_options) ||
          !identical(comparison$inputData,self$data)) {
        comparison = tryCatch(jamovi_mixed_comparison(self$options,self$data),error=identity)
      }
      if (inherits(comparison,"error")) {
        private$.comparisonState = NULL
        self$results$comparison$setVisible(TRUE)
        self$results$comparison$setError(conditionMessage(comparison))
        return(invisible(NULL))
      }
      comparison$fitOptions = comparison_options
      comparison$inputData = self$data
      jamovi_mixed_comparison_report(self$results,comparison,self$options$includeP)
      self$results$comparisonplot$setSize(if (self$options$predictionType == "cas") 1100 else 700,450)
      comparison_plot_failed = FALSE
      if (self$options$graphic) {
        layout = list(x=self$options$plotX,panels=self$options$plotPanels,group=self$options$plotGroup,
          type=self$options$predictionType,clusters=self$options$clusters,casGhost=self$options$casGhost)
        plot = tryCatch(do.call(jamovi_mixed_comparison_plot,c(list(state=comparison),layout)),error=identity)
        if (inherits(plot,"error")) {
          comparison$displayPlot = NULL
          comparison_plot_failed = TRUE
          self$results$comparisonplot$setError(conditionMessage(plot))
        } else {
          comparison$displayPlot = plot
          comparison$displayLayout = layout
          jamovi_mixed_sync_labels(self$options,plot,layout)
        }
      }
      private$.comparisonState = comparison
      self$results$comparisonplot$setState(if (comparison_plot_failed) NULL else comparison)
      self$results$comparisonplot$setVisible(self$options$graphic)
      self$results$plotExplanation$setContent("")
      self$results$plotExplanation$setVisible(FALSE)
    },
    .comparisonplot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_mixed_comparison_plot(image$state,self$options$plotX,self$options$plotPanels,
          self$options$plotGroup,self$options$predictionType,self$options$clusters,self$options$casGhost)
        print(jamovi_mixed_apply_labels(plot,self$options$plotLabels))
        TRUE
      },error=function(error) {
        # jmvcore otherwise labels a failed image with non-null state as rendering.
        image$setState(NULL)
        image$setError(conditionMessage(error))
        FALSE
      })
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_mixed_plot(image$state, self$options$plotX, self$options$plotPanels,
          self$options$plotGroup, self$options$predictionType, self$options$clusters)
        print(jamovi_mixed_apply_labels(plot,self$options$plotLabels))
        TRUE
      }, error=function(error) {
        # jmvcore otherwise labels a failed image with non-null state as rendering.
        image$setState(NULL)
        image$setError(conditionMessage(error))
        FALSE
      })
    }
  ))
