jamovi_mixed_escape = function(text) {
  jamovi_html_escape(text)
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
  fixed = jamovi_fixed_formula(options, data, terms=terms, quadratic=quadratic)
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
    jamovi_result_clear(item)
    if (item$name %in% c("fixed","fixed2")) item$getColumn("p")$setVisible(FALSE)
    if (inherits(item, "Image")) item$.setPath(NULL)
  }
  jamovi_plot_clear_states(results, c("plot", "comparisonplot"))
  results$waiting$setContent("<p>Select a numeric outcome and one cluster variable.</p>")
  results$waiting$setVisible(TRUE)
}

jamovi_mixed_tables = function(results, state, includeP, showCorrelations=FALSE, includeCI=TRUE, notes=TRUE) {
  coefficients = summary(state$model)$coefficients
  fixed = data.frame(term=rownames(coefficients), estimate=coefficients[,1], se=coefficients[,2],
    lower=coefficients[,1]-stats::qnorm(.975)*coefficients[,2],
    upper=coefficients[,1]+stats::qnorm(.975)*coefficients[,2], t=coefficients[,3], row.names=NULL)
  column = results$fixed$getColumn("p")
  column$setVisible(FALSE)
  jamovi_glm_table_rows(results$fixed, fixed)
  for (name in c("lower","upper")) results$fixed$getColumn(name)$setVisible(includeCI)
  results$fixed$setNote("intervals",NULL)
  if (includeCI && notes) results$fixed$setNote("intervals", "95% normal-Wald confidence intervals.")
  results$fixed$setNote("p",NULL)
  components = as.data.frame(lme4::VarCorr(state$model))
  components = components[is.na(components$var2), , drop=FALSE]
  random = data.frame(group=components$grp, term=components$var1,
    variance=components$vcov, sd=components$sdcor)
  random$term[random$group == "Residual"] = "Residual"
  jamovi_glm_table_rows(results$random, random)
  for (column in results$correlations$columns) column$visible = identical(column$name, "term")
  correlation = attr(lme4::VarCorr(state$model)[[1]], "correlation")
  if (showCorrelations && nrow(correlation) > 1L) {
    diag(correlation)[is.finite(diag(correlation))] = 1
    for (i in seq_len(ncol(correlation))) {
      results$correlations$addColumn(paste0("c",i), title=colnames(correlation)[i], type="number")
    }
    values = as.data.frame(correlation)
    names(values) = paste0("c",seq_len(ncol(correlation)))
    values = cbind(data.frame(term=rownames(correlation)),values)
    jamovi_glm_table_rows(results$correlations,values)
    results$correlations$setVisible(TRUE)
  }
}

jamovi_mixed_diagnostics = function(state) {
  diagnostics = character()
  if (isTRUE(state$singular)) diagnostics = c(diagnostics,
    "Singular fit detected. The estimated random-effects covariance matrix is rank-deficient. Interpret random-effect estimates cautiously.")
  if (length(state$warnings)) diagnostics = c(diagnostics, paste0("Convergence / fitting diagnostic: ",state$warnings))
  diagnostics
}

jamovi_mixed_report = function(results, state, includeP, showCorrelations=FALSE, includeCI=TRUE) {
  jamovi_mixed_tables(results,state,includeP,showCorrelations,includeCI)
  diagnostics = jamovi_mixed_diagnostics(state)
  if (length(diagnostics)) {
    results$diagnostics$setContent(paste0("<p><strong>",paste(jamovi_mixed_escape(diagnostics),collapse="</strong></p><p><strong>"),"</strong></p>"))
    results$diagnostics$setVisible(TRUE)
  }
  formula_text = paste(deparse(stats::formula(state$model)), collapse=" ")
  information = paste0("<p>", stats::nobs(state$model), " complete observations; ",
    nlevels(state$data[[state$cluster]]), " groups; ", state$omitted, " observations omitted.</p>")
  results$specification$setContent(paste0("<p><strong>Model:</strong> ", jamovi_mixed_escape(formula_text),
    " (", if (lme4::isREML(state$model)) "REML" else "ML", ")</p>", information))
  invisible(information)
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

# Keep cached plots and sampled clusters intact; ordinary ghosts copy that stream.
jamovi_mixed_display = function(state, options) {
  plot = jamovi_mixed_plot(state,options$plotX,options$plotPanels,options$plotGroup,
    options$predictionType,options$clusters)
  if (!isTRUE(options$ghostLines)) return(plot)
  if (options$predictionType=="cas") {
    attr(plot,"jamovi_ghost_note") = "Ghost lines unavailable. CAS uses adjusted observations and a separate fixed-effects curve; ordinary supplied-prediction ghosts are not supported."
    return(plot)
  }
  predictors = setdiff(all.vars(stats::formula(state$model))[-1],state$cluster)
  x = if (length(options$plotX)) options$plotX else predictors[1]
  panels = options$plotPanels
  if (options$predictionType=="cluster" && is.null(panels) && is.null(options$plotGroup)) panels = state$cluster
  formula = jamovi_model_layout(list(state$model),state$data,x,panels,options$plotGroup)
  jamovi_model_reference_ghosts(plot,formula,TRUE)
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
      jamovi_output_migrate(self$options, "mixedmodel")
      jamovi_plot_editor_migrate(self$options, "mixedmodel")
      previous = private$.modelState
      private$.modelState = NULL
      jamovi_mixed_sync_labels(self$options)
      jamovi_mixed_reset(self$results)
      if (!length(self$options$out) || !length(self$options$cluster)) return(invisible(NULL))
      fit_options = lapply(c("out","preds","interactions","polynomials","modelTerms","quadratic","cluster",
        "randomIntercept","randomSlopes","estimation"),self$options$get)
      self$results$waiting$setVisible(FALSE)
      comparison = NULL
      if (self$options$compare) {
        comparison_options = c(fit_options,lapply(c("comparisonFixed","comparisonTerms","comparisonInteractions","comparisonPolynomials",
          "comparisonQuadratic","comparisonRandomIntercept","comparisonRandomSlopes","comparisonEstimation"),self$options$get))
        comparison = private$.comparisonState
        if (is.null(comparison) || !identical(comparison$samplePolicy,"common") || !identical(comparison$fitOptions,comparison_options) ||
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
        state = comparison$first
        state$samplePolicy = "common"
      } else {
        if (!is.null(previous) && identical(previous$samplePolicy,"standalone") &&
            identical(previous$fitOptions,fit_options) && identical(previous$inputData,self$data)) {
          state = previous
        } else state = tryCatch(jamovi_mixed_fit(self$options,self$data),error=identity)
        if (!inherits(state,"error")) state$samplePolicy = "standalone"
      }
      if (inherits(state, "error")) {
        jamovi_diagnostic(self$results$explanation,
          paste0("Mixed Model could not be fitted: ", conditionMessage(state)), error=TRUE)
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
          display = jamovi_mixed_display(state,self$options)
          jamovi_mixed_sync_labels(self$options,plot,layout)
        } else {
          state$displayPlot = NULL
          plot_failed = TRUE
          self$results$plot$setError(conditionMessage(plot))
        }
      }
      information = jamovi_mixed_report(self$results, state, self$options$includeP, self$options$showCorrelations, self$options$includeCI)
      self$results$specification$setVisible(TRUE)
      jamovi_output_visible(self$results, c("fixed", "random"), self$options$estimates)
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
        " No prediction confidence bands are calculated.</p>",
        if (exists("display",inherits=FALSE))
          jamovi_model_ghost_note(jamovi_mixed_apply_labels(display,self$options$plotLabels))))
      self$results$plotExplanation$setContent(jamovi_plot_compact_notes(self$results$plotExplanation$content))
      if (!self$options$compare) return(invisible(NULL))
      jamovi_mixed_tables(list(fixed=self$results$fixed2,random=self$results$random2,
        correlations=self$results$correlations2),comparison$second,self$options$includeP,
        self$options$showCorrelations,self$options$includeCI,notes=FALSE)
      jamovi_output_visible(self$results, c("fixed2", "random2"), self$options$estimates)
      for (name in c("model1Heading","model2Heading","comparisonHeading")) {
        label = switch(name,model1Heading="Model 1",model2Heading="Model 2",comparisonHeading="Model Comparison")
        self$results[[name]]$setContent(paste0("<h3>",label,"</h3>"))
        self$results[[name]]$setVisible(if (name=="comparisonHeading")
          self$options$comparisonGraphic || self$options$comparisonStatistics else
          self$options$estimates || self$results[[if (name=="model1Heading") "correlations" else "correlations2"]]$visible)
      }
      diagnostics = unlist(lapply(seq_len(2),function(i) {
        messages = jamovi_mixed_diagnostics(comparison[[c("first","second")[i]]])
        if (length(messages)) paste0("Model ",i,": ",messages) else character()
      }))
      self$results$diagnostics$setContent(if (length(diagnostics)) paste0("<p><strong>",
        paste(jamovi_mixed_escape(diagnostics),collapse="</strong></p><p><strong>"),"</strong></p>") else "")
      self$results$diagnostics$setVisible(length(diagnostics)>0L)
      jamovi_mixed_comparison_report(self$results,comparison,self$options$includeP,information)
      self$results$comparison$setVisible(self$options$comparisonStatistics)
      self$results$comparisonplot$setSize(if (self$options$predictionType == "cas") 1100 else 700,450)
      comparison_plot_failed = FALSE
      if (self$options$comparisonGraphic) {
        layout = list(x=self$options$plotX,panels=self$options$plotPanels,group=self$options$plotGroup,
          type=self$options$predictionType,clusters=self$options$clusters,casGhost=self$options$casGhost)
        plot = tryCatch(do.call(jamovi_mixed_comparison_plot,c(list(state=comparison),layout)),error=identity)
        if (inherits(plot,"error")) {
          comparison$displayPlot = NULL
          comparison_plot_failed = TRUE
          self$results$comparisonplot$setError(conditionMessage(plot))
          jamovi_diagnostic(self$results$comparisonPlotNote,
            paste("Comparison plot unavailable:", conditionMessage(plot)))
        } else {
          comparison$displayPlot = plot
          comparison$displayLayout = layout
          note = attr(plot,"jamovi_comparison_note")
          if (!is.null(note)) jamovi_diagnostic(self$results$comparisonPlotNote,note)
          jamovi_mixed_sync_labels(self$options,plot,layout)
        }
      }
      private$.comparisonState = comparison
      self$results$comparisonplot$setState(if (comparison_plot_failed) NULL else comparison)
      self$results$comparisonplot$setVisible(self$options$comparisonGraphic)
      if (self$options$comparisonGraphic && !comparison_plot_failed &&
          self$options$predictionType == "cas" && self$options$casGhost &&
          isTRUE(attr(comparison$displayPlot,"jamovi_cas_ghost"))) {
        self$results$comparisonPlotNote$setContent(jamovi_plot_note_paragraph("Note. Dashed line in Model 1 represents Model 2's fixed-effects predictions."))
        self$results$comparisonPlotNote$setVisible(TRUE)
      }
      self$results$plotExplanation$setContent("")
      self$results$plotExplanation$setVisible(FALSE)
    },
    .comparisonplot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_mixed_comparison_plot(image$state,self$options$plotX,self$options$plotPanels,
          self$options$plotGroup,self$options$predictionType,self$options$clusters,self$options$casGhost)
        note = attr(plot,"jamovi_comparison_note")
        if (!is.null(note)) jamovi_diagnostic(self$results$comparisonPlotNote,note)
        print(jamovi_mixed_apply_labels(plot,self$options$plotLabels))
        TRUE
      },error=function(error) {
        # jmvcore otherwise labels a failed image with non-null state as rendering.
        image$setState(NULL)
        image$setError(conditionMessage(error))
        jamovi_diagnostic(self$results$comparisonPlotNote,
          paste("Comparison plot unavailable:",conditionMessage(error)))
        FALSE
      })
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        plot = jamovi_mixed_display(image$state,self$options)
        plot = jamovi_mixed_apply_labels(plot,self$options$plotLabels)
        print(plot)
        TRUE
      }, error=function(error) {
        # jmvcore otherwise labels a failed image with non-null state as rendering.
        image$setState(NULL)
        image$setError(conditionMessage(error))
        FALSE
      })
    }
  ))
