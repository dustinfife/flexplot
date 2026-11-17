jamovi_mixed_escape = function(text) {
  text = gsub("&", "&amp;", text, fixed=TRUE)
  text = gsub("<", "&lt;", text, fixed=TRUE)
  gsub(">", "&gt;", text, fixed=TRUE)
}

# jamovi's one-group Gaussian mixed-model adapter. Formulas use literal symbols.
jamovi_mixed_formula = function(options, data) {
  if (length(options$cluster) != 1L || !options$cluster %in% names(data) ||
      identical(options$out, options$cluster)) {
    stop("Select exactly one grouping variable, different from the outcome.", call.=FALSE)
  }
  slopes = options$randomSlopes
  if (any(!slopes %in% options$preds) || anyDuplicated(slopes)) {
    stop("Random slopes must use selected predictor variables.", call.=FALSE)
  }
  if (!options$randomIntercept && !length(slopes)) {
    stop("Select a random intercept or at least one random slope.", call.=FALSE)
  }
  fixed = jamovi_glm_generalized_formula(options, data)
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
  if (nlevels(eligible[[options$cluster]]) < 2L) stop("The grouping variable needs at least two observed groups.", call.=FALSE)
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
  if (lme4::isSingular(model)) warnings$messages = unique(c(warnings$messages,
    "Singular fit: some random-effect variances or correlations are on a boundary. The requested structure has not been simplified."))
  warnings$messages = unique(c(warnings$messages, model@optinfo$conv$lme4$messages))
  list(model=model, data=eligible, specification=specification, cluster=options$cluster,
    warnings=warnings$messages, omitted=nrow(data)-nrow(eligible))
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
  results$waiting$setContent("<p>Select a numeric outcome and one grouping variable.</p>")
  results$waiting$setVisible(TRUE)
}

jamovi_mixed_report = function(results, state, includeP) {
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
  random = data.frame(group=components$grp, term=components$var1, term2=components$var2,
    variance=ifelse(is.na(components$var2), components$vcov, NA_real_),
    sd=ifelse(is.na(components$var2), components$sdcor, NA_real_),
    correlation=ifelse(is.na(components$var2), NA_real_, components$sdcor))
  random$term[random$group == "Residual"] = "Residual"
  jamovi_glm_table_rows(results$random, random)
  results$random$setNote("correlation", "Correlations describe the jointly fitted random effects. No intervals for variance components are calculated.")
  fixed_text = paste(deparse(state$specification$fixed), collapse=" ")
  random_text = paste(deparse(state$specification$random), collapse=" ")
  results$specification$setContent(paste0("<p>Fixed: ", jamovi_mixed_escape(fixed_text), "</p><p>Random: ",
    jamovi_mixed_escape(random_text), "</p><p>Estimation: ", if (lme4::isREML(state$model)) "REML" else "ML", "</p>"))
  information = paste0("<p>", stats::nobs(state$model), " complete observations; ",
    nlevels(state$data[[state$cluster]]), " groups; ", state$omitted, " observations omitted.</p>")
  if (length(state$warnings)) information = paste0(information, "<p>",
    jamovi_mixed_escape(paste(state$warnings, collapse="; ")), "</p>")
  results$explanation$setContent(information)
}

jamovi_mixed_plot = function(state, x=NULL, panels=NULL, group=NULL, type="cluster", clusters=3) {
  if (!type %in% c("population", "cluster")) stop("Choose population-level or cluster-specific predictions.", call.=FALSE)
  if (length(clusters) != 1L || !is.finite(clusters) || clusters < 1 || clusters != as.integer(clusters)) {
    stop("The number of displayed clusters must be a positive integer.", call.=FALSE)
  }
  if (!identical(extract_random_term(state$model), state$cluster)) {
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
    .init=function() jamovi_mixed_reset(self$results),
    .run=function() {
      jamovi_mixed_reset(self$results)
      if (!length(self$options$out) || !length(self$options$cluster)) return(invisible(NULL))
      state = tryCatch(jamovi_mixed_fit(self$options, self$data), error=identity)
      self$results$waiting$setVisible(FALSE)
      if (inherits(state, "error")) {
        self$results$explanation$setVisible(TRUE)
        self$results$explanation$setError(conditionMessage(state))
        return(invisible(NULL))
      }
      jamovi_mixed_report(self$results, state, self$options$includeP)
      for (item in c("specification", "fixed", "random", "explanation")) self$results[[item]]$setVisible(TRUE)
      self$results$plot$setState(state)
      self$results$plot$setVisible(self$options$graphic)
      self$results$plotExplanation$setVisible(self$options$graphic)
      self$results$plotExplanation$setContent(paste0("<p>",
        if (self$options$predictionType == "cluster") "Cluster-specific predictions include the original model's estimated random effects." else
          "Population-level predictions exclude random effects.",
        " Curves use the existing marginal prediction method; undisplayed covariates retain the full eligible reference population.",
        " Up to ", self$options$clusters, " randomly selected clusters are displayed. No prediction confidence bands are calculated.</p>"))
    },
    .plot=function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      tryCatch({
        print(jamovi_mixed_plot(image$state, self$options$plotX, self$options$plotPanels,
          self$options$plotGroup, self$options$predictionType, self$options$clusters))
        TRUE
      }, error=function(error) {
        image$setError(conditionMessage(error))
        FALSE
      })
    }
  ))
