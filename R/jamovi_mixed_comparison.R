# Comparison fits are separate from the original single-model estimates.
jamovi_mixed_second_options = function(options) {
  list(out=options$out, preds=options$preds, cluster=options$cluster,
    modelTerms=options$comparisonTerms, interactions=options$comparisonInteractions,
    polynomials=options$comparisonPolynomials, quadratic=options$comparisonQuadratic,
    randomIntercept=options$comparisonRandomIntercept, randomSlopes=options$comparisonRandomSlopes,
    estimation=options$comparisonEstimation)
}

jamovi_mixed_ml_copy = function(model) {
  if (!lme4::isREML(model)) return(list(model=model,warnings=character()))
  messages = new.env(parent=emptyenv()); messages$value = character()
  fitted = withCallingHandlers(lme4::refitML(model),
    warning=function(warning) {
      messages$value = c(messages$value,conditionMessage(warning)); invokeRestart("muffleWarning")
    }, message=function(message) {
      messages$value = c(messages$value,conditionMessage(message)); invokeRestart("muffleMessage")
    })
  messages$value = unique(trimws(messages$value))
  messages$value = messages$value[!grepl("boundary \\(singular\\) fit",messages$value)]
  list(model=fitted,warnings=messages$value)
}

jamovi_mixed_converged = function(model) {
  convergence = model@optinfo$conv
  messages = convergence$lme4$messages
  messages = messages[!grepl("boundary \\(singular\\) fit",messages)]
  !length(messages) && (is.null(convergence$opt) || all(convergence$opt == 0))
}

jamovi_mixed_comparison_statistics = function(models) {
  copies = lapply(models,jamovi_mixed_ml_copy)
  fitted = lapply(copies,`[[`,"model")
  converged = all(vapply(fitted,jamovi_mixed_converged,logical(1)))
  singular = any(vapply(fitted,lme4::isSingular,logical(1)))
  result = list(models=fitted,aic=rep(NA_real_,2),bic=rep(NA_real_,2),bayes=rep(NA_real_,2),
    lr=NA_real_,df=NA_real_,p=NA_real_,random_p_unavailable=FALSE,singular=singular,warnings=unique(unlist(lapply(copies,`[[`,"warnings"))))
  if (!converged) {
    result$reason = "Comparison statistics unavailable: at least one ML fit has convergence diagnostics."
    return(result)
  }
  result$aic = vapply(fitted,stats::AIC,numeric(1))
  result$bic = vapply(fitted,stats::BIC,numeric(1))
  if (!singular) {
    bayes = bf.bic(fitted[[1]],fitted[[2]])
    if (is.finite(bayes) && bayes > 0 && is.finite(1/bayes)) result$bayes = c(bayes,1/bayes)
  }
  random_same = jamovi_mixed_same_random_design(fitted[[1]],fitted[[2]])
  matrix = lapply(fitted,lme4::getME,name="X")
  ranks = vapply(matrix,ncol,integer(1))
  likelihoods = lapply(fitted,stats::logLik)
  parameters = vapply(likelihoods,attr,integer(1),which="df")
  full = which.max(parameters); reduced = 3L-full
  fixed_nested = qr(cbind(matrix[[full]],matrix[[reduced]]))$rank == ranks[full]
  random_nested = jamovi_mixed_random_subset(fitted[[reduced]],fitted[[full]])
  if (!fixed_nested || !random_nested || parameters[full] == parameters[reduced]) {
    result$reason = "LRT unavailable: models are nonnested or equivalent."
    return(result)
  }
  df = parameters[full]-parameters[reduced]
  lr = 2*as.numeric(likelihoods[[full]]-likelihoods[[reduced]])
  if (df <= 0 || !is.finite(lr) || lr < -1e-8) {
    result$reason = "LRT unavailable: the likelihood difference or parameter-count difference is invalid."
    return(result)
  }
  result$lr = max(0,lr); result$df = df
  if (!random_same) {
    result$random_p_unavailable = TRUE
    result$reason = paste0("Nested ML likelihood-ratio comparison; Model ",full," is the larger model.")
    return(result)
  }
  if (!singular) result$p = stats::pchisq(result$lr,df=df,lower.tail=FALSE)
  test = if (singular) "Nested fixed-effect ML likelihood-ratio comparison" else
    "Nested fixed-effect likelihood-ratio test using ML (asymptotic chi-square)"
  result$reason = paste0(test,"; Model ",full,
    " is the larger model. Random-effects structures are identical.")
  result
}

jamovi_mixed_comparison = function(options, data) {
  second_options = jamovi_mixed_second_options(options)
  formulas = list(jamovi_mixed_formula(options,data)$formula,jamovi_mixed_formula(second_options,data)$formula)
  variables = unique(unlist(lapply(formulas,all.vars)))
  eligible = data[stats::complete.cases(data[,variables,drop=FALSE]),,drop=FALSE]
  if (!nrow(eligible)) stop("No common complete observations are available for both mixed models.",call.=FALSE)
  first = jamovi_mixed_fit(options,eligible)
  second = jamovi_mixed_fit(second_options,eligible)
  statistics = tryCatch(jamovi_mixed_comparison_statistics(list(first$model,second$model)),error=identity)
  if (inherits(statistics,"error")) statistics = list(aic=rep(NA_real_,2),bic=rep(NA_real_,2),bayes=rep(NA_real_,2),
    lr=NA_real_,df=NA_real_,p=NA_real_,warnings=character(),
    reason=paste0("ML comparison unavailable: ",conditionMessage(statistics)))
  list(model1=first$model,model2=second$model,data=first$data,cluster=options$cluster,
    first=first,second=second,statistics=statistics,omitted=nrow(data)-nrow(eligible))
}

jamovi_mixed_comparison_report = function(results, state, includeP=FALSE) {
  statistics = state$statistics
  values = data.frame(model=c("Model 1","Model 2"),n=c(stats::nobs(state$model1),stats::nobs(state$model2)),
    aic=statistics$aic,bic=statistics$bic,bayes.factor=statistics$bayes,
    lr=c(statistics$lr,NA_real_),df=c(statistics$df,NA_real_),p=c(statistics$p,NA_real_))
  p_column = results$comparison$getColumn("p")
  p_column$setVisible(includeP)
  jamovi_glm_table_rows(results$comparison,values)
  results$comparison$setNote("estimation",paste0("ML fits on ",nrow(state$data),
    " identical observations (",state$omitted," omitted). Original estimates and plots retain their selected ML/REML methods."))
  results$comparison$setNote("bayes",NULL)
  if (any(is.finite(statistics$bayes))) results$comparison$setNote("bayes",
    "Approximate BIC Bayes factors favor the model in each row; the two values are reciprocal.")
  test_note = statistics$reason
  if (is.finite(statistics$lr)) test_note = if (isTRUE(statistics$random_p_unavailable)) NULL else
    if (is.finite(statistics$p)) "Nested fixed-effects LRT (asymptotic chi-square)." else NULL
  results$comparison$setNote("test",test_note)
  results$comparison$setNote("randomP",NULL)
  if (isTRUE(p_column$visible) && isTRUE(statistics$random_p_unavailable)) {
    results$comparison$setNote("randomP",
      "P-value unavailable: random-effects comparisons may not follow the usual chi-square distribution; no calibrated alternative is implemented.")
  }
  if (isTRUE(statistics$singular)) results$comparison$setNote("singular",paste0(
    "Singular ML comparison fit; interpret covariance estimates cautiously. BIC Bayes factors",
    if (isTRUE(p_column$visible) && !isTRUE(statistics$random_p_unavailable)) " and ordinary p-values are" else " are",
    " unavailable."))
  warnings = unique(c(state$first$warnings,state$second$warnings,statistics$warnings))
  if (length(warnings)) results$comparison$setNote("warnings",paste(warnings,collapse="; "))
  content = character()
  for (i in seq_len(2)) {
    model = state[[paste0("model",i)]]
    content = c(content,paste0("<strong>Model ",i,":</strong> ",jamovi_mixed_escape(paste(deparse(stats::formula(model)),collapse=" ")),
      " (",if (lme4::isREML(model)) "REML" else "ML",
      if (lme4::isSingular(model)) "; singular fit" else "", ")"))
  }
  results$comparisonSpecification$setContent(paste0("<p>",paste(content,collapse="<br>"),"</p>"))
  results$specification$setVisible(FALSE)
  results$comparisonSpecification$setVisible(TRUE)
  results$comparison$setVisible(TRUE)
}

jamovi_mixed_comparison_plot = function(state, x=NULL, panels=NULL, group=NULL, type="cluster", clusters=3) {
  layout = list(x=x,panels=panels,group=group,type=type,clusters=clusters)
  if (!is.null(state$displayPlot) && identical(state$displayLayout,layout)) return(state$displayPlot)
  if (type == "cas") stop("Cluster-adjusted scatterplots are single-model displays. Choose population-level or cluster-specific predictions for model comparison.",call.=FALSE)
  if (!type %in% c("population","cluster")) stop("Choose population-level or cluster-specific comparisons.",call.=FALSE)
  predictors = setdiff(unique(c(all.vars(stats::formula(state$model1))[-1],all.vars(stats::formula(state$model2))[-1])),state$cluster)
  if (!length(predictors)) stop("These intercept-only models have no predictor for an x-axis comparison.",call.=FALSE)
  if (length(clusters) != 1L || !is.finite(clusters) || clusters < 1 || clusters != as.integer(clusters))
    stop("The number of displayed clusters must be a positive integer.",call.=FALSE)
  if (!identical(extract_random_term(state$model1),state$cluster) || !identical(extract_random_term(state$model2),state$cluster))
    stop("The existing mixed-model visualization requires a simple shared grouping-variable name.",call.=FALSE)
  if (identical(x,state$cluster)) stop("Use a predictor on x; place the grouping variable in panels.",call.=FALSE)
  if (type == "cluster" && !state$cluster %in% c(panels,group)) {
    stop("For cluster-specific comparisons, place the shared grouping variable in panels.",call.=FALSE)
  }
  formula = jamovi_model_layout(list(state$model1,state$model2),state$data,x,panels,group)
  model1 = state$model1; model2 = state$model2
  plot = compare_fits(formula,state$data,model1=model1,model2=model2,re=type=="cluster",
    clusters=clusters,report.se=FALSE,pred.type="response")
  plot = jamovi_comparison_style(plot)
  plot = plot + ggplot2::labs(subtitle=if (type == "cluster")
    "Model 1 vs Model 2: cluster-specific predictions" else "Model 1 vs Model 2: population-level predictions")
  attr(plot,"jamovi_mixed_comparison") = TRUE
  plot
}

# Canonicalize coefficient order, but compare the actual sparse random designs.
jamovi_mixed_same_random_design = function(model1, model2) {
  first = lme4::getME(model1,"Ztlist")
  second = lme4::getME(model2,"Ztlist")
  if (!setequal(names(first),names(second))) return(FALSE)
  identical(first[order(names(first))],second[order(names(second))])
}

# The milestone supports one correlated block: adding random coefficients nests
# its covariance structure. Compare actual designs, not just formula labels.
jamovi_mixed_random_subset = function(reduced, full) {
  first = lme4::getME(reduced,"Ztlist")
  second = lme4::getME(full,"Ztlist")
  if (!all(names(first) %in% names(second))) return(FALSE)
  all(vapply(names(first),function(term) identical(first[[term]],second[[term]]),logical(1)))
}
