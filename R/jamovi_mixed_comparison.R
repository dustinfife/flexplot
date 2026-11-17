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
    lr=NA_real_,df=NA_real_,p=NA_real_,warnings=unique(unlist(lapply(copies,`[[`,"warnings"))))
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
  full = which.max(ranks); reduced = 3L-full
  nested = diff(range(ranks)) > 0 && qr(cbind(matrix[[full]],matrix[[reduced]]))$rank == ranks[full]
  if (singular) {
    result$reason = "Singular ML fit: information criteria are descriptive; the BIC Bayes factor and ordinary LRT p-value are unavailable."
    return(result)
  }
  if (!random_same) {
    result$reason = "Random-effects structures differ. An ordinary chi-square LRT p-value is not reported because variance-component comparisons involve boundary conditions; a calibrated method is not implemented."
    return(result)
  }
  if (!nested) {
    result$reason = "LRT unavailable: fixed-effect models are nonnested or equivalent."
    return(result)
  }
  likelihoods = lapply(fitted,stats::logLik)
  df = attr(likelihoods[[full]],"df")-attr(likelihoods[[reduced]],"df")
  lr = 2*as.numeric(likelihoods[[full]]-likelihoods[[reduced]])
  if (df <= 0 || !is.finite(lr) || lr < -1e-8) {
    result$reason = "LRT unavailable: the likelihood difference or parameter-count difference is invalid."
    return(result)
  }
  result$lr = max(0,lr); result$df = df
  result$p = stats::pchisq(result$lr,df=df,lower.tail=FALSE)
  result$reason = paste0("Nested fixed-effect likelihood-ratio test using ML (asymptotic chi-square); Model ",full,
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

jamovi_mixed_comparison_report = function(results, state) {
  statistics = state$statistics
  values = data.frame(model=c("Model 1","Model 2"),n=c(stats::nobs(state$model1),stats::nobs(state$model2)),
    aic=statistics$aic,bic=statistics$bic,bayes.factor=statistics$bayes,
    lr=c(statistics$lr,NA_real_),df=c(statistics$df,NA_real_),p=c(statistics$p,NA_real_))
  p_column = results$comparison$getColumn("p")
  p_column$visible = results$fixed$getColumn("p")$visible
  jamovi_glm_table_rows(results$comparison,values)
  results$comparison$setNote("estimation","All comparison statistics use ML fits on identical complete observations. REML fits are copied and refitted with ML; the original model estimates and visualization fits retain their requested estimation methods.")
  results$comparison$setNote("population",paste0(nrow(state$data)," common observations; ",state$omitted,
    " omitted across both formulas. The original Model 1 estimates above retain its own eligible observations."))
  results$comparison$setNote("bayes","Approximate Bayes factors derived from BIC: Model 1's value favors Model 1 over Model 2; Model 2's value favors Model 2 over Model 1. The values are reciprocal, not exact Bayes factors. Unavailable approximations are blank.")
  results$comparison$setNote("test",statistics$reason)
  warnings = unique(c(state$first$warnings,state$second$warnings,statistics$warnings))
  if (length(warnings)) results$comparison$setNote("warnings",paste(warnings,collapse="; "))
  content = character()
  for (i in seq_len(2)) {
    model = state[[paste0("model",i)]]
    content = c(content,paste0("<p>Model ",i,": ",jamovi_mixed_escape(paste(deparse(stats::formula(model)),collapse=" ")),
      " (",if (lme4::isREML(model)) "REML" else "ML",
      if (lme4::isSingular(model)) "; singular fit" else "", ")</p>"))
  }
  results$comparisonSpecification$setContent(paste(content,collapse=""))
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
