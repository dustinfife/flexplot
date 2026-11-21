# Shared numerical inputs for packaged R6 and source-only renderer tests.
jamovi_diagnostic_data = function() {
  data = small
  data$factor_y = factor(data$y_bin, levels=c(0,1), labels=c("no","yes"))
  data$duplicate = data$x
  data$missing_y = NA_real_
  data
}

jamovi_diagnostic_cases = function() list(
  missing_both=list(values=list(out=NULL,preds=character()), waiting=TRUE, text="Select an outcome and predictors"),
  missing_outcome=list(values=list(out=NULL), waiting=TRUE, text="Select an outcome variable"),
  missing_predictors=list(values=list(preds=character()), waiting=TRUE, text="Select at least one predictor"),
  empty_terms=list(values=list(modelTerms=list()), waiting=TRUE, text="No Model 1 terms are selected"),
  factor_gaussian=list(values=list(out="factor_y",family="gaussian"), waiting=FALSE, text="finite numeric outcome"),
  invalid_binomial=list(values=list(out="y"), waiting=FALSE, text="numeric 0/1 outcome"),
  rank_deficient=list(values=list(preds=c("x","duplicate"),modelTerms=list("x","duplicate")), waiting=FALSE, text="rank deficient"),
  invalid_terms=list(values=list(modelTerms=list("z")), waiting=FALSE, text="selected predictor variables"),
  no_complete_rows=list(values=list(out="missing_y"), waiting=FALSE, text="No complete observations"),
  comparison_model2=list(values=list(compare=TRUE,preds=c("x","duplicate"),modelTerms=list("x"),
    comparisonTerms=list("x","duplicate"),comparisonX="x"), waiting=FALSE, text="Model 2: The model is not estimable")
)
