jamovi_capture_model_plot = function(...) {
  capture$plot = original(...)
  capture$plot
}

jamovi_model_plot_recorder = function(capture) {
  recorder = jamovi_capture_model_plot
  environment(recorder) = list2env(
    list(capture=capture, original=flexplot:::jamovi_glm_model_plot),
    parent=environment(recorder))
  recorder
}


jamovi_generalized_analysis = function(data, family, terms=NULL, predictors=c("x", "a"), panels=NULL) {
  options = flexplot:::glmgeneralizedOptions$new(out="y", preds=predictors,
    modelTerms=terms, family=family, plotX=predictors[1], plotPanels=panels)
  flexplot:::glmgeneralizedClass$new(options=options, data=data)
}

jamovi_generalized_capture = function(...) {
  capture$plot = original(...)
  capture$plot
}

jamovi_generalized_recorder = function(capture) {
  recorder = jamovi_generalized_capture
  environment(recorder) = list2env(list(capture=capture,
    original=flexplot:::jamovi_glm_generalized_plot), parent=environment(recorder))
  recorder
}


# Coefficient summaries follow estimates.glm(); CI bounds retain full precision.
jamovi_reported_se = function(table) {
  table[[intersect(c("SE", "Std..Error", "Std. Error"), names(table))[1]]]
}

# Minimal host data reader: the actual R6 analysis still initializes and runs.
jamovi_reporting_read = function(columns) {
  data[, unique(unlist(columns, use.names=FALSE)), drop=FALSE]
}

jamovi_reporting_host = function(options, data) {
  analysis = flexplot:::glmgeneralizedClass$new(options=options, data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data), parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  analysis
}

jamovi_reporting_update = function(analysis, values) {
  for (name in names(values)) {
    option = analysis$options$option(name)
    option$value = values[[name]]
  }
  analysis$optionsChangedHandler(names(values))
  analysis$run()
}

# Independent fitted-model inference, including full coefficient-name alignment.
jamovi_expect_lm_coefficients = function(analysis, model, includeP=FALSE) {
  table = analysis$results$glmcat$asDF
  expect_identical(analysis$results$glmcat$status,"complete")
  coefficients = coef(model)
  inference = summary(model)$coefficients
  intervals = confint(model)
  expect_identical(table$var,names(coefficients))
  expect_equal(table$means,unname(coefficients),tolerance=1e-12)
  index = match(names(coefficients),rownames(inference))
  expect_equal(table$se,unname(inference[index,2]),tolerance=1e-12)
  expect_equal(table$lower,unname(intervals[,1]),tolerance=1e-12)
  expect_equal(table$upper,unname(intervals[,2]),tolerance=1e-12)
  expect_equal(table$t,unname(inference[index,3]),tolerance=1e-12)
  if (includeP) expect_equal(table$p,unname(inference[index,4]),tolerance=1e-12) else
    expect_false("p" %in% names(table))
  invisible(table)
}
