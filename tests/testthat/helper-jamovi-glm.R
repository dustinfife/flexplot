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

