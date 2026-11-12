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

