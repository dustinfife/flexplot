jamovi_mixed_test_data = function() {
  set.seed(925)
  data = data.frame(id=factor(rep(seq_len(16), each=15)),
    x=rep(seq(-1.5,1.5,length.out=15),16), z=rnorm(240),
    a=ordered(rep(c("low","middle","high"),80),levels=c("middle","low","high")))
  intercept = rnorm(16,sd=.8)
  slope = .25*intercept+rnorm(16,sd=.45)
  data$y = .3+.6*data$x+.2*data$z+.4*(data$a=="high")+.3*data$x*(data$a=="high")+
    .15*data$x^2+intercept[data$id]+slope[data$id]*data$x+rnorm(240,sd=.4)
  data
}

jamovi_mixed_analysis = function(data=jamovi_mixed_test_data(), ...) {
  supplied = jamovi_mixed_fixture_layout(list(...))
  defaults = list(out="y",preds=c("x","z","a"),cluster="id",graphic=FALSE,
    comparisonGraphic=if (is.null(supplied$graphic)) FALSE else supplied$graphic)
  options = do.call(flexplot:::mixedmodelOptions$new, utils::modifyList(defaults,supplied,keep.null=TRUE))
  flexplot:::mixedmodelClass$new(options=options,data=data)
}

jamovi_mixed_capture = function(...) {
  capture$plot = original(...)
  capture$plot
}

jamovi_mixed_recorder = function(capture) {
  recorder = jamovi_mixed_capture
  environment(recorder) = list2env(list(capture=capture,original=flexplot:::jamovi_mixed_plot),
    parent=environment(recorder))
  recorder
}

jamovi_mixed_comparison_analysis = function(data=jamovi_mixed_test_data(), ...) {
  values = list(out="y",preds=c("x","a","z"),cluster="id",compare=TRUE,graphic=FALSE,
    modelTerms=list("x","a","z",c("x","a")),interactions=TRUE,polynomials=TRUE,quadratic="x",
    randomSlopes="x",comparisonTerms=list("x","a","z"),comparisonRandomSlopes="x")
  supplied = jamovi_mixed_fixture_layout(list(...))
  # Existing statistical fixtures suppress previews unless explicitly requested.
  values$comparisonGraphic = if (is.null(supplied$graphic)) FALSE else supplied$graphic
  for (name in names(supplied)) values[name] = supplied[name]
  options = do.call(flexplot:::mixedmodelOptions$new,values)
  flexplot:::mixedmodelClass$new(options=options,data=data)
}

# Older regression fixtures describe one layout; explicitly configure both outputs.
# Independence tests construct their new settings directly.
jamovi_mixed_fixture_layout = function(values) {
  for (pair in list(c("plotX","comparisonX"),c("plotPanels","comparisonPanels"),
      c("plotGroup","comparisonGroup"),c("predictionType","comparisonPredictionType"),
      c("clusters","comparisonClusters"),c("plotLabels","comparisonPlotLabels"),c("plotLabelStore","comparisonPlotLabelStore")))
    if (pair[1] %in% names(values) && !pair[2] %in% names(values)) values[pair[2]] = values[pair[1]]
  values
}
