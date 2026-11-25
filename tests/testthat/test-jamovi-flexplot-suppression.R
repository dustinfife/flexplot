ordinary_fitted_layers = function(plot) {
  Filter(function(layer) inherits(layer$stat,"StatSmooth") || inherits(layer$stat,"StatSummary") ||
    inherits(layer$geom,"GeomLine") || "pred_prob" %in% names(layer$data),plot$layers)
}

test_that("ordinary suppression removes continuous/categorical fits and ghosts, then restores them", {
  for (pred in c("x","a")) {
    options = list(preds=pred,ghost=TRUE,line="Regression",se=TRUE,suppr=FALSE,jittx=0,jitty=0,bins=3)
    formula = reformulate(paste(pred,"| b"),response="y")
    fitted = suppressMessages(jamovi_plots(formula,small,options))
    expect_gt(length(ordinary_fitted_layers(fitted)),0L)
    expect_true(any(vapply(fitted$layers,function(layer)inherits(layer$geom,"GeomLine") &&
      identical(layer$aes_params$colour,"black"),logical(1))))
    options$suppr = TRUE
    hidden = suppressMessages(jamovi_plots(formula,small,options))
    expect_length(ordinary_fitted_layers(hidden),0L)
    expect_null(attr(hidden,"ghost_reference"))
    expect_identical(flexplot:::jamovi_model_ghost_note(hidden),"")
    raw = ggplot2::ggplot_build(hidden)$data[[1]]
    expect_equal(nrow(raw),nrow(small))
    expect_equal(sort(raw$y),sort(small$y))
    expect_true(options$ghost)
    options$suppr = FALSE
    restored = suppressMessages(jamovi_plots(formula,small,options))
    expect_equal(ggplot2::ggplot_build(restored)$data,ggplot2::ggplot_build(fitted)$data)
  }
})

test_that("binary categorical probability fits are suppressible while observations remain", {
  d = small;d$binary = factor(d$y_bin,levels=c(0,1))
  options = list(preds="a",ghost=TRUE,suppr=FALSE,jittx=0,jitty=0,bins=3)
  fitted = suppressMessages(jamovi_plots(binary~a|b,d,options))
  expect_gt(length(ordinary_fitted_layers(fitted)),0L)
  options$suppr = TRUE
  hidden = suppressMessages(jamovi_plots(binary~a|b,d,options))
  expect_length(ordinary_fitted_layers(hidden),0L)
  raw = Filter(function(layer)"y_scaled" %in% names(layer$data),hidden$layers)
  expect_length(raw,1L)
  expect_equal(nrow(raw[[1]]$data),nrow(d))
  expect_true(all(raw[[1]]$data$binary %in% c(0,1)))
  expect_s3_class(ggplot2::ggplotGrob(hidden),"gtable")
  options$suppr = FALSE
  restored = suppressMessages(jamovi_plots(binary~a|b,d,options))
  expect_equal(ggplot2::ggplot_build(restored)$data,ggplot2::ggplot_build(fitted)$data)
})

test_that("native predictor/suppression transitions and legacy options survive serialization", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  options = flexplot:::flexplotaOptions$new(out="y",preds="x",given="b",suppr=TRUE,ghost=TRUE,
    line="Regression",jittx=0,jitty=0,bins=5)
  analysis = flexplot:::flexplotaClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=small),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  for (pred in c("x","a","x")) for (suppress in c(TRUE,FALSE)) {
    expect_true(jamovi_reporting_update(analysis,list(preds=pred,suppr=suppress)))
    plot = jamovi_flexplot_render(analysis,capture)
    expect_identical(length(ordinary_fitted_layers(plot))==0L,suppress)
    expect_true(analysis$options$ghost)
    expect_equal(analysis$options$bins,5)
    expect_identical(analysis$results$explanation$content,"")
  }
  # The hidden related-t option is still part of the real protobuf API.
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  cell = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  string = function(value) {item = cell$clone();item$s=value;item}
  yes = cell$clone();yes$o=1L
  bins = cell$clone();bins$d=5
  variables = pb$clone();variables$options=list(string("a"))
  preds = cell$clone();preds$c=variables
  pb$names = c("out","preds","diff","suppr","ghost","bins")
  pb$options = list(string("y"),preds,yes,yes,yes,bins)
  legacy = flexplot:::flexplotaOptions$new();legacy$fromProtoBuf(pb)
  restored = flexplot:::flexplotaOptions$new();restored$fromProtoBuf(legacy$asProtoBuf())
  expect_true(restored$diff);expect_true(restored$suppr);expect_true(restored$ghost);expect_equal(restored$bins,5)
  expect_s3_class(jamovi_plots(y~a,small,restored),"ggplot")
  expect_s3_class(jamovi_plots(y~1,small,list(suppr=TRUE,ghost=TRUE)),"ggplot")
})
