isolation_saved_options = function(factory, values) {
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  message$names = names(values)
  message$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options = factory$new()
  options$fromProtoBuf(message)
  options
}

test_that("GLM stale saved comparison roles and labels survive protocol reopen and recovery", {
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  values = list(out="y",preds=c("x","z","a"),family="gaussian",modelTerms=list("x","z"),
    comparisonTerms=list("x"),compare=TRUE,graphic=FALSE,comparisonGraphic=TRUE,
    outputOptionsVersion=2,comparisonX="x")
  options = isolation_saved_options(flexplot:::glmgeneralizedOptions,values)
  analysis = jamovi_reporting_host(options,small)
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(comparisonPlotLabels=
    isolation_title(analysis$options$comparisonPlotLabels,"Retained comparison title"))))
  expected = analysis$results$comparison$asDF
  # The host saves its current option message, not just the R-side value.
  flexplot:::jamovi_output_option(analysis$options,"comparisonX","a")
  expect_true(jamovi_reporting_update(analysis,list(comparisonX="a")))
  options = flexplot:::glmgeneralizedOptions$new()
  options$fromProtoBuf(analysis$options$asProtoBuf())
  reopened = jamovi_reporting_host(options,small)
  expect_identical(options$comparisonX,"a")
  expect_true(reopened$run())
  expect_equal(reopened$results$comparison$asDF,expected,tolerance=0)
  expect_gt(reopened$results$coefficients$rowCount,0L)
  expect_match(reopened$results$comparisonplot$state$plotError,"predictor")
  expect_true(jamovi_reporting_update(reopened,list(comparisonX="x")))
  expect_true(any(vapply(options$comparisonPlotLabels,function(row)
    row$label=="Retained comparison title",logical(1))))
  expect_null(reopened$results$comparisonplot$state$plotError)
})

test_that("LM real saved label messages retain edits through preview errors and reopen", {
  jmvcore:::initProtoBuf()
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  previous = ggplot2::theme_get(); withr::defer(ggplot2::theme_set(previous))
  options = isolation_saved_options(flexplot:::glmbasicOptions,list(out="y",preds=c("x","z","a"),
    compare=TRUE,comparisonTerms=list("x"),plotX="x",comparisonX="x",graphic=TRUE))
  analysis = flexplot:::glmbasicClass$new(options=options,data=small)
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=
    isolation_title(options$plotLabels,"Saved primary title"))))
  coefficients = analysis$results$glmcat$asDF
  comparison = analysis$results$comparison$asDF
  store = options$plotLabelStore
  original = flexplot:::jamovi_plot_apply_labels
  inject = function() {
    local_mocked_bindings(jamovi_plot_apply_labels=function(plot,entries,context,layout) {
      if (context$output=="model") stop("saved preview probe")
      original(plot,entries,context,layout)
    },.package="flexplot")
    expect_true(analysis$run())
    expect_identical(analysis$results$plot$status,"error")
    expect_equal(analysis$results$glmcat$asDF,coefficients,tolerance=0)
    expect_equal(analysis$results$comparison$asDF,comparison,tolerance=0)
  }
  inject()
  reopened = flexplot:::glmbasicOptions$new()
  reopened$fromProtoBuf(options$asProtoBuf())
  expect_identical(reopened$plotLabelStore,store)
  restored = flexplot:::glmbasicClass$new(options=reopened,data=small)
  expect_true(restored$run())
  expect_null(restored$results$plot$state$plotError)
  expect_true(any(vapply(reopened$plotLabels,function(row)row$label=="Saved primary title",logical(1))))
  expect_equal(restored$results$glmcat$asDF,coefficients,tolerance=0)
  expect_equal(restored$results$comparison$asDF,comparison,tolerance=0)
  expect_true(restored$.__enclos_env__$private$.plot(restored$results$plot))
  expect_identical(restored$results$plot$status,"complete")
})
