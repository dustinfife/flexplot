test_that("GLM stale comparison layouts preserve fitted tables and recover independently", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  previous = ggplot2::theme_get(); withr::defer(ggplot2::theme_set(previous))
  data = small
  # Force actual common-sample selection without making the stale role a fit term.
  data$z[1:3] = NA_real_
  for (family in c("gaussian","binomial","auto")) {
    for (shown in c(TRUE,FALSE)) {
      analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
        out=if (family=="gaussian") "y" else "y_bin",preds=c("x","z","a"),family=family,
        modelTerms=list("x","z"),comparisonTerms=list("x"),compare=TRUE,
        plotX="x",comparisonX="x",graphic=!shown,comparisonGraphic=shown),data)
      output = isolation_resources(analysis); withr::defer(unlink(output,recursive=TRUE))
      expect_true(analysis$run())
      coefficients = analysis$results$coefficients$asDF
      statistics = analysis$results$comparison$asDF
      state = analysis$results$comparisonplot$state
      expected = glm(if(family=="gaussian") y~x+z else y_bin~x+z,
        data=data,family=if(family=="gaussian") gaussian() else binomial())
      expect_equal(coef(state$model1),coef(expected),tolerance=0)
      expect_identical(nobs(state$model1),nobs(state$model2))
      if (shown) isolation_image(analysis,"comparisonplot",output)
      for (x in c("a","x","a","x")) {
        expect_true(jamovi_reporting_update(analysis,list(comparisonX=x)))
        expect_null(analysis$results$comparisonplot$filePath)
        expect_equal(analysis$results$coefficients$asDF,coefficients,tolerance=0)
        expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
        expect_identical(analysis$results$coefficients$status,"complete")
        expect_identical(analysis$results$comparison$status,"complete")
        expect_identical(analysis$results$plot$visible,!shown)
        expect_identical(analysis$results$comparisonplot$visible,shown)
        current = analysis$results$comparisonplot$state
        expect_equal(coef(current$model1),coef(state$model1),tolerance=0)
        expect_equal(predict(current$model1,type="response"),predict(state$model1,type="response"),tolerance=0)
        if (x=="a") {
          expect_null(current$formula)
          expect_match(current$plotError,"predictor")
          expect_length(analysis$options$comparisonPlotLabels,0L)
          if (shown) {
            expect_true(analysis$results$comparisonPlotExplanation$visible)
            expect_match(analysis$results$comparisonPlotExplanation$content,"predictor")
            isolation_image(analysis,"comparisonplot",output,current$plotError)
          } else expect_false(analysis$results$comparisonPlotExplanation$visible)
          if (!shown) isolation_image(analysis,"plot",output)
        } else {
          expect_null(current$plotError)
          if (shown) isolation_image(analysis,"comparisonplot",output)
        }
      }
    }
  }
})

test_that("GLM fitting and inference failures are not swallowed by layout isolation", {
  skip_if_not_installed("jmvcore")
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","a"),family="binomial",
    modelTerms=list("x","z"),comparisonTerms=list("x"),compare=TRUE,comparisonX="a")
  expect_error(flexplot:::jamovi_glm_generalized_comparison(options,small),"Model 1:")
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","a"),family="gaussian",
    modelTerms=list("x","z"),comparisonTerms=list("x"),compare=TRUE,comparisonX="a")
  local_mocked_bindings(model.comparison=function(...) stop("inference probe"),.package="flexplot")
  expect_error(flexplot:::jamovi_glm_generalized_comparison(options,small),"inference probe")
})

test_that("GLM comparison rendering errors clear only their image and explanation", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","z"),
    family="auto",compare=TRUE,comparisonTerms=list("x"),plotX="x",comparisonX="x"),small)
  output = isolation_resources(analysis); withr::defer(unlink(output,recursive=TRUE))
  expect_true(analysis$run())
  coefficients = analysis$results$coefficients$asDF
  statistics = analysis$results$comparison$asDF
  inject = function() {
    local_mocked_bindings(jamovi_glm_generalized_comparison_plot=function(...) stop("comparison render probe"),
      .package="flexplot")
    isolation_image(analysis,"comparisonplot",output,"comparison render probe")
    expect_match(analysis$results$comparisonPlotExplanation$content,"comparison render probe")
    expect_equal(analysis$results$coefficients$asDF,coefficients,tolerance=0)
    expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
    isolation_image(analysis,"plot",output)
  }
  inject()
  expect_true(analysis$run())
  isolation_image(analysis,"comparisonplot",output)
  expect_false(grepl("comparison render probe",analysis$results$comparisonPlotExplanation$content,fixed=TRUE))
  expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
})

test_that("LM label preparation and rendering failures stay local and retain saved edits", {
  skip_if_not_installed("jmvcore")
  previous = ggplot2::theme_get(); withr::defer(ggplot2::theme_set(previous))
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  data = small
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds=c("x","z","a"),compare=TRUE,comparisonTerms=list("x"),
    plotX="x",plotPanels="a",ghostLines=TRUE,comparisonX="x"),data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader); analysis$.setReadDatasetHeaderSource(reader)
  output = isolation_resources(analysis); withr::defer(unlink(output,recursive=TRUE))
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=
    isolation_title(analysis$options$plotLabels,"Saved primary title"))))
  coefficients = analysis$results$glmcat$asDF
  statistics = analysis$results$comparison$asDF
  store = analysis$options$plotLabelStore
  isolation_image(analysis,"plot",output)
  original = flexplot:::jamovi_plot_apply_labels
  for (phase in c("preview","renderer")) {
    # Restore bindings at function exit so the same analysis can recover below.
    inject = function() {
      local_mocked_bindings(jamovi_plot_apply_labels=function(plot,entries,context,layout) {
        if (context$output=="model") stop("<literal> label & probe")
        original(plot,entries,context,layout)
      },.package="flexplot")
      if (phase=="preview") {
        expect_true(analysis$run())
        expect_null(analysis$results$plot$filePath)
        expect_match(analysis$results$plot$state$plotError,"<literal> label & probe",fixed=TRUE)
      }
      isolation_image(analysis,"plot",output,"<literal> label & probe")
      expect_true(analysis$results$plotExplanation$visible)
      expect_match(analysis$results$plotExplanation$content,"&lt;literal&gt;")
      expect_false(grepl("Reference-panel",analysis$results$plotExplanation$content,fixed=TRUE))
      expect_equal(analysis$results$glmcat$asDF,coefficients,tolerance=0)
      expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
      expect_identical(analysis$options$plotLabelStore,store)
      isolation_image(analysis,"comparisonplot",output)
    }
    if (phase=="renderer") expect_true(analysis$run())
    inject()
    expect_true(analysis$run())
    expect_null(analysis$results$plot$filePath)
    expect_null(analysis$results$plot$state$plotError)
    isolation_image(analysis,"plot",output)
    jamovi_expect_lm_coefficients(analysis,lm(y~x+z+a,small))
    expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
    expect_false(grepl("label &amp; probe",analysis$results$plotExplanation$content,fixed=TRUE))
  }
  original_info = flexplot:::jamovi_lm_label_info
  inject_comparison = function() {
    local_mocked_bindings(jamovi_lm_label_info=function(model,options,comparison=FALSE,formula=NULL) {
      if (comparison) stop("comparison label preparation probe")
      original_info(model,options,comparison,formula)
    },.package="flexplot")
    expect_true(analysis$run())
    expect_null(analysis$results$plot$state$plotError)
    isolation_image(analysis,"comparisonplot",output,"comparison label preparation probe")
    isolation_image(analysis,"plot",output)
    expect_equal(analysis$results$glmcat$asDF,coefficients,tolerance=0)
    expect_equal(analysis$results$comparison$asDF,statistics,tolerance=0)
  }
  inject_comparison()
  expect_true(analysis$run())
  isolation_image(analysis,"comparisonplot",output)
  expect_identical(analysis$options$plotLabelStore,store)
})
