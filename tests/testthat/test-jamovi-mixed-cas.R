jamovi_mixed_cas_data = function() {
  data = jamovi_mixed_test_data()[seq_len(120),]
  data$continuous = data$x
  data$group = data$a
  data$mixed = data$y
  data$count = rep(0:5,length.out=nrow(data))
  data
}

jamovi_mixed_cas_resource = function(name, extension) {
  list(rootPath=output_dir,relPath=paste0(name,".",extension))
}

test_that("the simplified CAS case renders real images with comparison on or off and cluster panels", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_cas_data()
  output_dir = withr::local_tempdir()
  resources = jamovi_mixed_cas_resource
  environment(resources) = list2env(list(output_dir=output_dir),parent=environment(resources))
  for (comparison in c(FALSE,TRUE)) for (panels in list(NULL,"id")) {
    analysis = jamovi_mixed_analysis(data,out="mixed",preds=c("continuous","group"),
      modelTerms=list("continuous","group"),comparisonTerms=list("continuous"),
      graphic=TRUE,compare=comparison,predictionType="cas",plotX="continuous",plotPanels=panels)
    expect_true(analysis$run())
    image = if (comparison) analysis$results$comparisonplot else analysis$results$plot
    plot = image$state$displayPlot
    if (comparison) plot = attr(plot,"jamovi_cas_children")[[1]]
    model = if (comparison) image$state$model1 else image$state$model
    expected = model.response(model.frame(model)) - (predict(model)-predict(model,re.form=NA))
    expect_equal(plot$data$mixed_adj,unname(expected),ignore_attr=TRUE,tolerance=1e-10)
    expect_equal(nrow(plot$data),nobs(model))
    expect_false(any(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomSmooth"),logical(1))))
    curve = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    x = unique(curve$continuous)
    predictions = vapply(x,function(value) {
      profile = model.frame(model)
      profile$continuous = value
      mean(predict(model,newdata=profile,re.form=NA))
    },numeric(1))
    expect_equal(curve$prediction,unname(predictions[match(curve$continuous,x)]),tolerance=1e-10)
    built = ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout),if (is.null(panels)) 1L else nlevels(model.frame(model)$id))
    if (comparison) {
      expect_identical(plot$labels$title,"Model 1")
      expect_null(plot$labels$caption)
      expect_true(analysis$results$comparison$visible)
    }
    analysis$.setResourcesPathSource(resources)
    expect_true(analysis$.createImage(if (comparison) ".comparisonplot" else ".plot",image))
    file = file.path(output_dir,image$filePath)
    expect_gt(file.info(file)$size,2000)
    expect_identical(as.integer(readBin(file,"raw",8L)),c(137L,80L,78L,71L,13L,10L,26L,10L))
    expect_identical(image$status,"complete")
  }
})

test_that("CAS handles the original numeric moderator and cluster-panel layout", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_cas_data()
  for (comparison in c(FALSE,TRUE)) {
    analysis = jamovi_mixed_analysis(data,out="z",preds=c("count","mixed"),modelTerms=list("count","mixed"),
      comparisonTerms=list("count"),graphic=TRUE,compare=comparison,predictionType="cas",
      plotX="count",plotPanels=c("mixed","id"))
    expect_true(analysis$run())
    image = if (comparison) analysis$results$comparisonplot else analysis$results$plot
    plot = image$state$displayPlot
    if (comparison) plot = attr(plot,"jamovi_cas_children")[[1]]
    model = if (comparison) image$state$model1 else image$state$model
    expected = model.response(model.frame(model)) - (predict(model)-predict(model,re.form=NA))
    expect_equal(plot$data$z_adj,unname(expected),ignore_attr=TRUE,tolerance=1e-10)
    expect_true(all(c("mixed_binned","id") %in% names(plot$data)))
    expect_no_warning(ggplot2::ggplotGrob(plot))
    expect_gt(nrow(ggplot2::ggplot_build(plot)$layout$layout),1L)
  }
})

test_that("failed CAS images serialize as errors rather than endless rendering", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  data = jamovi_mixed_cas_data()
  for (comparison in c(FALSE,TRUE)) {
    analysis = jamovi_mixed_analysis(data,out="mixed",preds=c("continuous","group"),
      graphic=TRUE,compare=comparison,predictionType="cas",plotX="id")
    expect_true(analysis$run())
    image = if (comparison) analysis$results$comparisonplot else analysis$results$plot
    expect_identical(image$status,"error")
    expect_match(image$.__enclos_env__$private$.error,"Use a model predictor on x")
    expect_null(image$state)
    expect_equal(image$asProtoBuf(status=3L,includeState=FALSE)$status,4L)
    # Also exercise errors raised during drawing after successful plot preparation.
    expect_true(jamovi_reporting_update(analysis,list(plotX="continuous")))
    expect_false(is.null(image$state))
    local_mocked_bindings(jamovi_mixed_apply_labels=function(...) stop("CAS render probe"))
    callback = if (comparison) analysis$.__enclos_env__$private$.comparisonplot else analysis$.__enclos_env__$private$.plot
    expect_false(callback(image))
    expect_match(image$.__enclos_env__$private$.error,"CAS render probe")
    expect_null(image$state)
    expect_equal(image$asProtoBuf(status=3L,includeState=FALSE)$status,4L)
  }
})
