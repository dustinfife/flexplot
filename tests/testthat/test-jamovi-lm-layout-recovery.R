test_that("LM layout failures complete real diagnostic images and recover without stale output", {
  skip_if_not_installed("jmvcore")
  data = small;data$group = data$a
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(out="y",
    preds=c("x","group"),plotPanels="group",plotGroup="x",graphic=TRUE),data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  output = withr::local_tempdir()
  analysis$.setResourcesPathSource(function(name,ext)list(rootPath=output,relPath=paste0(name,".",ext)))
  previous_theme = ggplot2::theme_get();withr::defer(ggplot2::theme_set(previous_theme))
  expect_true(analysis$run())
  expected = analysis$results$glmcat$asDF
  expected_means = analysis$results$factorMeans$asDF
  image = analysis$results$plot
  for (valid in c(FALSE,TRUE,FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(plotGroup=if(valid) NULL else "x")))
    expect_null(image$filePath)
    expect_true(suppressWarnings(analysis$.createImage(".plot",image,noThrow=TRUE)))
    expect_gt(file.info(file.path(output,image$filePath))$size,1000)
    expect_identical(image$status,if(valid) "complete" else "error")
    if (!valid) {
      expect_match(image$.__enclos_env__$private$.error,"Choose different model predictors",fixed=TRUE)
      expect_null(image$state)
    } else expect_false(is.null(image$state))
    expect_equal(analysis$results$glmcat$asDF,expected)
    expect_equal(analysis$results$factorMeans$asDF,expected_means)
    jamovi_expect_lm_coefficients(analysis,lm(y~x+group,data))
  }
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE,comparisonTerms=list("x"),comparisonX="x",plotGroup="x")))
  comparison = analysis$results$comparison$asDF
  expect_equal(nrow(comparison),2)
  expect_true(analysis$.createImage(".plot",image,noThrow=TRUE))
  expect_identical(image$status,"error")
  expect_true(jamovi_reporting_update(analysis,list(plotGroup=NULL)))
  expect_true(analysis$.createImage(".plot",image,noThrow=TRUE))
  expect_equal(analysis$results$comparison$asDF,comparison)
  expect_equal(analysis$results$glmcat$asDF,expected)
  # Exceptions raised while drawing labels must also produce a terminal image.
  local_mocked_bindings(jamovi_plot_apply_labels=function(...) stop("<literal> render & probe"))
  expect_true(analysis$run())
  expect_true(analysis$.createImage(".plot",image,noThrow=TRUE))
  expect_identical(image$status,"error")
  expect_null(image$state)
  expect_match(image$.__enclos_env__$private$.error,"<literal> render & probe",fixed=TRUE)
})

test_that("LM automatic layouts retain valid numeric and categorical grouping and panels", {
  skip_if_not_installed("jmvcore")
  data = small;data$group = data$a
  previous_theme = ggplot2::theme_get();withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_options(device=function(...)grDevices::pdf(file=NULL))
  for (roles in list(list(),list(plotX="x"),list(plotGroup="z"),list(plotGroup="group"),
    list(plotPanels="group"),list(plotPanels=c("group","b")),
    list(plotX="x",plotGroup="z",plotPanels="group"))) {
    options = do.call(flexplot:::glmbasicOptions$new,c(list(out="y",preds=c("x","z","group","b")),roles))
    analysis = flexplot:::glmbasicClass$new(options=options,data=data)
    expect_true(suppressWarnings(analysis$run()))
    expect_true(suppressWarnings(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    expect_identical(analysis$results$plot$status,"complete")
    expect_identical(analysis$results$plot$.__enclos_env__$private$.error,"")
    jamovi_expect_lm_coefficients(analysis,lm(y~x+z+group+b,data))
  }
  for (roles in list(list(plotX="x",plotGroup="x"),list(plotX="x",plotPanels="x"),
    list(plotGroup="z"))) {
    options = do.call(flexplot:::glmbasicOptions$new,c(list(out="y",preds=c("x","group")),roles))
    analysis = flexplot:::glmbasicClass$new(options=options,data=data)
    expect_true(analysis$run())
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_identical(analysis$results$plot$status,"error")
    expect_null(analysis$results$plot$state)
    expect_gt(analysis$results$glmcat$rowCount,0)
  }
})
