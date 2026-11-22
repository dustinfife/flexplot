test_that("LM waiting, fitting failures and recovery clear every output independently of estimates", {
  skip_if_not_installed("jmvcore")
  data = small;data$empty = NA_real_
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    graphic=FALSE,graphicassump=TRUE,estimates=FALSE),data=NULL)
  reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  analysis$init()
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$plot$visible)
  expect_true(analysis$run())
  expect_match(analysis$results$waiting$content,"Select an outcome and predictors",fixed=TRUE)
  for (name in c("plot","comparisonplot","assumpplot","glmcat","rsq")) expect_false(analysis$results[[name]]$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_match(analysis$results$waiting$content,"Select at least one predictor",fixed=TRUE)
  expect_true(jamovi_reporting_update(analysis,list(out=NULL,preds="x")))
  expect_match(analysis$results$waiting$content,"Select an outcome variable",fixed=TRUE)
  for (i in 1:2) {
    expect_true(jamovi_reporting_update(analysis,list(out="y",preds="x",compare=TRUE,comparisonTerms=list(),comparisonX="x")))
    expected = analysis$results$glmcat$asDF
    expect_true(analysis$results$comparison$visible)
    for (name in c("plot","assumpplot","comparisonplot")) analysis$results[[name]]$.setPath("old.png")
    expect_true(jamovi_reporting_update(analysis,list(out="empty")))
    expect_true(analysis$results$explanation$visible)
    expect_match(analysis$results$explanation$content,"No complete observations",fixed=TRUE)
    expect_false(analysis$results$waiting$visible)
    for (name in c("plot","assumpplot","comparisonplot")) {
      expect_false(analysis$results[[name]]$visible)
      expect_null(analysis$results[[name]]$filePath)
      expect_null(analysis$results[[name]]$state)
    }
    for (name in c("glmcat","factorMeans","diff","rsq","comparison")) expect_equal(analysis$results[[name]]$rowCount,0)
    expect_true(jamovi_reporting_update(analysis,list(out="y",compare=FALSE)))
    expect_equal(analysis$results$glmcat$asDF,expected)
    expect_false(analysis$results$glmcat$visible)
    expect_false(analysis$results$comparison$visible)
    expect_null(analysis$results$comparisonplot$state)
    expect_identical(analysis$results$explanation$content,"")
    jamovi_expect_lm_coefficients(analysis,lm(y~x,data))
  }
})

test_that("LM comparison layouts and residual draw errors preserve fitted results and complete images", {
  skip_if_not_installed("jmvcore")
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),
    compare=TRUE,comparisonTerms=list("x"),comparisonX="x",graphicassump=TRUE),data=small)
  output = withr::local_tempdir()
  analysis$.setResourcesPathSource(function(name,ext)list(rootPath=output,relPath=paste0(name,".",ext)))
  old_theme = ggplot2::theme_get();withr::defer(ggplot2::theme_set(old_theme))
  expect_true(analysis$run())
  expected = analysis$results$comparison$asDF
  for (group in c("x",NA_character_,"x")) {
    expect_true(jamovi_reporting_update(analysis,list(comparisonGroup=if(is.na(group)) NULL else group)))
    image = analysis$results$comparisonplot
    expect_true(suppressWarnings(analysis$.createImage(".comparisonplot",image,noThrow=TRUE)))
    expect_identical(image$status,if(is.na(group)) "complete" else "error")
    expect_gt(file.info(file.path(output,image$filePath))$size,1000)
    expect_equal(analysis$results$comparison$asDF,expected)
    jamovi_expect_lm_coefficients(analysis,lm(y~x+a,small))
  }
  local_mocked_bindings(visualize=function(...) stop("Residual <plot> & probe"))
  expect_true(analysis$.createImage(".assumpplot",analysis$results$assumpplot,noThrow=TRUE))
  expect_identical(analysis$results$assumpplot$status,"error")
  expect_null(analysis$results$assumpplot$state)
  expect_equal(analysis$results$comparison$asDF,expected)
})

test_that("Flexplot remains univariate-capable and recovers from empty and failed plots", {
  skip_if_not_installed("jmvcore")
  data = small;data$empty = NA_real_
  analysis = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(suppr=TRUE,ghost=FALSE),data=NULL)
  reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  output = withr::local_tempdir()
  analysis$init()
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$plot$visible)
  analysis$.setResourcesPathSource(function(name,ext)list(rootPath=output,relPath=paste0(name,".",ext)))
  expect_true(analysis$run())
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$plot$visible)
  for (values in list(list(out="y"),list(preds="x",given=c("a","b")),list(preds=NULL,given=NULL))) {
    expect_true(jamovi_reporting_update(analysis,values))
    expect_null(analysis$results$plot$filePath)
    expect_true(suppressWarnings(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE)))
    expect_identical(analysis$results$plot$status,"complete")
    expect_false(analysis$results$waiting$visible)
  }
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_null(analysis$results$plot$filePath)
  expect_null(analysis$results$plot$state)
  expect_true(analysis$results$waiting$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="empty")))
  expect_true(analysis$results$explanation$visible)
  expect_match(analysis$results$explanation$content,"No complete observations",fixed=TRUE)
  expect_false(analysis$results$plot$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="b",preds=c("a","x"))))
  expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
  expect_identical(analysis$results$plot$status,"error")
  expect_null(analysis$results$plot$state)
  expect_true(jamovi_reporting_update(analysis,list(out="y",preds="x")))
  expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
  expect_identical(analysis$results$plot$status,"complete")
  original = flexplot:::jamovi_plots;fail = TRUE
  local_mocked_bindings(jamovi_plots=function(...) {if(fail) stop("Unsupported <layout> & probe");original(...)})
  for (i in 1:2) {
    expect_true(jamovi_reporting_update(analysis,list(out="y",preds="x")))
    expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
    expect_identical(analysis$results$plot$status,"error")
    expect_null(analysis$results$plot$state)
    expect_gt(file.info(file.path(output,analysis$results$plot$filePath))$size,1000)
    fail = FALSE
    expect_true(analysis$run())
    expect_null(analysis$results$plot$filePath)
    expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
    expect_identical(analysis$results$plot$status,"complete")
    fail = TRUE
  }
})
