test_that("simultaneous Mixed plots use requested-method common-sample fits and independent caches", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$z[1:9] = NA_real_
  original = flexplot:::jamovi_mixed_fit
  fits = 0L
  local_mocked_bindings(jamovi_mixed_fit=function(...) {fits <<- fits+1L; original(...)},.package="flexplot")
  for (method in c("ML","REML")) {
    options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",
      modelTerms=list("x","a"),randomSlopes="x",estimation=method,compare=TRUE,
      comparisonTerms=list("x","z"),comparisonRandomSlopes=NULL,comparisonEstimation="REML",
      graphic=TRUE,comparisonGraphic=TRUE,ghostLines=TRUE,plotX="x",plotPanels="a",predictionType="population",
      comparisonX="x",comparisonPanels="id",comparisonPredictionType="cluster",comparisonClusters=4)
    analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    fits = 0L
    expect_true(analysis$run())
    expect_identical(fits,2L)
    main = analysis$results$plot$state
    comparison = analysis$results$comparisonplot$state
    expect_true(analysis$results$plot$visible)
    expect_true(analysis$results$comparisonplot$visible)
    expect_identical(main$model,comparison$model1)
    expect_identical(lme4::isREML(main$model),method=="REML")
    expect_true(lme4::isREML(comparison$model2))
    expect_true(all(!vapply(comparison$statistics$models,lme4::isREML,logical(1))))
    expect_identical(rownames(main$data),rownames(data[complete.cases(data[c("y","x","a","z","id")]),]))
    reference = lme4::lmer(y~x+a+(1+x|id),data=main$data,REML=method=="REML")
    expect_equal(lme4::fixef(main$model),lme4::fixef(reference),tolerance=0)
    expect_equal(as.numeric(predict(main$model)),as.numeric(predict(reference)),tolerance=0)
    tables = function()lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$asDF)
    numbers = tables()
    main_plot = main$displayPlot
    comparison_plot = comparison$displayPlot
    seed = .Random.seed
    labels = options$plotLabels
    labels[[1]]$label = "Main time"
    expect_true(jamovi_reporting_update(analysis,list(plotLabels=labels)))
    expect_identical(fits,2L)
    expect_identical(analysis$results$plot$state$displayPlot,main_plot)
    expect_identical(analysis$results$comparisonplot$state$displayPlot,comparison_plot)
    expect_identical(.Random.seed,seed)
    expect_false(any(vapply(options$comparisonPlotLabels,function(row)row$label=="Main time",logical(1))))
    expect_true(jamovi_reporting_update(analysis,list(predictionType="cas",plotPanels="a")))
    cas = analysis$results$plot$state$displayPlot
    expect_true(is.data.frame(attr(cas,"cas_prediction")))
    expect_match(analysis$results$plotExplanation$content,"Dashed gray lines",fixed=TRUE)
    expect_false(options$casGhost)
    expect_identical(analysis$results$comparisonplot$state$displayPlot,comparison_plot)
    expect_identical(fits,2L)
    expect_true(jamovi_reporting_update(analysis,list(comparisonPredictionType="cas",comparisonPanels="z",casGhost=TRUE)))
    expect_identical(analysis$results$plot$state$displayPlot,cas)
    expect_true(attr(analysis$results$comparisonplot$state$displayPlot,"jamovi_cas_ghost"))
    expect_match(analysis$results$comparisonPlotNote$content,"Model 2",fixed=TRUE)
    for (visibility in list(list(graphic=FALSE),list(graphic=TRUE,comparisonGraphic=FALSE),list(comparisonGraphic=TRUE))) {
      expect_true(jamovi_reporting_update(analysis,visibility))
      expect_identical(analysis$results$plot$visible,options$graphic)
      expect_identical(analysis$results$comparisonplot$visible,options$comparisonGraphic)
      expect_identical(analysis$results$plotExplanation$visible,options$graphic)
      expect_equal(tables(),numbers,tolerance=0)
      expect_identical(fits,2L)
    }
  }
})

test_that("ordinary CAS ghosts copy supplied fixed curves without changing adjusted observations", {
  data = jamovi_mixed_test_data()
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",
    modelTerms=list("x","a","z",c("x","a")),interactions=TRUE,quadratic="x",polynomials=TRUE,
    randomSlopes="x",predictionType="cas",plotX="x",plotPanels=c("a","z"))
  state = flexplot:::jamovi_mixed_fit(options,data)
  base = flexplot:::jamovi_mixed_display(state,options)
  state$displayPlot = base
  state$displayLayout = list(x="x",panels=c("a","z"),group=NULL,type="cas",clusters=3)
  option = options$option("ghostLines");option$value = TRUE
  plot = flexplot:::jamovi_mixed_display(state,options)
  expect_identical(plot$data,base$data)
  expect_identical(attr(plot,"cas_prediction"),attr(base,"cas_prediction"))
  reference = attr(plot,"ghost_reference")
  expect_true(reference$drawable)
  ghosts = vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))
  expect_identical(lapply(plot$layers[!ghosts],function(layer)layer$data),lapply(base$layers,function(layer)layer$data))
  expect_equal(sum(ghosts),1L)
  source = attr(base,"cas_prediction")
  for(i in seq_along(reference$columns)) source = source[as.character(source[[reference$columns[i]]]) %in% reference$levels[[i]],]
  expected = flexplot:::flexplot_ghost_prediction_data(source,"x",c("a","z"),base$data)
  expect_identical(plot$layers[[which(ghosts)]]$data,expected)
  expect_match(flexplot:::jamovi_model_ghost_note(plot),"a = low",fixed=TRUE)
  expect_null(attr(plot,"jamovi_cas_ghost"))
  expect_false(options$casGhost)
  option = options$option("plotGroup");option$value = "a"
  option = options$option("plotPanels");option$value = "z"
  invalid = flexplot:::jamovi_mixed_display(state,options)
  expect_match(attr(invalid,"jamovi_ghost_note"),"Use panels",fixed=TRUE)
})

test_that("simultaneous conditional plots retain independent sampled-cluster selections", {
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",randomSlopes="x",
    modelTerms=list("x","a","z"),compare=TRUE,comparisonTerms=list("x","a"),comparisonRandomSlopes="x",
    plotX="x",plotPanels="id",predictionType="cluster",clusters=3,
    comparisonX="x",comparisonPanels="id",comparisonPredictionType="cluster",comparisonClusters=5)
  analysis = flexplot:::mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
  expect_true(analysis$run())
  curves = function(plot) Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
  groups = function(plot) unique(unlist(lapply(curves(plot),function(layer)as.character(layer$data$id))))
  main = analysis$results$plot$state$displayPlot
  comparison = analysis$results$comparisonplot$state$displayPlot
  expect_length(groups(main),3L);expect_length(groups(comparison),5L)
  expect_true(jamovi_reporting_update(analysis,list(clusters=2)))
  expect_length(groups(analysis$results$plot$state$displayPlot),2L)
  expect_identical(analysis$results$comparisonplot$state$displayPlot,comparison)
  main = analysis$results$plot$state$displayPlot
  expect_true(jamovi_reporting_update(analysis,list(comparisonClusters=4)))
  expect_length(groups(analysis$results$comparisonplot$state$displayPlot),4L)
  expect_identical(analysis$results$plot$state$displayPlot,main)
})

test_that("Mixed plot errors use their adjacent guide without suppressing the other fit or plot", {
  skip_if_not_installed("jmvcore")
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",
    compare=TRUE,comparisonTerms=list("x","a","z"),plotX="x",plotPanels="a",
    predictionType="population",comparisonX="x",comparisonPanels="a",comparisonPredictionType="population")
  analysis = flexplot:::mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
  expect_true(analysis$run())
  tables = function() lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$asDF)
  expected = tables()
  other = analysis$results$comparisonplot$state$displayPlot
  expect_true(jamovi_reporting_update(analysis,list(predictionType="cluster")))
  expect_null(analysis$results$plot$state)
  expect_match(analysis$results$plot$.__enclos_env__$private$.error,"place the cluster variable",fixed=TRUE)
  expect_true(analysis$results$plotExplanation$visible)
  expect_match(analysis$results$plotExplanation$content,"Model plot unavailable:",fixed=TRUE)
  expect_match(analysis$results$plotExplanation$content,"place the cluster variable",fixed=TRUE)
  expect_false(grepl("Curves use",analysis$results$plotExplanation$content,fixed=TRUE))
  expect_identical(analysis$results$comparisonplot$state$displayPlot,other)
  expect_equal(tables(),expected,tolerance=0)
  expect_true(jamovi_reporting_update(analysis,list(predictionType="population",comparisonPredictionType="cluster")))
  expect_identical(analysis$results$plotExplanation$content,"")
  expect_false(analysis$results$plotExplanation$visible)
  expect_false(grepl("unavailable",analysis$results$plotExplanation$content,fixed=TRUE))
  expect_null(analysis$results$comparisonplot$state)
  expect_match(analysis$results$comparisonPlotNote$content,"place the shared cluster variable",fixed=TRUE)
  expect_equal(tables(),expected,tolerance=0)
  expect_true(jamovi_reporting_update(analysis,list(comparisonPredictionType="population")))
  expect_true(analysis$results$plot$visible)
  expect_true(analysis$results$comparisonplot$visible)
  expect_identical(analysis$results$comparisonPlotNote$content,"")
  expect_false(analysis$results$comparisonPlotNote$visible)
  expect_equal(tables(),expected,tolerance=0)
  expect_true(jamovi_reporting_update(analysis,list(graphic=FALSE,comparisonGraphic=FALSE)))
  expect_false(analysis$results$plotExplanation$visible)
  expect_false(analysis$results$comparisonPlotNote$visible)
  expect_false(grepl("unavailable",analysis$results$plotExplanation$content,fixed=TRUE))
  expect_identical(analysis$results$comparisonPlotNote$content,"")
})

test_that("late Mixed main rendering errors replace the guide and recover on recalculation", {
  skip_if_not_installed("jmvcore")
  analysis = flexplot:::mixedmodelClass$new(options=flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a"),cluster="id",
    plotX="x",plotPanels="a",predictionType="population",compare=TRUE,comparisonTerms=list("x","a"),
    comparisonX="x",comparisonPanels="a",comparisonPredictionType="population"),data=jamovi_mixed_test_data())
  expect_true(analysis$run())
  numbers = analysis$results$fixed$asDF
  analysis$results$plot$.setPath("stale-main.png")
  local_mocked_bindings(jamovi_mixed_apply_labels=function(...)stop("Injected <render> & failure"),.package="flexplot")
  expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_null(analysis$results$plot$state)
  expect_null(analysis$results$plot$.__enclos_env__$private$.filePath)
  expect_true(analysis$results$plotExplanation$visible)
  expect_match(analysis$results$plotExplanation$content,"Injected &lt;render&gt; &amp; failure",fixed=TRUE)
  expect_equal(analysis$results$fixed$asDF,numbers,tolerance=0)
  analysis$results$comparisonplot$.setPath("stale-comparison.png")
  expect_false(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_null(analysis$results$comparisonplot$state)
  expect_null(analysis$results$comparisonplot$.__enclos_env__$private$.filePath)
  expect_match(analysis$results$comparisonPlotNote$content,"Injected &lt;render&gt; &amp; failure",fixed=TRUE)
  expect_equal(analysis$results$fixed$asDF,numbers,tolerance=0)
  expect_true(jamovi_reporting_update(analysis,list(graphic=FALSE)))
  expect_false(analysis$results$plotExplanation$visible)
  expect_false(grepl("unavailable",analysis$results$plotExplanation$content,fixed=TRUE))
})
