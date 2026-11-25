test_that("legacy Mixed layouts and all retained label keys migrate once through real protobuf", {
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (compare in c(FALSE,TRUE)) for(type in c("population","cluster","cas")) for(revision in 0:1) {
    row = flexplot:::jamovi_mixed_label_row("x","x",display="X-axis label")
    row$label = "Legacy time"
    absent = row;absent$key = "temporarily unavailable";absent$label = "Retained"
    saved = list(out="y",preds=c("x","z"),cluster="id",compare=compare,plotX="x",plotPanels=c("id","z"),
      plotGroup="z",predictionType=type,clusters=7L,casGhost=TRUE,ghostLines=TRUE,
      plotLabels=list(row),plotLabelStore=list(row,absent),outputOptionsVersion=revision,
      graphic=FALSE,comparisonGraphic=TRUE,comparisonStatistics=FALSE)
    pb$names = names(saved)
    pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
    flexplot:::jamovi_output_migrate(options,"mixedmodel")
    for(pair in list(c("comparisonX","plotX"),c("comparisonPanels","plotPanels"),c("comparisonGroup","plotGroup"),
        c("comparisonPredictionType","predictionType"),c("comparisonClusters","clusters"),
        c("comparisonPlotLabels","plotLabels"),c("comparisonPlotLabelStore","plotLabelStore")))
      expect_equal(options[[pair[1]]],options[[pair[2]]])
    expect_true(options$ghostLines);expect_true(options$casGhost)
    expect_false(options$graphic)
    expect_identical(options$comparisonGraphic,revision==1L)
    expect_false(options$comparisonStatistics)
    expect_identical(options$outputOptionsVersion,2L)
    reopened = flexplot:::mixedmodelOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
    expect_equal(reopened$comparisonPlotLabelStore,list(row,absent))
    expect_equal(reopened$comparisonPanels,c("id","z"))
    flexplot:::jamovi_output_option(reopened,"comparisonPredictionType","cas")
    flexplot:::jamovi_output_option(reopened,"predictionType","population")
    flexplot:::jamovi_output_migrate(reopened,"mixedmodel")
    expect_identical(reopened$comparisonPredictionType,"cas")
    expect_identical(reopened$predictionType,"population")
  }
})

test_that("new independent Mixed options and label arrays survive protobuf restoration", {
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  values = list(outputOptionsVersion=2L,plotX="x",plotPanels="a",predictionType="population",clusters=3L,
    comparisonX="z",comparisonPanels="id",comparisonPredictionType="cluster",comparisonClusters=6L,
    plotLabels=list(),comparisonPlotLabels=list(),plotLabelStore=list(),comparisonPlotLabelStore=list())
  pb$names = names(values);pb$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
  row = flexplot:::jamovi_mixed_label_row("x","z",display="X-axis label");row$label = "Comparison age"
  flexplot:::jamovi_plot_publish_labels(options,"comparisonPlotLabels",list(row))
  flexplot:::jamovi_plot_publish_labels(options,"comparisonPlotLabelStore",list(row))
  flexplot:::jamovi_output_migrate(options,"mixedmodel")
  restored = flexplot:::mixedmodelOptions$new();restored$fromProtoBuf(options$asProtoBuf())
  expect_equal(restored$comparisonPlotLabels,list(row))
  expect_equal(restored$plotLabels,list())
  expect_identical(restored$plotX,"x");expect_identical(restored$comparisonX,"z")
  expect_identical(restored$predictionType,"population");expect_identical(restored$comparisonPredictionType,"cluster")
  expect_equal(restored$clusters,3);expect_equal(restored$comparisonClusters,6)
})

test_that("both restored Mixed plot states reuse fits and their selected clusters", {
  jmvcore:::initProtoBuf()
  data = jamovi_mixed_test_data()
  saved = list(out="y",preds=c("x","a","z"),cluster="id",modelTerms=list("x","a","z"),compare=TRUE,
    randomSlopes="x",comparisonTerms=list("x","a"),comparisonRandomSlopes="x",
    graphic=TRUE,comparisonGraphic=TRUE,predictionType="cluster",plotX="x",plotPanels="id",clusters=4L,
    comparisonPredictionType="cas",comparisonX="x",comparisonPanels="a",casGhost=TRUE,
    outputOptionsVersion=2L,plotLabelEditorVersion=1L)
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  host = function(options) {
    analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    analysis
  }
  options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
  original = host(options)
  expect_true(original$run())
  before = original$results$plot$state$displayPlot
  comparison = original$results$comparisonplot$state$displayPlot
  message = original$results$asProtoBuf(status=4L,includeState=TRUE)
  options2 = flexplot:::mixedmodelOptions$new();options2$fromProtoBuf(options$asProtoBuf())
  reopened = host(options2)
  reopened$results$fromProtoBuf(message,character(),character())
  local_mocked_bindings(jamovi_mixed_fit=function(...)stop("Unexpected fit after restore"),
    jamovi_mixed_comparison=function(...)stop("Unexpected comparison fit after restore"),.package="flexplot")
  seed = .Random.seed
  expect_true(reopened$run())
  expect_true(reopened$results$plot$visible);expect_true(reopened$results$comparisonplot$visible)
  expect_identical(.Random.seed,seed)
  layers = function(plot) lapply(plot$layers,function(layer)if(is.data.frame(layer$data))layer$data else NULL)
  expect_equal(layers(reopened$results$plot$state$displayPlot),layers(before),tolerance=0)
  children = function(plot)lapply(attr(plot,"jamovi_cas_children"),layers)
  expect_equal(children(reopened$results$comparisonplot$state$displayPlot),children(comparison),tolerance=0)
  expect_identical(reopened$results$plot$state$model,reopened$results$comparisonplot$state$model1)
  expect_true(jamovi_reporting_update(reopened,list(graphic=FALSE,comparisonClusters=5L)))
  expect_false(reopened$results$plot$visible);expect_true(reopened$results$comparisonplot$visible)
})

test_that("restored Mixed rendering diagnostics retain result objects, fits and recovery", {
  jmvcore:::initProtoBuf()
  saved = list(out="y",preds=c("x","a"),cluster="id",compare=TRUE,
    comparisonTerms=list("x","a"),plotX="x",plotPanels="a",predictionType="cluster",
    comparisonX="x",comparisonPanels="a",comparisonPredictionType="population")
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
  host = function(options) {
    analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=jamovi_mixed_test_data()),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    analysis
  }
  first = host(options)
  expect_true(first$run())
  before = first$results$fixed$asDF
  message = first$results$asProtoBuf(status=4L,includeState=TRUE)
  image = message$group$elements[[which(vapply(message$group$elements,function(x)x$name=="plot",logical(1)))]]
  expect_match(image$error$message,"place the grouping variable",fixed=TRUE)
  expect_identical(image$image$path,"")
  restored_options = flexplot:::mixedmodelOptions$new();restored_options$fromProtoBuf(options$asProtoBuf())
  reopened = host(restored_options)
  note = reopened$results$plotExplanation
  reopened$results$fromProtoBuf(message,character(),character())
  expect_true(reopened$run())
  expect_identical(reopened$results$plotExplanation,note)
  expect_true(note$visible)
  expect_match(note$content,"Model plot unavailable:",fixed=TRUE)
  expect_true(reopened$results$comparisonplot$visible)
  expect_equal(reopened$results$fixed$asDF,before,tolerance=0)
  expect_true(jamovi_reporting_update(reopened,list(predictionType="population")))
  expect_false(grepl("unavailable",note$content,fixed=TRUE))
  expect_true(reopened$results$plot$visible)
  expect_equal(reopened$results$fixed$asDF,before,tolerance=0)
})
