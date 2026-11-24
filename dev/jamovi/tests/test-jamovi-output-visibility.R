test_that("native saved output revisions preserve old effective visibility and explicit current choices", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) for (graphic in c(FALSE,TRUE)) {
    target = if (module=="glmbasic") 1L else 2L
    values = list(out="y",preds=c("x","z"),modelTerms=list("x","z"),comparisonTerms=list("x"),
      graphic=graphic,compare=FALSE,plotX="x")
    data = small
    if (module=="mixedmodel") {
      values = c(values,list(cluster="id",showCorrelations=TRUE,randomSlopes="x",comparisonRandomSlopes="x",plotPanels="id"))
      data = jamovi_mixed_test_data()
    } else values$comparisonX = "x"
    if (module=="glmgeneralized") values = c(values,list(family="gaussian",showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE))
    for (current in c(FALSE,TRUE)) {
      saved = values
      if (current) saved = c(saved,list(outputOptionsVersion=target,comparisonGraphic=!graphic,comparisonStatistics=FALSE))
      message$names = names(saved)
      message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
      options = get(paste0(module,"Options"),asNamespace("flexplot"))$new()
      options$fromProtoBuf(message)
      analysis = get(paste0(module,"Class"),asNamespace("flexplot"))$new(options=options,data=NULL)
      reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
      analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
      expect_true(analysis$run())
      expected = if (current) !graphic else module=="glmbasic" || graphic
      expect_identical(options$comparisonGraphic,expected)
      expect_equal(options$outputOptionsVersion,target)
      reopened = get(paste0(module,"Options"),asNamespace("flexplot"))$new()
      reopened$fromProtoBuf(options$asProtoBuf())
      expect_identical(reopened$comparisonGraphic,expected)
      expect_equal(reopened$outputOptionsVersion,target)
      expect_identical(reopened$graphic,graphic)
      expect_identical(reopened$comparisonStatistics,!current)
      expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
      expect_identical(analysis$results$comparisonplot$visible,expected)
      expect_identical(analysis$results$comparison$visible,!current)
      if (module=="glmgeneralized") for (name in c("showEstimates","showIRT","showProbabilities")) expect_false(reopened[[name]])
      if (module=="mixedmodel") expect_true(reopened$showCorrelations)
    }
  }
})

test_that("Mixed revision 2 preserves divergent settings and retained labels on repeated reopen", {
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  row = flexplot:::jamovi_mixed_label_row("x","x",display="X-axis label")
  row$label = "Legacy time"
  retained = row; retained$key = "temporarily absent"; retained$label = "Keep this edit"
  saved = list(outputOptionsVersion=1L,plotX="x",plotPanels="a",predictionType="population",clusters=3L,
    plotLabels=list(row),plotLabelStore=list(row,retained),graphic=FALSE,comparisonGraphic=TRUE,
    comparisonStatistics=FALSE,ghostLines=TRUE,casGhost=TRUE,includeP=TRUE,includeCI=FALSE)
  pb$names = names(saved)
  pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  options = flexplot:::mixedmodelOptions$new(); options$fromProtoBuf(pb)
  flexplot:::jamovi_output_migrate(options,"mixedmodel")
  expect_identical(options$outputOptionsVersion,2L)
  for (pair in list(c("comparisonX","plotX"),c("comparisonPanels","plotPanels"),
      c("comparisonPredictionType","predictionType"),c("comparisonClusters","clusters"),
      c("comparisonPlotLabels","plotLabels"),c("comparisonPlotLabelStore","plotLabelStore")))
    expect_equal(options[[pair[1]]],options[[pair[2]]])
  divergent = list(comparisonX="z",comparisonPanels="id",comparisonPredictionType="cluster",comparisonClusters=7L)
  for (name in names(divergent)) flexplot:::jamovi_output_option(options,name,divergent[[name]])
  comparison_row = row; comparison_row$label = "Comparison age"
  flexplot:::jamovi_plot_publish_labels(options,"comparisonPlotLabels",list(comparison_row))
  flexplot:::jamovi_plot_publish_labels(options,"comparisonPlotLabelStore",list(comparison_row,retained))
  for (i in 1:2) {
    reopened = flexplot:::mixedmodelOptions$new(); reopened$fromProtoBuf(options$asProtoBuf())
    flexplot:::jamovi_output_migrate(reopened,"mixedmodel")
    expect_equal(reopened$outputOptionsVersion,2L)
    for (name in names(divergent)) expect_equal(reopened[[name]],divergent[[name]])
    for (name in setdiff(names(saved),"outputOptionsVersion")) expect_equal(reopened[[name]],saved[[name]])
    expect_equal(reopened$comparisonPlotLabels,list(comparison_row))
    expect_equal(reopened$comparisonPlotLabelStore,list(comparison_row,retained))
    options = reopened
  }
})
