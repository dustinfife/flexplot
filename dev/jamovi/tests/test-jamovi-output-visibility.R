test_that("native saved output revisions preserve old effective visibility and explicit current choices", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) for (graphic in c(FALSE,TRUE)) {
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
      if (current) saved = c(saved,list(outputOptionsVersion=1L,comparisonGraphic=!graphic,comparisonStatistics=FALSE))
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
      expect_equal(options$outputOptionsVersion,if(module=="glmgeneralized") 2L else 1L)
      reopened = get(paste0(module,"Options"),asNamespace("flexplot"))$new()
      reopened$fromProtoBuf(options$asProtoBuf())
      expect_identical(reopened$comparisonGraphic,expected)
      expect_equal(reopened$outputOptionsVersion,if(module=="glmgeneralized") 2L else 1L)
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
