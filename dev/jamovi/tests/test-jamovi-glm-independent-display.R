test_that("real GLM protobuf migration copies shared displays once and retains explicit independent choices", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (revision in 0:2) for (style in c("auto","raw")) {
    saved = list(out="y_bin",preds=c("x","a"),modelTerms=list("x","a"),comparisonTerms=list("x"),
      family="binomial",plotX="x",comparisonX="x",compare=FALSE,se=FALSE,plotStyle=style,plotScale="logit",
      graphic=FALSE,comparisonGraphic=TRUE,comparisonStatistics=FALSE,
      plotLabelStore=list(list(key="retained",role="title",variable="",value="",display="Title",label="Main title")),
      comparisonPlotLabelStore=list(list(key="retained-comparison",role="title",variable="",value="",display="Title",label="Comparison title")))
    if (revision) saved$outputOptionsVersion = revision
    if (revision==2) saved = c(saved,list(comparisonSe=TRUE,comparisonPlotStyle="raw",comparisonPlotScale="probability"))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(message)
    analysis = jamovi_reporting_host(options,small)
    expect_true(analysis$run())
    expect_equal(options$outputOptionsVersion,2L)
    expect_identical(options$comparisonSe,revision==2)
    expect_identical(options$comparisonPlotStyle,if(revision==2) "raw" else style)
    expect_identical(options$comparisonPlotScale,if(revision==2) "probability" else "logit")
    expect_identical(options$comparisonGraphic,revision>0)
    expect_false(options$comparisonStatistics)
    reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
    for (name in c("outputOptionsVersion","se","plotStyle","plotScale","comparisonSe","comparisonPlotStyle",
        "comparisonPlotScale","graphic","comparisonGraphic","comparisonStatistics","plotLabelStore","comparisonPlotLabelStore"))
      expect_equal(reopened[[name]],options[[name]])
    before = lapply(c("comparisonSe","comparisonPlotStyle","comparisonPlotScale"),function(name)options[[name]])
    expect_true(jamovi_reporting_update(analysis,list(se=TRUE,plotStyle="raw",plotScale="probability",compare=TRUE)))
    expect_identical(lapply(c("comparisonSe","comparisonPlotStyle","comparisonPlotScale"),function(name)options[[name]]),before)
    expect_identical(options$plotLabelStore[[1]]$label,"Main title")
    expect_identical(options$comparisonPlotLabelStore[[1]]$label,"Comparison title")
  }
  options = flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),family="binomial",
    se=FALSE,plotScale="logit",comparisonSe=TRUE,comparisonPlotScale="probability")
  analysis = jamovi_reporting_host(options,small)
  expect_true(analysis$run())
  expect_false(options$se);expect_true(options$comparisonSe)
  expect_identical(options$comparisonPlotScale,"probability")
})
