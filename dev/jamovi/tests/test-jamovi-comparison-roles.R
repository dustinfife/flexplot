test_that("legacy grouping and derived magnet capability round-trip through native options", {
  jmvcore:::initProtoBuf()
  data = withr::with_seed(875, data.frame(y=rnorm(160),x=rnorm(160),z=rnorm(160),
    a=factor(rep(c("A","B"),80)),b=factor(rep(c("low","high"),each=80)),binary=rbinom(160,1,.45)))
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (outcome in c("binary","y")) {
    values = list(out=outcome,preds=c("a","b","z"),compare=TRUE,graphic=FALSE,comparisonX="a",
      modelTerms=list("a","b","z"),comparisonTerms=list("a","z"),comparisonGroup="b",
      comparisonMagnet=TRUE) # A stale capability must not grant eligibility.
    message$names = names(values)
    message$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmbasicOptions$new();options$fromProtoBuf(message)
    analysis = flexplot:::glmbasicClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    expect_identical(options$comparisonGroup,"b")
    expect_identical(options$comparisonMagnet,outcome=="binary")
    reopened = flexplot:::glmbasicOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
    expect_identical(reopened$comparisonMagnet,outcome=="binary")
    expect_identical(reopened$comparisonGroup,"b")
    expect_identical(reopened$modelTerms,values$modelTerms)
    image = analysis$results$comparisonplot
    withr::local_options(device=function(...) grDevices::pdf(file=NULL))
    expect_true(analysis$.__enclos_env__$private$.comparisonplot(image))
    element = image$asProtoBuf(status=4L,includeState=FALSE)
    expect_false(as.integer(element$status)==5L) # Native ANALYSIS_RENDERING.
  }
  mixed = jamovi_mixed_analysis(graphic=TRUE,compare=TRUE,predictionType="cluster",plotX="x",
    plotGroup="id",plotPanels=NULL,modelTerms=list("x","z"),comparisonTerms=list("x"))
  expect_true(mixed$run())
  expect_true(mixed$results$comparisonPlotNote$visible)
  expect_match(mixed$results$comparisonPlotNote$content,"in panels",fixed=TRUE)
  image = mixed$results$comparisonplot
  expect_null(image$state)
  expect_false(as.integer(image$asProtoBuf(status=4L,includeState=FALSE)$status)==5L)
  expect_equal(nrow(mixed$results$comparison$asDF),2L)
})
