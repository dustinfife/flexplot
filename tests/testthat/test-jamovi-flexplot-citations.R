expect_flexplot_bibliography_only = function(analysis) {
  response = analysis$asProtoBuf(includeState=FALSE)
  expect_identical(vapply(response$references,function(ref)ref$name,character(1)),
    c("flexplot","graphicalAnalysis"))
  expect_true(all(vapply(response$references,function(ref)nzchar(ref$title) && ref$year>0L,logical(1))))
  walk = function(element) {
    expect_length(element$refs,0L)
    if (element$has("group")) for (child in element$group$elements) walk(child)
  }
  walk(response$results)
  expect_length(analysis$results$getRefs(),0L)
  expect_identical(analysis$results$getRefs(recurse=TRUE),c("flexplot","graphicalAnalysis"))
  invisible(response)
}

test_that("ordinary Flexplot retains full bibliography without plot or empty-analysis callouts", {
  skip_if_not_installed("jmvcore")
  jmvcore:::initProtoBuf()
  empty = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(),
    data=NULL,analysisId=1L,datasetId="citation-review")
  empty$.setReadDatasetSource(function(columns)data.frame())
  empty$.setReadDatasetHeaderSource(function(columns)data.frame())
  empty$init()
  expect_true(empty$results$waiting$visible)
  expect_flexplot_bibliography_only(empty)
  analysis = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(
    out="y",preds="x",given="a",ghost=FALSE),data=small,analysisId=2L,datasetId="citation-review")
  expect_true(analysis$run())
  before = analysis$results$plot$state
  response = expect_flexplot_bibliography_only(analysis)
  expect_identical(analysis$results$plot$state,before)
  expect_true(analysis$results$plot$visible)
  expect_identical(all.vars(before$formula),c("y","x","a"))
  # Old files carry root marker references. The current public collection policy
  # must still apply after cached results are restored, without rebuilding items.
  response$results$refs = c("flexplot","graphicalAnalysis")
  restored = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(
    out="y",preds="x",given="a",ghost=FALSE),data=small,analysisId=3L,datasetId="citation-review")
  objects = restored$results$items
  restored$results$fromProtoBuf(response$results,character(),character())
  expect_identical(restored$results$items,objects)
  expect_true(restored$run())
  expect_flexplot_bibliography_only(restored)
  expect_identical(restored$results$itemNames,c("waiting","explanation","plot"))
  # Constructing another ordinary analysis does not change an earlier instance.
  expect_flexplot_bibliography_only(analysis)
  lm = flexplot:::glmbasicResults$new(flexplot:::glmbasicOptions$new())
  expect_identical(lm$methodReferences$getRefs(),c("flexplot","graphicalAnalysis"))
})
