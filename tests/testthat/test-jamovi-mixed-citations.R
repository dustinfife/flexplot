expect_mixed_bibliography_only = function(analysis) {
  response = analysis$asProtoBuf(includeState=FALSE)
  expect_identical(vapply(response$references,function(ref)ref$name,character(1)),
    c("flexplot","graphicalAnalysis"))
  expect_true(all(vapply(response$references,function(ref)nzchar(ref$title) && ref$year>0L,logical(1))))
  walk = function(element) {
    expect_length(element$refs,0L)
    if(element$has("group"))for(child in element$group$elements)walk(child)
  }
  walk(response$results)
  expect_length(analysis$results$getRefs(),0L)
  expect_identical(analysis$results$getRefs(recurse=TRUE),c("flexplot","graphicalAnalysis"))
  invisible(response)
}

test_that("Mixed Models retains bibliography without waiting-message or fitted-result callouts", {
  jmvcore:::initProtoBuf()
  empty = flexplot:::mixedmodelClass$new(options=flexplot:::mixedmodelOptions$new(),data=NULL,
    analysisId=1L,datasetId="mixed-citation-review")
  empty$.setReadDatasetSource(function(columns)data.frame())
  empty$.setReadDatasetHeaderSource(function(columns)data.frame())
  empty$init()
  expect_true(empty$results$waiting$visible)
  expect_mixed_bibliography_only(empty)
  data = jamovi_mixed_test_data()
  for (compare in c(FALSE,TRUE)) {
    make = function() flexplot:::mixedmodelClass$new(options=flexplot:::mixedmodelOptions$new(
      out="y",preds=c("x","a"),cluster="id",modelTerms=list("x","a"),
      compare=compare,comparisonFixed="x",comparisonTerms=list("x"),graphic=FALSE,comparisonGraphic=FALSE),
      data=data,analysisId=2L,datasetId="mixed-citation-review")
    analysis = make()
    expect_true(analysis$run())
    response = expect_mixed_bibliography_only(analysis)
    numbers = function(results)lapply(Filter(function(item)inherits(item,"Table"),results$items),function(item)item$asDF)
    before = numbers(analysis$results)
    # Restore the old root callouts without replacing result objects.
    response$results$refs = c("flexplot","graphicalAnalysis")
    restored = make()
    objects = restored$results$items
    restored$results$fromProtoBuf(response$results,character(),character())
    expect_identical(restored$results$items,objects)
    expect_mixed_bibliography_only(restored)
    expect_true(restored$run())
    expect_mixed_bibliography_only(restored)
    expect_equal(numbers(restored$results),before,tolerance=0)
    expect_mixed_bibliography_only(analysis)
  }
})
