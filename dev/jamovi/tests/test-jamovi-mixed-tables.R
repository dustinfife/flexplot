test_that("saved output options restore both model tables with serialized visibility", {
  jmvcore:::initProtoBuf()
  saved = list(out="y",preds=c("x","z"),cluster="id",modelTerms=list("x"),compare=TRUE,
    comparisonFixed=c("x","z"),comparisonTerms=list("x","z"),comparisonRandomSlopes="x",
    includeCI=FALSE,includeP=TRUE,showCorrelations=TRUE,graphic=FALSE,samplePolicyVersion=1L)
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  message$names = names(saved)
  message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(message)
  restored = flexplot:::mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
  expect_true(restored$run())
  for(name in c("fixed","fixed2")) {
    expect_true(restored$results[[name]]$visible)
    columns = restored$results[[name]]$asProtoBuf()$table$columns
    visibility = setNames(vapply(columns,function(column)column$visible,integer(1)),vapply(columns,function(column)column$name,character(1)))
    enum = get("jamovi.coms.Visible",asNamespace("jmvcore"))
    expect_equal(visibility[c("lower","upper","p")],c(lower=enum$NO,upper=enum$NO,p=enum$YES))
  }
  expect_mixed_tables(restored$results,restored$results$comparisonplot$state$model2,second=TRUE)
  legacy = flexplot:::mixedmodelOptions$new()
  expect_true(legacy$includeCI)
})
