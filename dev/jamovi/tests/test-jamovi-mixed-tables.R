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
    expect_equal(visibility[c("lower","upper","p")],c(lower=enum$NO,upper=enum$NO,p=enum$NO))
    expect_identical(restored$results[[name]]$getColumn("p")$name,"p")
    expect_false(restored$results[[name]]$getColumn("p")$visible)
  }
  expect_mixed_tables(restored$results,restored$results$comparisonplot$state$model2,second=TRUE)
  legacy = flexplot:::mixedmodelOptions$new()
  expect_true(legacy$includeCI)
})

test_that("Mixed comparison p values remain independent of hidden fixed-effect p columns", {
  analysis = jamovi_mixed_comparison_analysis(includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  models = lapply(list(state$model1,state$model2),lme4::refitML)
  likelihood = lapply(models,logLik)
  lr = 2*as.numeric(likelihood[[1]]-likelihood[[2]])
  df = attr(likelihood[[1]],"df")-attr(likelihood[[2]],"df")
  expected = pchisq(lr,df=df,lower.tail=FALSE)
  expect_true(is.finite(expected))
  for (include in c(TRUE,FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(includeP=include)))
    for (name in c("fixed","fixed2")) {
      expect_identical(analysis$results[[name]]$getColumn("p")$name,"p")
      expect_false(analysis$results[[name]]$getColumn("p")$visible)
    }
    expect_identical(analysis$results$comparison$getColumn("p")$visible,include)
    expect_equal(analysis$results$comparison$getCell("p",rowNo=1)$value,expected,tolerance=1e-12)
    expect_null(analysis$results$comparison$getCell("p",rowNo=2)$value)
  }
})
