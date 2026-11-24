test_that("Mixed comparisons restore independent second-model specifications and switch modes without stale output", {
  jmvcore:::initProtoBuf()
  saved = list(out="y",preds=c("x","a","z"),cluster="id",modelTerms=list("x","a"),
    compare=TRUE,comparisonFixed=c("x","a"),comparisonTerms=list("x","a",c("x","a")),comparisonInteractions=TRUE,
    comparisonPolynomials=TRUE,comparisonQuadratic="x",randomSlopes="x",
    comparisonRandomIntercept=FALSE,comparisonRandomSlopes="x",comparisonEstimation="ML",
    includeP=TRUE,casGhost=TRUE,graphic=TRUE,plotX="x",plotPanels="id",predictionType="population")
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  message$names=names(saved)
  message$options=lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options=flexplot:::mixedmodelOptions$new()
  options$fromProtoBuf(message)
  data=jamovi_mixed_test_data()
  analysis=flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader=jamovi_reporting_read
  environment(reader)=list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_true(options$compare)
  expect_true(options$includeP)
  expect_true(options$casGhost)
  expect_true(analysis$results$comparison$getColumn("p")$visible)
  state=analysis$results$comparisonplot$state
  independent=lme4::lmer(y~x*a+I(x^2)+(0+x|id),data,REML=FALSE)
  expect_equal(lme4::fixef(state$model2),lme4::fixef(independent),tolerance=1e-10)
  expect_identical(lme4::getME(state$model2,"cnms")[[1]],"x")
  expect_true(lme4::isREML(state$model1))
  expect_false(lme4::isREML(state$model2))
  original_primary=analysis$results$plot$state$model
  expect_true(jamovi_reporting_update(analysis,list(comparisonEstimation="REML")))
  expect_true(lme4::isREML(analysis$results$comparisonplot$state$model2))
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(original_primary),tolerance=1e-10)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_false(analysis$results$comparison$visible)
  expect_false(analysis$results$comparisonSpecification$visible)
  expect_false(analysis$results$comparisonplot$visible)
  expect_null(analysis$results$comparisonplot$state)
  expect_true(analysis$results$plot$visible)
  expect_identical(options$comparisonTerms,list("x","a",c("x","a")))
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
  expect_true(analysis$results$comparisonplot$visible)
  expect_true(analysis$results$comparison$visible)
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(original_primary),tolerance=1e-10)
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$comparison$visible)
  expect_null(analysis$results$comparisonplot$state)
})

test_that("Mixed comparison p columns and random-test notes follow options in serialized results", {
  jmvcore:::initProtoBuf()
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x"),polynomials=FALSE,quadratic=NULL,
    comparisonTerms=list("x"),comparisonRandomSlopes=NULL,includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_true(state$statistics$random_p_unavailable)
  expect_true(is.finite(state$statistics$lr))
  expect_gt(state$statistics$df,0)
  for (enabled in c(TRUE,FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(includeP=enabled)))
    table = analysis$results$comparison
    expect_identical(table$getColumn("p")$visible,enabled)
    serialized = table$asProtoBuf()$table$columns
    column = Filter(function(column) column$name == "p",serialized)[[1]]
    visibility = get("jamovi.coms.Visible",asNamespace("jmvcore"))
    expect_equal(column$visible,if (enabled) visibility$YES else visibility$NO)
    if (enabled) expect_match(table$notes$randomP$note,"^P-value unavailable") else
      expect_null(table$notes$randomP)
    expect_identical(analysis$results$comparisonplot$state$model1,state$model1)
    expect_equal(table$asDF$lr[1],state$statistics$lr)
    expect_equal(table$asDF$df[1],state$statistics$df)
  }
  # A nonnested random structure does not receive the boundary-test note.
  expect_true(jamovi_reporting_update(analysis,list(randomSlopes="x",comparisonRandomSlopes="z",includeP=TRUE)))
  expect_match(analysis$results$comparison$notes$test$note,"nonnested")
  expect_null(analysis$results$comparison$notes$randomP)
  expect_true(is.na(analysis$results$comparisonplot$state$statistics$p))
})

test_that("saved CAS ghost settings restore the same note and serialized result order", {
  jmvcore:::initProtoBuf()
  for (enabled in c(TRUE,FALSE)) {
    saved = list(out="y",preds=c("x","z"),cluster="id",modelTerms=list("x","z"),
      comparisonFixed=c("x","z"),comparisonTerms=list("z"),compare=TRUE,graphic=TRUE,
      predictionType="cas",plotX="x",casGhost=enabled)
    message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::mixedmodelOptions$new()
    options$fromProtoBuf(message)
    restored = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=jamovi_mixed_test_data()),parent=environment(reader))
    restored$.setReadDatasetSource(reader)
    restored$.setReadDatasetHeaderSource(reader)
    expect_true(restored$run())
    expect_identical(restored$results$comparisonPlotNote$visible,enabled)
    note = restored$results$comparisonPlotNote$asProtoBuf()
    expect_identical(note$html$content,if (enabled)
      flexplot:::jamovi_plot_note_paragraph("Note. Dashed line in Model 1 represents Model 2's fixed-effects predictions.") else "")
    message = restored$results$asProtoBuf(status=3L)
    items = vapply(message$group$elements,function(item)item$name,character(1))
    specification = message$group$elements[[match("comparisonSpecification",items)]]
    expect_match(specification$html$content,"</p><p>240 complete observations; 16 groups; 0 observations omitted.</p>",fixed=TRUE)
    expect_false(restored$results$explanation$visible)
    expect_identical(restored$results$explanation$content,"")
    expect_identical(match("comparisonPlotNote",items),match("comparisonplot",items)+1L)
  }
})
