test_that("Mixed inference preferences restore through real native protobuf options", {
  jmvcore:::initProtoBuf()
  pb=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for(ci in list(NULL,FALSE,TRUE))for(comparison in c(FALSE,TRUE)) {
    saved=list(out="y",preds=c("x","z"),cluster="id",modelTerms=list("x","z"),comparisonFixed="x",
      comparisonTerms=list("x"),compare=comparison,includeP=TRUE,graphic=FALSE,comparisonGraphic=FALSE)
    if(!is.null(ci))saved$includeCI=ci
    pb$names=names(saved);pb$options=lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options=flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
    expect_identical(options$includeCI,!identical(ci,FALSE))
    roundtrip=flexplot:::mixedmodelOptions$new();roundtrip$fromProtoBuf(options$asProtoBuf())
    expect_identical(roundtrip$includeCI,!identical(ci,FALSE))
    expect_true(roundtrip$includeP)
    analysis=flexplot:::mixedmodelClass$new(options=roundtrip,data=NULL)
    reader=jamovi_reporting_read;environment(reader)=list2env(list(data=jamovi_mixed_test_data()),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    # Simulate legacy cached presentation: empty p columns and obsolete notes.
    for(name in c("fixed","fixed2")) {
      analysis$results[[name]]$getColumn("p")$setVisible(TRUE)
      analysis$results[[name]]$setNote("p","Old unavailable p-value note")
      analysis$results[[name]]$setNote("intervals","Old prediction-interval wording")
    }
    analysis$results$random$setNote("old","")
    cached=analysis$results$asProtoBuf(status=4L,includeState=FALSE)
    objects=analysis$results$items
    analysis$results$fromProtoBuf(cached,character(),character())
    expect_identical(analysis$results$items,objects)
    expect_true(analysis$run())
    for(name in c("fixed","fixed2")) {
      expect_false(analysis$results[[name]]$getColumn("p")$visible)
      expect_null(analysis$results[[name]]$notes$p)
    }
    expect_identical(analysis$results$fixed$getColumn("lower")$visible,!identical(ci,FALSE))
    expect_null(analysis$results$fixed$notes$intervals)
    expect_length(analysis$results$random$notes,0L)
    expect_true(jamovi_reporting_update(analysis,list(compare=!comparison)))
    expect_true(roundtrip$includeP)
    expect_identical(roundtrip$includeCI,!identical(ci,FALSE))
    expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
    expect_true(analysis$results$comparison$getColumn("p")$visible)
    expect_false(analysis$results$fixed2$getColumn("p")$visible)
  }
})
