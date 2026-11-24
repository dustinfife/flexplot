test_that("failed CAS images serialize as errors rather than endless rendering", {
  jmvcore:::initProtoBuf()
  data = jamovi_mixed_cas_data()
  for (comparison in c(FALSE,TRUE)) {
    analysis = jamovi_mixed_analysis(data,out="mixed",preds=c("continuous","group"),
      graphic=TRUE,compare=comparison,predictionType="cas",plotX="id")
    expect_true(analysis$run())
    image = if (comparison) analysis$results$comparisonplot else analysis$results$plot
    expect_identical(image$status,"error")
    expect_match(image$.__enclos_env__$private$.error,"Use a model predictor on x")
    expect_null(image$state)
    expect_equal(image$asProtoBuf(status=3L,includeState=FALSE)$status,4L)
    # Also exercise errors raised during drawing after successful plot preparation.
    expect_true(jamovi_reporting_update(analysis,setNames(list("continuous"),if(comparison) "comparisonX" else "plotX")))
    expect_false(is.null(image$state))
    local_mocked_bindings(jamovi_mixed_apply_labels=function(...) stop("CAS render probe"),.package="flexplot")
    callback = if (comparison) analysis$.__enclos_env__$private$.comparisonplot else analysis$.__enclos_env__$private$.plot
    expect_false(callback(image))
    expect_match(image$.__enclos_env__$private$.error,"CAS render probe")
    expect_null(image$state)
    expect_equal(image$asProtoBuf(status=3L,includeState=FALSE)$status,4L)
  }
})
