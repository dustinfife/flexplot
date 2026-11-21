test_that("new and old saved GLMs retain compatible output defaults and explicit choices", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  base = list(out="y_bin",preds=c("x","a"),family="binomial",graphic=FALSE)
  for (choices in list(list(),list(showEstimates=FALSE,showIRT=TRUE,showProbabilities=FALSE),
      list(showEstimates=TRUE,showIRT=FALSE,showProbabilities=TRUE))) {
    saved = c(base,choices)
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(message)
    reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
    expected = utils::modifyList(list(showEstimates=TRUE,showIRT=TRUE,showProbabilities=TRUE),choices)
    for (name in names(expected)) expect_identical(reopened[[name]],expected[[name]])
    analysis = jamovi_reporting_host(reopened,small)
    expect_true(analysis$run())
    for (pair in list(c("coefficients","showEstimates"),c("irt","showIRT"),c("probabilities","showProbabilities")))
      expect_identical(analysis$results[[pair[1]]]$visible,expected[[pair[2]]])
  }
})
