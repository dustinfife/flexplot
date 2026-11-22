test_that("real saved GLM options default off and restore panel ghosts without changing models", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  base = list(out="y_bin",preds=c("x","a"),family="binomial",plotX="x",plotPanels="a",graphic=FALSE,
    modelTerms=list("x","a",c("x","a")),quadratic="x")
  for (choice in list(list(),list(ghostLines=TRUE),list(ghostLines=FALSE))) {
    saved = c(base,choice)
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(message)
    reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
    expect_identical(reopened$ghostLines,isTRUE(choice$ghostLines))
    for (name in names(base)) expect_equal(reopened[[name]],base[[name]])
    analysis = jamovi_reporting_host(reopened,small)
    expect_true(analysis$run())
    plot = flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",panels="a",ghostLines=reopened$ghostLines)
    expect_identical(any(vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))),isTRUE(choice$ghostLines))
  }
})
