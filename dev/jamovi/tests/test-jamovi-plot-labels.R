test_that("LM and GLM publish independent label editors through the real host protocol", {
  jmvcore:::initProtoBuf()
  for (module in c("glmbasic","glmgeneralized")) {
    message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    initial = list(out=if (module=="glmbasic") "y" else "y_bin",preds=c("x","a"),
      modelTerms=list("x","a"),comparisonTerms=list("x"),plotX="x",comparisonX="x",compare=TRUE,graphic=TRUE)
    if (module=="glmgeneralized") initial$family = "binomial"
    message$names = names(initial)
    message$options = lapply(initial,jamovi_mixed_saved_option,option_template=template,group_template=message)
    factory = get(paste0(module,"Options"),asNamespace("flexplot"))
    options = factory$new();options$fromProtoBuf(message)
    for (name in c("plotLabels","plotLabelStore","comparisonPlotLabels","comparisonPlotLabelStore"))
      expect_length(options[[name]],0L)
    analysis = if (module=="glmbasic") flexplot:::glmbasicClass$new(options=options,data=small) else jamovi_reporting_host(options,small)
    expect_true(analysis$run())
    for (name in c("plotLabels","comparisonPlotLabels")) {
      rows = options[[name]]
      expect_gt(length(rows),0L)
      rows[[1]]$label = paste(module,name,"saved title")
      option = options$option(name)
      option$value = rows
    }
    analysis$optionsChangedHandler(c("plotLabels","comparisonPlotLabels"))
    expect_true(analysis$run())
    reopened = factory$new();reopened$fromProtoBuf(options$asProtoBuf())
    for (name in c("plotLabels","plotLabelStore","comparisonPlotLabels","comparisonPlotLabelStore")) {
      expect_equal(reopened[[name]],options[[name]])
      expect_true(any(vapply(reopened[[name]],function(row)nzchar(row$label),logical(1))))
    }
    restored = if (module=="glmbasic") flexplot:::glmbasicClass$new(options=reopened,data=small) else jamovi_reporting_host(reopened,small)
    expect_true(restored$run())
    expect_equal(restored$options$plotLabels,options$plotLabels)
    expect_equal(restored$options$comparisonPlotLabels,options$comparisonPlotLabels)
  }
})
