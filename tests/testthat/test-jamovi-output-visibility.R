output_host = function(module, values=list(), data=small) {
  defaults = list(out="y",preds=c("x","a","z"),modelTerms=list("x","a","z"),
    comparisonTerms=list("x","a"),plotX="x",compare=TRUE)
  if (module=="mixedmodel") {
    data = jamovi_mixed_test_data()
    defaults = c(defaults,list(cluster="id",plotPanels="id",predictionType="population",randomSlopes="x",
      comparisonRandomSlopes="x",showCorrelations=TRUE,clusters=16,comparisonX="x",
      comparisonPanels="id",comparisonPredictionType="population",comparisonClusters=16))
  } else defaults$comparisonX = "x"
  if (module=="glmgeneralized") defaults$family = "gaussian"
  options = do.call(get(paste0(module,"Options"),asNamespace("flexplot"))$new,
    utils::modifyList(defaults,values,keep.null=TRUE))
  analysis = get(paste0(module,"Class"),asNamespace("flexplot"))$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  analysis
}

output_tables = function(analysis) {
  tables = Filter(function(item) inherits(item,"Table"),analysis$results$items)
  setNames(lapply(tables,function(table)table$asDF),vapply(tables,function(table)table$name,character(1)))
}

output_fit = function(analysis,module) {
  state = analysis$results$plot$state
  model = if (module=="glmbasic") lm(state$formula,state$data) else state$model
  list(formula=formula(model),coefficients=if (module=="mixedmodel") lme4::fixef(model) else coef(model),
    covariance=as.matrix(vcov(model)),predictions=as.numeric(predict(model)),rows=rownames(model.frame(model)))
}

test_that("model outputs default on and toggles independently preserve fits, tables, labels and order", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) {
    analysis = output_host(module)
    expect_true(analysis$run())
    results = analysis$results
    order = vapply(results$items,function(item)item$name,character(1))
    expect_lt(match("plot",order),match(if (module=="glmbasic") "glmcat" else if (module=="mixedmodel") "fixed" else "coefficients",order))
    expect_lt(match("comparisonplot",order),match("comparison",order))
    expect_true(results$comparisonplot$visible)
    expect_true(results$comparison$visible)
    expect_true(results$plot$visible)
    tables = output_tables(analysis); fit = output_fit(analysis,module)
    labels = list(list(key="retained",role="legendLevel",variable="a",value="high",display="retained",label="Custom"))
    expect_true(jamovi_reporting_update(analysis,list(plotLabelStore=labels)))
    for (values in list(list(graphic=FALSE),list(comparisonGraphic=FALSE),list(comparisonStatistics=FALSE),
        list(graphic=TRUE),list(comparisonGraphic=TRUE),list(comparisonStatistics=TRUE))) {
      expect_true(jamovi_reporting_update(analysis,values))
      expect_identical(results$plot$visible,analysis$options$graphic)
      expect_identical(results$comparisonplot$visible,analysis$options$comparisonGraphic)
      expect_identical(results$comparison$visible,analysis$options$comparisonStatistics)
      expect_equal(output_tables(analysis),tables)
      expect_equal(output_fit(analysis,module),fit)
      expect_identical(analysis$options$plotLabelStore[[1]]$label,"Custom")
      expect_identical(vapply(results$items,function(item)item$name,character(1)),order)
      if (!results$comparisonplot$visible && module=="glmgeneralized") expect_false(results$comparisonPlotExplanation$visible)
      if (!results$comparisonplot$visible && module=="mixedmodel") expect_false(results$comparisonPlotNote$visible)
    }
    estimates = if (module=="glmgeneralized") "showEstimates" else "estimates"
    expect_true(jamovi_reporting_update(analysis,setNames(list(FALSE),estimates)))
    names = switch(module,glmbasic=c("glmcat","factorMeans","diff","rsq"),
      glmgeneralized="coefficients",mixedmodel=c("fixed","random","fixed2","random2"))
    for (name in names) expect_false(results[[name]]$visible)
    if (module=="mixedmodel") expect_true(results$correlations$visible)
    expect_equal(output_tables(analysis),tables)
    expect_true(jamovi_reporting_update(analysis,list(comparisonGraphic=FALSE,comparisonStatistics=FALSE,compare=FALSE)))
    expect_false(results$comparisonplot$visible);expect_false(results$comparison$visible)
    expect_equal(results$comparison$rowCount,0L)
    expect_null(results$comparisonplot$state)
    if (module=="mixedmodel") expect_true(results$plot$visible)
    expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
    expect_false(results$comparisonplot$visible);expect_false(results$comparison$visible)
    expect_equal(results$comparison$asDF,tables$comparison)
    expect_true(jamovi_reporting_update(analysis,setNames(list(TRUE),estimates)))
    for (name in names) expect_true(results[[name]]$visible)
    expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
    for (item in results$items) if (inherits(item,"Table")) expect_equal(item$rowCount,0L)
  }
})

test_that("plot visibility clears cached files and rendering restores current output", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) {
    analysis = output_host(module)
    expect_true(analysis$run())
    image = analysis$results$comparisonplot
    image$.setPath("old-image.png")
    expect_true(jamovi_reporting_update(analysis,list(comparisonGraphic=FALSE)))
    expect_false(image$visible)
    expect_null(image$filePath)
    expect_true(jamovi_reporting_update(analysis,list(comparisonGraphic=TRUE)))
    expect_true(analysis$.__enclos_env__$private$.comparisonplot(image))
    expect_false(is.null(image$state))
  }
})
