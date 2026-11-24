test_that("real protobuf restoration discovers legacy edits and retains explicit editor preferences", {
  jmvcore:::initProtoBuf()
  pb=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  label=list(list(key="temporarily absent",role="legendLevel",variable="model",value="model1 (lm)",display="Old display",label="Preferred"))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) for (current in c(FALSE,TRUE)) {
    saved=list(out="y",preds=c("x","z"),modelTerms=list("x","z"),comparisonTerms=list("x"),
      graphic=FALSE,comparisonGraphic=FALSE,compare=FALSE,plotLabelStore=label,
      outputOptionsVersion=if(module=="glmgeneralized") 2L else 1L)
    data=small
    if (module=="mixedmodel") {saved$cluster="id";data=jamovi_mixed_test_data()} else saved$comparisonPlotLabelStore=label
    if (current) saved=c(saved,list(plotLabelEditorVersion=1L,modifyPlotLabels=FALSE,modifyComparisonPlotLabels=FALSE))
    pb$names=names(saved);pb$options=lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options=get(paste0(module,"Options"),asNamespace("flexplot"))$new();options$fromProtoBuf(pb)
    analysis=get(paste0(module,"Class"),asNamespace("flexplot"))$new(options=options,data=NULL)
    reader=jamovi_reporting_read;environment(reader)=list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    expect_identical(options$modifyPlotLabels,!current);expect_identical(options$modifyComparisonPlotLabels,!current)
    expect_equal(options$plotLabelEditorVersion,1L)
    restored=get(paste0(module,"Options"),asNamespace("flexplot"))$new();restored$fromProtoBuf(options$asProtoBuf())
    expect_identical(restored$modifyPlotLabels,!current)
    expect_equal(restored$plotLabelStore,label)
    tables=lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(table)table$asDF)
    expect_true(jamovi_reporting_update(analysis,list(modifyPlotLabels=FALSE,modifyComparisonPlotLabels=FALSE)))
    expect_equal(lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(table)table$asDF),tables)
    expect_equal(options$plotLabelStore,label)
    expect_true(jamovi_reporting_update(analysis,list(modifyPlotLabels=TRUE)))
    expect_true(options$modifyPlotLabels);expect_false(options$modifyComparisonPlotLabels)
  }
})
