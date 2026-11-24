test_that("model ghost options restore real protobuf preferences without altering estimates or comparisons", {
  jmvcore:::initProtoBuf()
  pb=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) for (choice in list(NULL,FALSE,TRUE)) {
    data=jamovi_mixed_test_data()
    saved=list(out="y",preds=c("x","z"),modelTerms=list("x","z"),comparisonTerms=list("x"),
      plotPanels="z",graphic=TRUE,comparisonGraphic=TRUE,compare=FALSE,plotLabelEditorVersion=1L,
      outputOptionsVersion=if(module=="glmgeneralized")2L else 1L)
    if(module=="mixedmodel")saved=c(saved,list(cluster="id",predictionType="population",comparisonFixed="x"))
    if(!is.null(choice))saved$ghostLines=choice
    pb$names=names(saved);pb$options=lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options=get(paste0(module,"Options"),asNamespace("flexplot"))$new();options$fromProtoBuf(pb)
    expect_identical(options$ghostLines,isTRUE(choice))
    restored=get(paste0(module,"Options"),asNamespace("flexplot"))$new();restored$fromProtoBuf(options$asProtoBuf())
    expect_identical(restored$ghostLines,isTRUE(choice))
    analysis=get(paste0(module,"Class"),asNamespace("flexplot"))$new(options=restored,data=NULL)
    reader=jamovi_reporting_read;environment(reader)=list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    tables=function()lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$asDF)
    values=tables()
    expect_true(jamovi_reporting_update(analysis,list(ghostLines=!isTRUE(choice))))
    expect_equal(tables(),values,tolerance=0)
    expect_true(jamovi_reporting_update(analysis,list(plotGroup="z",plotPanels=NULL)))
    expect_identical(restored$ghostLines,!isTRUE(choice))
    expect_true(jamovi_reporting_update(analysis,list(plotGroup=NULL,plotPanels="z",compare=TRUE)))
    if(module=="mixedmodel") {
      expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
      rows=restored$plotLabels
      expect_true(all(c("Legend entry: Model 1","Legend entry: Model 2") %in% vapply(rows,`[[`,character(1),"display")))
    }
    comparison=tables()
    expect_true(jamovi_reporting_update(analysis,list(ghostLines=isTRUE(choice))))
    expect_equal(tables(),comparison,tolerance=0)
  }
})
