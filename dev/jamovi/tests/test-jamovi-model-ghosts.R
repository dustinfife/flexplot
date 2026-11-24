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

test_that("installed ghost notes survive cached-result restoration, label edits and layout transitions", {
  data = jamovi_mixed_test_data()
  records = list()
  for (module in c("glmbasic","glmgeneralized","mixedmodel")) {
    values = list(out="y",preds=c("x","a","z"),modelTerms=list("x","a","z"),
      plotX="x",plotPanels="a",graphic=TRUE,ghostLines=TRUE)
    if(module=="mixedmodel") values = c(values,list(cluster="id",predictionType="population"))
    if(module=="glmgeneralized") values$family = "gaussian"
    options = do.call(get(paste0(module,"Options"),asNamespace("flexplot"))$new,values)
    analysis = get(paste0(module,"Class"),asNamespace("flexplot"))$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    render = function() analysis$.__enclos_env__$private$.plot(analysis$results$plot)
    expect_true(render())
    note = analysis$results$plotExplanation
    expect_match(note$content,"Dashed gray lines",fixed=TRUE)
    expect_true(note$visible)
    order = analysis$results$itemNames
    expect_identical(order[match("plot",order)+1L],"plotExplanation")
    tables = function()lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$asDF)
    numbers = tables()
    labels = options$plotLabels
    for(i in seq_along(labels)) if(labels[[i]]$role=="facetLevel" && labels[[i]]$variable=="a")
      labels[[i]]$label = paste0("Edited ",labels[[i]]$value)
    expect_true(jamovi_reporting_update(analysis,list(plotLabels=labels)))
    expect_true(render())
    expect_match(note$content,"Edited low",fixed=TRUE)
    records[[length(records)+1L]] = list(name=module,content=note$content,expected="Edited low",compact=TRUE)
    message = analysis$results$asProtoBuf(status=4L,includeState=FALSE)
    expect_true(jamovi_reporting_update(analysis,list(plotGroup="a",plotPanels=NULL)))
    expect_true(render())
    expect_match(note$content,"Ghost lines unavailable",fixed=TRUE)
    expect_false(grepl("Dashed gray lines",note$content,fixed=TRUE))
    analysis$results$fromProtoBuf(message,character(),character())
    expect_identical(analysis$results$plotExplanation,note)
    expect_true(jamovi_reporting_update(analysis,list(plotGroup=NULL,plotPanels="a")))
    expect_true(render())
    expect_match(note$content,"Edited low",fixed=TRUE)
    expect_identical(analysis$results$itemNames,order)
    expect_equal(tables(),numbers,tolerance=0)
    expect_true(jamovi_reporting_update(analysis,list(graphic=FALSE)))
    expect_false(note$visible)
    expect_true(jamovi_reporting_update(analysis,list(graphic=TRUE,ghostLines=FALSE)))
    expect_true(render())
    expect_false(grepl("Dashed gray lines",note$content,fixed=TRUE))
  }
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(records,file,auto_unbox=TRUE)
  assets = "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets"
  runtime = list.files(assets,pattern="^resultsview-.*\\.js$",full.names=TRUE)[1]
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-diagnostic-renderer.cjs"),runtime,file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
})
