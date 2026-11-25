test_that("Mixed review controls compile with unified suppliers and native p eligibility", {
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")[1]
  output = withr::local_tempfile()
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime,"mixedmodel")),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("conditional native p enablement",log,fixed=TRUE)))
  generated = new.env(parent=asNamespace("flexplot"))
  sys.source(paste0(output,".header.R"),generated)
  options = generated$mixedmodelOptions$new()
  expect_identical(options$predictionType,"cas")
  expect_identical(options$comparisonPredictionType,"cas")
  expect_false(options$comparisonPAvailable)
  schema = yaml::read_yaml(file.path(root,"jamovi/mixedmodel.r.yaml"))
  items = setNames(schema$items,vapply(schema$items,`[[`,character(1),"name"))
  expect_identical(items$plot$title,"Model Visualization")
  expect_identical(items$comparisonplot$title,"Model Comparison Visualization")
  for(name in c("random","random2")) expect_identical(items[[name]]$columns[[1]]$title,"Cluster variable")
})

test_that("native saved Mixed modes retain explicit choices and historical absent defaults", {
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for(first in c("population","cluster","cas")) for(second in c("population","cluster","cas")) {
    saved = list(out="y",preds=c("x","z"),cluster="id",modelTerms=list("x","z"),comparisonFixed="x",
      comparisonTerms=list("x"),predictionType=first,comparisonPredictionType=second,compare=TRUE,
      outputOptionsVersion=2,graphic=FALSE,comparisonGraphic=FALSE,includeP=TRUE)
    pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
    for(cycle in 1:2) {
      analysis = flexplot:::mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
      expect_true(analysis$run())
      expect_identical(options$predictionType,first)
      expect_identical(options$comparisonPredictionType,second)
      expect_true(options$includeP)
      reopened = flexplot:::mixedmodelOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
      expect_identical(reopened$predictionType,first)
      expect_identical(reopened$comparisonPredictionType,second)
      options = reopened
    }
  }
  for(revision in 0:2) {
    saved = list(out="y",preds="x",cluster="id",graphic=FALSE,comparisonGraphic=FALSE,outputOptionsVersion=revision)
    pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options = flexplot:::mixedmodelOptions$new();options$fromProtoBuf(pb)
    analysis = flexplot:::mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
    expect_true(analysis$run())
    expect_identical(options$predictionType,"cluster")
    expect_identical(options$comparisonPredictionType,"cluster")
  }
})

test_that("Mixed header explanations render once in native left-to-right order", {
  records = list()
  for(missing in c(FALSE,TRUE)) for(includeP in c(FALSE,TRUE)) {
    data = jamovi_mixed_test_data()
    if(missing)data$z[1:5] = NA_real_
    analysis = jamovi_mixed_analysis(data=data,modelTerms=list("x"),comparisonFixed=c("x","z"),
      comparisonTerms=list("x","z"),compare=TRUE,includeP=includeP)
    expect_true(analysis$run())
    table = analysis$results$comparison
    pb = table$asProtoBuf()
    records[[length(records)+1L]] = list(name="Mixed comparison",visible=TRUE,columns=length(pb$table$columns),
      headings=unname(lapply(Filter(function(column)column$visible,table$columns),function(column)
        list(name=column$name,value=column$title,classes="",sortable=FALSE))),
      notes=unname(lapply(table$notes,function(note)list(note=note$note))),cells=list(),
      expected=list("Approximate BIC Bayes factor"),forbidden=list())
  }
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(records,file,auto_unbox=TRUE,null="null")
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-note-renderer.cjs"),
    "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets",file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("native table-footer code rendered",log,fixed=TRUE)))
})
