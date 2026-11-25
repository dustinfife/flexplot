test_that("LM column header markers and citation ownership survive native serialization and old caches", {
  jmvcore:::initProtoBuf()
  data = small[complete.cases(small[c("y","x","z")]),]
  records = list()
  for (missing in c(FALSE,TRUE)) for (includeP in c(FALSE,TRUE)) {
    d = data;if(missing)d$z[1] = NA_real_
    analysis = lm_quadratic_analysis(data=d,preds=c("x","z"),modelTerms=list("x"),
      comparisonTerms=list("x","z"),compare=TRUE,includeP=includeP,graphic=FALSE,comparisonGraphic=FALSE)
    expect_true(analysis$run())
    table = analysis$results$comparison
    pb = table$asProtoBuf(status=4L,includeState=FALSE)
    columns = Filter(function(column)isTRUE(column$visible),table$columns)
    records[[length(records)+1L]] = list(includeP=includeP,
      columns=unname(lapply(columns,function(column)list(name=column$name,title=column$title))),
      notes=unname(lapply(table$notes,function(note)list(note=note$note,init=note$init))),
      cells=unname(unlist(lapply(table$columns,function(column)lapply(seq_len(table$rowCount),
        function(i)list(footnotes=column$getCell(i)$footnotes))),recursive=FALSE)))
    for(column in columns) {
      saved = Filter(function(item)item$name==column$name,pb$table$columns)[[1]]
      expect_identical(saved$title,column$title)
    }
    # Real historical cache: root citations, no new labeled reference result.
    cached = analysis$results$asProtoBuf(status=4L,includeState=FALSE)
    cached$refs = c("flexplot","graphicalAnalysis")
    cached$group$elements = Filter(function(item)item$name!="methodReferences",cached$group$elements)
    options = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    cell = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    values = setNames(lapply(analysis$options$names,analysis$options$get),analysis$options$names)
    options$names = names(values)
    options$options = lapply(values,jamovi_mixed_saved_option,option_template=cell,group_template=options)
    restoredOptions = flexplot:::glmbasicOptions$new();restoredOptions$fromProtoBuf(options)
    reopened = flexplot:::glmbasicClass$new(options=restoredOptions,data=NULL)
    reader = jamovi_reporting_read;environment(reader) = list2env(list(data=d),parent=environment(reader))
    reopened$.setReadDatasetSource(reader);reopened$.setReadDatasetHeaderSource(reader)
    objects = reopened$results$items
    reopened$results$fromProtoBuf(cached,character(),character())
    expect_identical(reopened$results$items,objects)
    expect_true(reopened$run())
    expect_equal(reopened$results$comparison$asDF,table$asDF,tolerance=0)
    expect_identical(reopened$options$includeP,includeP)
    expect_length(reopened$results$getRefs(),0L)
    expect_identical(reopened$results$methodReferences$getRefs(),c("flexplot","graphicalAnalysis"))
    expect_match(reopened$results$methodReferences$content,"Method References",fixed=TRUE)
    expect_true(reopened$results$methodReferences$visible)
    expect_identical(tail(reopened$results$itemNames,1),"methodReferences")
    expect_match(table$asString(),if(missing)"N¹" else "BIC Bayes factor¹",fixed=TRUE)
  }
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(records,file,auto_unbox=TRUE,null="null")
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-lm-header-notes.cjs"),
    "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets",file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("PASS: installed native Unicode",log,fixed=TRUE)))
})

test_that("compiled LM definitions preserve supported result citation ownership and concise names", {
  schema = yaml::read_yaml(file.path(root,"jamovi/glmbasic.r.yaml"))
  expect_null(schema$refs)
  refs = Filter(function(item)length(item$refs)>0L,schema$items)
  expect_length(refs,1L)
  expect_identical(refs[[1]]$name,"methodReferences")
  expect_identical(refs[[1]]$refs,c("flexplot","graphicalAnalysis"))
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")[1]
  output = withr::local_tempfile()
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime,"glmbasic")),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("PASS: native comparison supplier",log,fixed=TRUE)))
  generated = new.env(parent=asNamespace("flexplot"));sys.source(paste0(output,".header.R"),generated)
  results = generated$glmbasicResults$new(generated$glmbasicOptions$new())
  expect_identical(results$plot$title,"Model Visualization")
  expect_identical(results$comparisonplot$title,"Model Comparison Visualization")
  expect_length(results$getRefs(),0L)
  expect_identical(results$methodReferences$getRefs(),c("flexplot","graphicalAnalysis"))
  options = yaml::read_yaml(file.path(root,"jamovi/glmbasic.a.yaml"))$options
  title = function(name)Filter(function(item)item$name==name,options)[[1]]$title
  expect_identical(title("se"),"95% confidence intervals")
  expect_identical(title("graphic"),"Model Visualization")
  expect_identical(title("comparisonGraphic"),"Model Comparison Visualization")
})
