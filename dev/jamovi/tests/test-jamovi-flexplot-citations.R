test_that("native bibliography and output-marker renderers consume the separate response fields", {
  jmvcore:::initProtoBuf()
  records = list()
  for (module in c("flexplota","glmbasic","glmgeneralized","mixedmodel")) for (empty in c(FALSE,TRUE)) {
    factory = get(paste0(module,"Options"),asNamespace("flexplot"))
    class = get(paste0(module,"Class"),asNamespace("flexplot"))
    options = if(empty)factory$new() else factory$new(out="y",preds="x")
    data = if(module=="mixedmodel")jamovi_mixed_test_data() else small
    if(!empty && module=="mixedmodel") options = factory$new(out="y",preds="x",cluster="id",graphic=FALSE)
    analysis = class$new(options=options,data=if(empty)NULL else data,
      analysisId=1L,datasetId="citation-review")
    if(empty) {
      analysis$.setReadDatasetSource(function(columns)data.frame())
      analysis$.setReadDatasetHeaderSource(function(columns)data.frame())
      analysis$init()
    } else expect_true(analysis$run())
    response = analysis$asProtoBuf(includeState=FALSE)
    capture = new.env(parent=emptyenv());capture$elements = list()
    walk = function(element) {
      capture$elements[[length(capture$elements)+1L]] = as.character(element$refs)
      if(element$has("group"))for(child in element$group$elements)walk(child)
    }
    walk(response$results)
    records[[length(records)+1L]] = list(element_refs=capture$elements,
      references=lapply(response$references,function(ref)list(name=ref$name,title=ref$title)))
  }
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(records,file,auto_unbox=TRUE,null="null")
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-flexplot-bibliography.cjs"),
    "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets",file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("PASS: native References panel",log,fixed=TRUE)))
})
