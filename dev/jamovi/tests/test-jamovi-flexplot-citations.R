test_that("native bibliography and output-marker renderers consume the separate response fields", {
  jmvcore:::initProtoBuf()
  records = list()
  for (empty in c(FALSE,TRUE)) {
    options = if(empty)flexplot:::flexplotaOptions$new() else
      flexplot:::flexplotaOptions$new(out="y",preds="x",ghost=FALSE)
    analysis = flexplot:::flexplotaClass$new(options=options,data=if(empty)NULL else small,
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
