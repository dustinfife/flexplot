test_that("actual R6 diagnostics render through the installed jamovi HTML method", {
  data = jamovi_diagnostic_data()
  fixtures = list()
  for (name in names(jamovi_diagnostic_cases())) {
    case = jamovi_diagnostic_cases()[[name]]
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds="x",family="binomial",graphic=FALSE),data)
    expect_true(jamovi_reporting_update(analysis,case$values))
    item = analysis$results[[if(case$waiting) "waiting" else "explanation"]]
    fixtures[[length(fixtures)+1L]] = list(name=name,content=item$content,
      error=item$.__enclos_env__$private$.error,expected=case$text)
  }
  mixed = jamovi_mixed_analysis(randomIntercept=FALSE,randomSlopes=NULL)
  expect_true(mixed$run())
  fixtures[[length(fixtures)+1L]] = list(name="mixed-fit",content=mixed$results$explanation$content,
    error=mixed$results$explanation$.__enclos_env__$private$.error,expected="random intercept")
  main_error = flexplot:::mixedmodelClass$new(options=flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a"),cluster="id",
    predictionType="cluster",plotX="x",plotPanels="a"),data=jamovi_mixed_test_data())
  expect_true(main_error$run())
  fixtures[[length(fixtures)+1L]] = list(name="mixed-main-image-guide",content=main_error$results$plotExplanation$content,
    error=main_error$results$plotExplanation$.__enclos_env__$private$.error,expected="place the grouping variable")
  flexplot:::jamovi_result_clear(mixed$results$explanation)
  expect_identical(mixed$results$explanation$content,"")
  expect_identical(mixed$results$explanation$status,"complete")
  expect_identical(mixed$results$explanation$.__enclos_env__$private$.error,"")
  literal = "<script>stop('untrusted')</script> & <img onerror=probe()>"
  flexplot:::jamovi_diagnostic(mixed$results$explanation,paste("Cannot fit variable",literal),error=TRUE)
  fixtures[[length(fixtures)+1L]] = list(name="escaped-variable",content=mixed$results$explanation$content,
    error=mixed$results$explanation$.__enclos_env__$private$.error,expected="Cannot fit variable",literal=literal)
  flexplot:::jamovi_result_clear(mixed$results$explanation)
  flexplot:::jamovi_diagnostic(mixed$results$explanation,paste("Select variable",literal))
  expect_identical(mixed$results$explanation$status,"complete")
  fixtures[[length(fixtures)+1L]] = list(name="cleared-and-reused",content=mixed$results$explanation$content,
    error=mixed$results$explanation$.__enclos_env__$private$.error,expected="Select variable",literal=literal)
  lm = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(estimates=FALSE),data=NULL)
  fp = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(),data=NULL)
  for (name in c("LM","Flexplot")) {
    analysis = if(name=="LM") lm else fp
    reader = jamovi_reporting_read;environment(reader)=list2env(list(data=small),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    fixtures[[length(fixtures)+1L]] = list(name=paste(name,"waiting"),content=analysis$results$waiting$content,
      error="",expected="Select an outcome")
  }
  local_mocked_bindings(jamovi_lm_formula=function(...)stop(paste("Cannot fit",literal)))
  expect_true(jamovi_reporting_update(lm,list(out="y",preds="x")))
  fixtures[[length(fixtures)+1L]] = list(name="LM fitting error",content=lm$results$explanation$content,
    error=lm$results$explanation$.__enclos_env__$private$.error,expected="Cannot fit",literal=literal)
  data = small;data$y[] = NA_real_
  fp = flexplot:::flexplotaClass$new(options=flexplot:::flexplotaOptions$new(out="y"),data=data)
  expect_true(fp$run())
  fixtures[[length(fixtures)+1L]] = list(name="Flexplot no observations",content=fp$results$explanation$content,
    error=fp$results$explanation$.__enclos_env__$private$.error,expected="No complete observations")
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(fixtures,file,auto_unbox=TRUE)
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/resultsview-*.js")
  expect_length(runtime,1L)
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-diagnostic-renderer.cjs"),runtime,file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_match(paste(log,collapse="\n"),"old error-only HTML fails",fixed=TRUE)
})

test_that("invalid restored GLM settings produce visible diagnostics and recover", {
  jmvcore:::initProtoBuf()
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  option = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (saved in list(list(out="factor_y",preds="x",family="gaussian"),
      list(out="y_bin",preds="x",family="binomial",modelTerms=list(),quadratic=NULL),
      list(out="y",preds="x",family="binomial"))) {
    message = template$clone();message$names=names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=option,group_template=message)
    restored = flexplot:::glmgeneralizedOptions$new();restored$fromProtoBuf(message)
    reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(restored$asProtoBuf())
    analysis = jamovi_reporting_host(reopened,jamovi_diagnostic_data())
    expect_true(analysis$run())
    item = if (length(saved$modelTerms)==0L && !is.null(saved$modelTerms)) analysis$results$waiting else analysis$results$explanation
    expect_true(item$visible)
    expect_true(nzchar(item$content))
    expect_false(analysis$results$coefficients$visible)
    for (name in names(saved)) expect_equal(analysis$options[[name]],saved[[name]])
    expect_true(jamovi_reporting_update(analysis,list(out="y_bin",preds="x",family="binomial",modelTerms=NULL,quadratic=NULL)))
    expect_true(analysis$results$coefficients$visible)
    expect_false(analysis$results$waiting$visible)
    expect_false(analysis$results$explanation$visible)
  }
})
