test_that("shared quadratic synchronization has a module-neutral contract", {
  output = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-quadratic-sync.cjs"),root)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(output,"status"),info=paste(output,collapse="\n"))
  expect_match(paste(output,collapse="\n"),"PASS: shared quadratic filtering",fixed=TRUE)
})

test_that("compiled LM polynomial handlers drive the actual fitted formulas", {
  node = Sys.which("node")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  root = normalizePath(test_path("..","..",".."))
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(c(output,paste0(output,".header.R"),paste0(output,".quadratic.json"))))
  log = system2(node,shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime[1])),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  records = jsonlite::fromJSON(paste0(output,".quadratic.json"),simplifyVector=FALSE)
  primary = list(default=y~z,`enabled-empty`=y~z,squared=y~z+x+I(x^2),`square-only-x`=y~z+I(x^2),
    `two-squares`=y~z+x+I(x^2)+I(z^2),comparison=y~z+x+I(x^2)+I(z^2),
    `disabled-primary`=y~z+x,`disabled-second`=y~z+x,restored=y~z+I(x^2),`removed-predictor`=y~z)
  secondary = list(comparison=y~x+z+I(z^2),`disabled-primary`=y~x+z+I(z^2),
    `disabled-second`=y~x+z,restored=y~x+I(z^2),`removed-predictor`=y~I(z^2))
  for(record in records) {
    values = record$options
    values$preds = unlist(values$preds)
    values$quadratic = unlist(values$quadratic)
    values$comparisonQuadratic = unlist(values$comparisonQuadratic)
    values$modelTerms = lapply(values$modelTerms,unlist)
    values$comparisonTerms = lapply(values$comparisonTerms,unlist)
    analysis = do.call(lm_quadratic_analysis,values)
    expect_true(analysis$run(),info=record$stage)
    expected = lm(primary[[record$stage]],small)
    jamovi_expect_lm_coefficients(analysis,expected)
    if(isTRUE(values$compare)) {
      expected2 = lm(secondary[[record$stage]],small)
      expect_equal(coef(analysis$results$comparisonplot$state$model2),coef(expected2),tolerance=1e-12)
    }
  }
})

test_that("saved polynomial options and public LM calls retain independent specifications", {
  jmvcore:::initProtoBuf()
  for(polynomial in c(FALSE,TRUE)) {
    saved = list(out="y",preds=c("x","z"),graphic=FALSE,modelTerms=list("z"),compare=TRUE,
      comparisonTerms=list("x"),comparisonX="x")
    if(polynomial) saved = c(saved,list(polynomials=TRUE,quadratic="x",
      comparisonPolynomials=TRUE,comparisonQuadratic="z"))
    message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmbasicOptions$new();options$fromProtoBuf(message)
    analysis = flexplot:::glmbasicClass$new(options=options,data=small)
    expect_true(analysis$run())
    jamovi_expect_lm_coefficients(analysis,lm(if(polynomial)y~z+I(x^2) else y~z,small))
    expect_equal(coef(analysis$results$comparisonplot$state$model2),
      coef(lm(if(polynomial)y~x+I(z^2) else y~x,small)),tolerance=1e-12)
    expect_identical(options$polynomials,polynomial)
    expect_identical(options$comparisonPolynomials,polynomial)
  }
  results = glmbasic(small,out="y",preds=c("x","z"),modelTerms=~x+z,
    polynomials=TRUE,quadratic="x",graphic=FALSE)
  expect_equal(results$glmcat$asDF$means,unname(coef(lm(y~x+z+I(x^2),small))),tolerance=1e-12)
})

test_that("simplified LM native controls and protobuf restoration preserve both fitted formulas", {
  jmvcore:::initProtoBuf()
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")[1]
  output = withr::local_tempfile()
  withr::defer(unlink(paste0(output,c(".header.R",".quadratic.json",".restored.json",".interactions.json",".main-effects.json"))))
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  records = jsonlite::read_json(paste0(output,".main-effects.json"),simplifyVector=FALSE)
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (record in records) {
    values = record$options
    for (name in c("preds","quadratic","comparisonQuadratic","mainEffects","comparisonMainEffects","plotPanels","comparisonPanels"))
      values[name] = list(unlist(values[[name]],use.names=FALSE))
    for (name in c("modelTerms","comparisonTerms")) values[name] = list(lapply(values[[name]],unlist,use.names=FALSE))
    values$graphic = FALSE
    message$names = names(values)
    message$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=message)
    saved = flexplot:::glmbasicOptions$new();saved$fromProtoBuf(message)
    restored = flexplot:::glmbasicOptions$new();restored$fromProtoBuf(saved$asProtoBuf())
    expect_equal(restored$modelTerms,values$modelTerms,info=record$stage)
    expect_equal(restored$comparisonTerms,values$comparisonTerms,info=record$stage)
    expect_equal(restored$quadratic,values$quadratic,info=record$stage)
    first = if (startsWith(record$stage,"legacy-subset")) y~z else
      if (startsWith(record$stage,"legacy-interaction")) y~x:z else
      if (startsWith(record$stage,"legacy-square")) y~I(x^2) else
      if (startsWith(record$stage,"legacy-empty")) y~1 else
      switch(record$stage,fresh=y~x,add=y~x+z,remove=y~z,y~z+x)
    analysis = flexplot:::glmbasicClass$new(options=restored,data=small)
    expect_true(analysis$run(),info=record$stage)
    jamovi_expect_lm_coefficients(analysis,lm(first,small))
    if (isTRUE(values$compare)) {
      second = if (startsWith(record$stage,"legacy-")) y~z else
        if(record$stage=="second-interaction-only") y~x:z else y~I(x^2)
      model = analysis$results$comparisonplot$state$model2
      expect_identical(attr(terms(model),"term.labels"),attr(terms(second),"term.labels"))
      expect_equal(coef(model),coef(lm(second,small)),tolerance=1e-12)
      expect_equal(predict(model,newdata=small),predict(lm(second,small),newdata=small),tolerance=1e-12)
    }
  }
})
