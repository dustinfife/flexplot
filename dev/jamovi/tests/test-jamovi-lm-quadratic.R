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
