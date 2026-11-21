test_that("restored GLM native controls preserve both models through live edits", {
  node = Sys.which("node")
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  root = normalizePath(test_path("..", "..", ".."))
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(c(output,paste0(output,c(".header.R",".restored.json")))))
  log = system2(node,shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime[1],"glmgeneralized")),
                stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  if (!file.exists(paste0(output,".restored.json"))) return(invisible(NULL))
  records = jsonlite::read_json(paste0(output,".restored.json"),simplifyVector=FALSE)
  set.seed(713)
  d = data.frame(x=rnorm(180),z=rnorm(180))
  d$y = rbinom(nrow(d),1,plogis(-.4+.5*d$x-.3*d$z+.2*d$x*d$z+.15*d$x^2))
  analysis = NULL
  for (record in records) {
    values = record$options
    for (name in c("preds","quadratic","comparisonQuadratic","plotPanels","comparisonPanels","mainEffects","comparisonMainEffects"))
      values[name] = list(unlist(values[[name]],use.names=FALSE))
    for (name in c("modelTerms","comparisonTerms"))
      values[name] = list(if (is.null(values[[name]])) NULL else lapply(values[[name]],unlist,use.names=FALSE))
    options = do.call(flexplot:::glmgeneralizedOptions$new,values)
    if (is.null(analysis)) analysis = jamovi_reporting_host(options,d) else
      expect_true(jamovi_reporting_update(analysis,values),info=record$stage)
    expect_true(analysis$run(),info=record$stage)
    waiting = record$stage %in% c("empty","missing-outcome","empty-primary","remove-predictors")
    expect_identical(analysis$results$waiting$visible,waiting,info=record$stage)
    if (waiting) {
      expect_null(analysis$results$plot$state,info=record$stage)
      expect_false(analysis$results$coefficients$visible,info=record$stage)
      next
    }
    expected = switch(record$stage,
      fresh=y~x+z, legacy=y~x+z, `empty-secondary`=y~x,
      if (startsWith(record$stage,"remove-interaction")) y~x+z+I(x^2) else y~x*z+I(x^2))
    independent = glm(expected,data=d,family=binomial())
    model = analysis$results$plot$state$model
    expect_identical(attr(terms(model),"term.labels"),attr(terms(independent),"term.labels"),info=record$stage)
    expect_equal(coef(model),coef(independent),tolerance=1e-12,info=record$stage)
    expect_equal(predict(model,type="response"),predict(independent,type="response"),tolerance=1e-12,info=record$stage)
    if (isTRUE(values$compare)) {
      comparison = analysis$results$comparisonplot$state
      secondary = if(record$stage=="empty-secondary") y~1 else if(record$stage=="legacy") y~x+z else
        if (startsWith(record$stage,"remove-second-quadratic")) y~z else y~z+I(z^2)
      independent2 = glm(secondary,data=d,family=binomial())
      expect_equal(coef(comparison$model2),coef(independent2),tolerance=1e-12,info=record$stage)
      expect_equal(predict(comparison$model2,type="response"),predict(independent2,type="response"),tolerance=1e-12,info=record$stage)
    } else expect_false(analysis$results$comparison$visible,info=record$stage)
  }
})

test_that("GLM saved option messages retain independent interactions and quadratics", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  # Use the same protocol encoder as the existing saved-analysis tests.
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  saved = list(out="y",preds=c("x","z"),family="binomial",compare=TRUE,
    modelTerms=list("x","z",c("x","z")),quadratic="x",comparisonTerms=list("z"),comparisonQuadratic="z")
  message$names = names(saved)
  message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options = flexplot:::glmgeneralizedOptions$new()
  options$fromProtoBuf(message)
  reopened = flexplot:::glmgeneralizedOptions$new()
  reopened$fromProtoBuf(options$asProtoBuf())
  for (name in names(saved)) expect_equal(reopened[[name]],saved[[name]])
})
