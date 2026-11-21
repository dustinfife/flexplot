test_that("saved GLM scale options and live callbacks preserve statistical results", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  saved = list(out="y",preds=c("x","z"),family="binomial",modelTerms=list("x","z",c("x","z")),
    quadratic="x",polynomials=TRUE,interactions=TRUE,interactionSpecVersion=2,compare=TRUE,
    comparisonTerms=list("x","z"),comparisonX="x",plotX="x",plotScale="logit",se=TRUE)
  message$names = names(saved)
  message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(message)
  reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
  for (name in names(saved)) expect_equal(reopened[[name]],saved[[name]])
  expect_identical(flexplot:::glmgeneralizedOptions$new()$plotScale,"probability")
  withr::local_seed(743)
  d = data.frame(x=seq(-2,2,length.out=220),z=rnorm(220))
  d$y = rbinom(220,1,plogis(.2+.5*d$x+.6*d$z+.2*d$x*d$z+.1*d$x^2))
  d$z[1:7] = NA_real_
  analysis = jamovi_reporting_host(reopened,d)
  expect_true(analysis$run())
  expect_true(analysis$results$comparisonPlotExplanation$visible)
  expect_match(analysis$results$comparisonPlotExplanation$content,"logits")
  initial = analysis$results$plot$state
  table = analysis$results$comparison$asDF
  capture = new.env(parent=emptyenv())
  original1 = flexplot:::jamovi_glm_generalized_plot
  original2 = flexplot:::jamovi_glm_generalized_comparison_plot
  local_mocked_bindings(jamovi_glm_generalized_plot=function(...) {capture$primary=original1(...);capture$primary},
    jamovi_glm_generalized_comparison_plot=function(...) {capture$comparison=original2(...);capture$comparison})
  withr::local_options(device=function(...)grDevices::pdf(file=NULL))
  for (scale in c("probability","logit","probability","logit")) {
    expect_true(jamovi_reporting_update(analysis,list(plotScale=scale)))
    current = analysis$results$plot$state
    expect_equal(coef(current$model),coef(initial$model),tolerance=1e-12)
    expect_equal(vcov(current$model),vcov(initial$model),tolerance=1e-12)
    expect_equal(current$fitStats,initial$fitStats,tolerance=1e-12)
    expect_equal(analysis$results$comparison$asDF,table,tolerance=1e-12)
    expect_identical(analysis$options$modelTerms,saved$modelTerms)
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
    expect_no_warning(ggplot2::ggplotGrob(capture$primary))
    expect_no_warning(ggplot2::ggplotGrob(capture$comparison))
    if (scale=="logit") expect_equal(capture$primary$scales$get_scales("y")$name,"Logit (log odds)")
  }
  expect_true(jamovi_reporting_update(analysis,list(plotStyle="raw")))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_false(identical(capture$primary$scales$get_scales("y")$name,"Logit (log odds)"))
  expect_match(analysis$results$plotExplanation$content,"Probability scale is used")
  expect_true(jamovi_reporting_update(analysis,list(plotStyle="auto")))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_true(jamovi_reporting_update(analysis,list(family="gaussian")))
  expect_identical(analysis$options$plotScale,"logit")
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_true(jamovi_reporting_update(analysis,list(family="binomial")))
  expect_identical(analysis$options$plotScale,"logit")
  expect_equal(coef(analysis$results$plot$state$model),coef(initial$model),tolerance=1e-12)
})
