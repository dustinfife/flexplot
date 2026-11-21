test_that("GLM diagnostics distinguish waiting and fitting failures and recover without stale results", {
  skip_if_not_installed("jmvcore")
  data = jamovi_diagnostic_data()
  baseline = list(out="y_bin",preds="x",family="binomial",modelTerms=NULL,quadratic=NULL,compare=FALSE)
  independent = glm(y_bin~x,data=data,family=binomial())
  for (case in jamovi_diagnostic_cases()) {
    analysis = jamovi_reporting_host(do.call(flexplot:::glmgeneralizedOptions$new,c(baseline,list(graphic=FALSE))),data)
    expect_true(analysis$run())
    expected = analysis$results$coefficients$asDF
    expect_true(jamovi_reporting_update(analysis,case$values))
    result = analysis$results[[if(case$waiting) "waiting" else "explanation"]]
    expect_true(result$visible)
    expect_match(result$content,case$text,fixed=TRUE)
    expect_identical(result$status,if(case$waiting) "complete" else "error")
    expect_false(any(vapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$visible,logical(1))))
    expect_equal(analysis$results$coefficients$rowCount,0L)
    expect_null(analysis$results$plot$state)
    expect_null(analysis$results$comparisonplot$state)
    expect_null(analysis$results$plot$filePath)
    expect_false(analysis$results$plot$visible)
    for (name in names(case$values)) expect_equal(analysis$options[[name]],case$values[[name]])
    expect_true(jamovi_reporting_update(analysis,baseline))
    expect_false(analysis$results$waiting$visible)
    expect_false(analysis$results$explanation$visible)
    expect_identical(analysis$results$explanation$content,"")
    expect_equal(analysis$results$coefficients$asDF,expected)
    model = analysis$results$plot$state$model
    expect_equal(coef(model),coef(independent),tolerance=1e-12)
    expect_equal(vcov(model),vcov(independent),tolerance=1e-12)
    expect_equal(predict(model,type="response"),predict(independent,type="response"),tolerance=1e-12)
  }
})

test_that("untrusted variable names in other fitting failures are displayed as escaped text", {
  skip_if_not_installed("jmvcore")
  data = jamovi_diagnostic_data()
  name = "<img src=x onerror=stop('must not run')> & <script>probe()</script>"
  data[[name]] = data$x
  options = flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=name,family="binomial",graphic=FALSE)
  analysis = jamovi_reporting_host(options,data)
  expect_true(analysis$run())
  expect_equal(unname(coef(analysis$results$plot$state$model)),unname(coef(glm(y_bin~x,data=data,family=binomial()))),tolerance=1e-12)
  original = flexplot:::jamovi_glm_generalized_fit
  fail = TRUE
  local_mocked_bindings(jamovi_glm_generalized_fit=function(...) {
    if (fail) stop(paste("Cannot fit predictor",name),call.=FALSE)
    original(...)
  },.package="flexplot")
  expect_true(analysis$run())
  result = analysis$results$explanation
  expect_true(result$visible)
  expect_match(result$content,"&lt;img",fixed=TRUE)
  expect_match(result$content,"&amp;",fixed=TRUE)
  expect_false(grepl("<script>|<img",result$content))
  expect_match(result$.__enclos_env__$private$.error,name,fixed=TRUE)
  expect_false(grepl("Backtrace|Calls:",result$content))
  fail = FALSE
  expect_true(analysis$run())
  expect_false(result$visible)
  expect_identical(result$content,"")
  expect_true(analysis$results$coefficients$visible)
})

test_that("Mixed Model fitting errors use visible escaped content and recover", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(925)
  data = jamovi_mixed_test_data()
  analysis = flexplot:::mixedmodelClass$new(options=flexplot:::mixedmodelOptions$new(out="y",preds="x",cluster="id",graphic=FALSE),data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  original = analysis$results$fixed$asDF
  expect_true(jamovi_reporting_update(analysis,list(randomIntercept=FALSE)))
  expect_true(analysis$results$explanation$visible)
  expect_match(analysis$results$explanation$content,"random intercept or at least one random slope",fixed=TRUE)
  expect_identical(analysis$results$explanation$status,"error")
  expect_false(analysis$results$fixed$visible)
  expect_null(analysis$results$plot$state)
  expect_true(jamovi_reporting_update(analysis,list(randomIntercept=TRUE)))
  expect_false(analysis$results$explanation$visible)
  expect_identical(analysis$results$explanation$content,"")
  expect_equal(analysis$results$fixed$asDF,original)
  model = analysis$results$plot$state$model
  independent = lme4::lmer(y~x+(1|id),data=data)
  expect_equal(lme4::fixef(model),lme4::fixef(independent),tolerance=1e-12)
  expect_equal(as.data.frame(lme4::VarCorr(model)),as.data.frame(lme4::VarCorr(independent)),tolerance=1e-12)
})


test_that("family and outcome changes update diagnostics without changing user selections", {
  skip_if_not_installed("jmvcore")
  data = jamovi_diagnostic_data()
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="factor_y",preds="x",family="gaussian",graphic=FALSE),data)
  expect_true(analysis$run())
  expect_match(analysis$results$explanation$content,"finite numeric outcome")
  expect_identical(analysis$options$family,"gaussian")
  expect_true(jamovi_reporting_update(analysis,list(family="binomial")))
  expect_true(analysis$results$coefficients$visible)
  expect_equal(coef(analysis$results$plot$state$model),coef(glm(factor_y~x,data=data,family=binomial())),tolerance=1e-12)
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_identical(analysis$options$family,"binomial")
  expect_match(analysis$results$explanation$content,"numeric 0/1")
  expect_true(jamovi_reporting_update(analysis,list(family="gaussian")))
  expect_false(analysis$results$explanation$visible)
  expect_equal(coef(analysis$results$plot$state$model),coef(glm(y~x,data=data)),tolerance=1e-12)
})
