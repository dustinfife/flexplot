test_that("GLM output options default on and only control eligible result visibility", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome = switch(family,gaussian="y",binomial="y_bin",Gamma="y_gam",gammaLog="y_gam","y_pois")
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
      out=outcome,preds=c("x","a"),family=family,graphic=FALSE),small)
    expect_true(analysis$run())
    fields = c(coefficients="showEstimates",irt="showIRT",probabilities="showProbabilities")
    baseline = lapply(names(fields),function(name) analysis$results[[name]]$asDF)
    state = analysis$results$plot$state
    for (field in fields) expect_true(analysis$options[[field]])
    for (name in names(fields)) {
      expect_identical(analysis$results[[name]]$visible, name=="coefficients" || family=="binomial")
      expect_true(jamovi_reporting_update(analysis,setNames(list(FALSE),fields[[name]])))
      expect_false(analysis$results[[name]]$visible)
      for (other in setdiff(names(fields),name)) expect_identical(analysis$results[[other]]$visible,
        other=="coefficients" || family=="binomial")
      expect_true(jamovi_reporting_update(analysis,setNames(list(TRUE),fields[[name]])))
      expect_identical(analysis$results[[name]]$visible,name=="coefficients" || family=="binomial")
      for (i in seq_along(fields)) expect_equal(analysis$results[[names(fields)[i]]]$asDF,baseline[[i]])
      fitted = analysis$results$plot$state
      expect_equal(coef(fitted$model),coef(state$model),tolerance=1e-12)
      expect_equal(vcov(fitted$model),vcov(state$model),tolerance=1e-12)
      expect_equal(predict(fitted$model,type="response"),predict(state$model,type="response"),tolerance=1e-12)
      expect_equal(fitted$fitStats,state$fitStats,tolerance=1e-12)
    }
  }
})

test_that("output preferences survive family and model changes without empty or stale tables", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
    out="y_bin",preds=c("x","a"),family="binomial",graphic=FALSE),small)
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(showIRT=FALSE,showProbabilities=FALSE)))
  expect_true(jamovi_reporting_update(analysis,list(out="y_pois",family="poisson")))
  expect_true(jamovi_reporting_update(analysis,list(out="y_bin",family="binomial")))
  expect_false(analysis$options$showIRT)
  expect_false(analysis$options$showProbabilities)
  expect_false(analysis$results$irt$visible)
  expect_false(analysis$results$probabilities$visible)
  expect_true(jamovi_reporting_update(analysis,list(showIRT=TRUE,showProbabilities=TRUE,preds="a",modelTerms=list("a"))))
  expect_false(analysis$results$irt$visible)
  expect_equal(analysis$results$irt$rowCount,0L)
  expect_true(analysis$results$probabilities$visible)
  expect_true(jamovi_reporting_update(analysis,list(preds="x",modelTerms=list("x"))))
  expect_true(analysis$results$irt$visible)
  expect_false(analysis$results$probabilities$visible)
  expect_equal(analysis$results$probabilities$rowCount,0L)
  expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_true(analysis$results$explanation$visible)
  expect_match(analysis$results$explanation$content,"numeric 0/1")
  expect_true(jamovi_reporting_update(analysis,list(out="y_bin")))
  expect_false(analysis$results$explanation$visible)
  for(name in c("coefficients","irt","probabilities")) expect_false(analysis$results[[name]]$visible)
})

test_that("GLM comparison statistics and rendered predictions ignore output preferences", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
    out="y_bin",preds=c("x","a"),family="binomial",modelTerms=list("x","a"),
    comparisonTerms=list("x"),compare=TRUE,comparisonX="x",plotX="x",graphic=FALSE),small)
  expect_true(analysis$run())
  before = analysis$results$comparison$asDF
  state = analysis$results$plot$state
  withr::local_seed(912)
  plot = flexplot:::jamovi_glm_generalized_plot(state,x="x")
  built = ggplot2::ggplot_build(plot)$data
  expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
  expect_equal(analysis$results$comparison$asDF,before)
  set.seed(912)
  expect_equal(ggplot2::ggplot_build(flexplot:::jamovi_glm_generalized_plot(
    analysis$results$plot$state,x="x"))$data,built)
  for (compare in c(FALSE,TRUE,FALSE)) {
    expect_true(jamovi_reporting_update(analysis,list(compare=compare)))
    for(field in c("showEstimates","showIRT","showProbabilities")) expect_false(analysis$options[[field]])
    for(name in c("coefficients","irt","probabilities")) expect_false(analysis$results[[name]]$visible)
  }
})
