test_that("GLM notes and display headings are concise, family-specific and preserve all table values", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome = switch(family,gaussian="y",binomial="y_bin",Gamma="y_gam",gammaLog="y_gam","y_pois")
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=outcome,
      preds=c("x","a"),family=family,graphic=FALSE),small)
    expect_true(analysis$run())
    results = analysis$results
    model = results$plot$state$model
    values = estimates(model)
    columns = setdiff(names(values),c("instantaneous_slope","intercept_threshold","Pr...t..","Pr...z.."))
    for (name in columns) {
      expected_value = values[[name]]
      if (is.numeric(expected_value)) expected_value[!is.finite(expected_value)] = NA_real_
      expect_equal(results$coefficients$asDF[[name]],expected_value)
    }
    expected = c(raw.coefficients="Coefficient",Estimate="Coefficient",OR="OR",inverse.OR="1/OR",
      standardized.OR="Std. OR",inverse.standardized.OR="1/Std. OR",multiplicative.coef="Multiplier",
      inverse.coef="Reciprocal",std.mult.coef=if (family=="Gamma") "Std. reciprocal" else "Std. multiplier",
      `Prediction Difference (+/- 1 SD)`="Prediction difference")
    for (name in intersect(names(expected),columns)) {
      expect_identical(gsub("[ᵃᵇᶜᵈᵉᶠᵍʰⁱʲ]","",results$coefficients$getColumn(name)$title),unname(expected[[name]]))
      expect_identical(results$coefficients$getColumn(name)$name,name)
    }
    notes = jamovi_test_table_notes(results$coefficients)
    text = paste(notes,collapse=" ")
    expect_false(anyDuplicated(notes)>0L)
    expect_false(grepl("estimates.glm|95% t confidence intervals|95% normal.Wald confidence intervals",text))
    expect_false(grepl("Interaction effects",text))
    expect_false(grepl("Factor coefficients follow",text,fixed=TRUE))
    if (family=="binomial") {
      expect_match(text,"Success event: 1",fixed=TRUE)
      expect_null(results$coefficients$notes$interpretation)
      expect_match(text,"raw coefficients",fixed=TRUE)
      expect_match(text,"model-matrix column",fixed=TRUE)
      expect_null(results$coefficients$notes$success)
      expect_match(results$irt$notes$column_reference$note,"reference levels",fixed=TRUE)
      expect_length(results$irt$getCell("instantaneous_slope",rowNo=1)$footnotes,0L)
    } else {
      expect_false(grepl("Success event|odds|Std. OR",text))
      expect_length(results$irt$notes,0L)
      expect_length(results$probabilities$notes,0L)
    }
    if (family=="Gamma") expect_match(text,"not response ratios",fixed=TRUE)
    if (family=="negativebinomial") expect_match(text,"estimated size",fixed=TRUE)
  }
})

test_that("empty and hidden GLM tables have no notes, and notes return on live changes", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",
    preds="a",family="binomial",graphic=FALSE),small)
  expect_true(analysis$run())
  expect_length(analysis$results$irt$notes,0L)
  expect_true(jamovi_reporting_update(analysis,list(preds="x")))
  expect_length(analysis$results$probabilities$notes,0L)
  expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
  for (name in c("coefficients","irt","probabilities")) expect_length(analysis$results[[name]]$notes,0L)
  expect_true(jamovi_reporting_update(analysis,list(preds=c("x","a"),showEstimates=TRUE,showIRT=TRUE,showProbabilities=TRUE)))
  for (name in c("coefficients","irt")) expect_gt(length(jamovi_test_table_notes(analysis$results[[name]])),0L)
  expect_length(jamovi_test_table_notes(analysis$results$probabilities),0L)
})

test_that("GLM explanations omit statistics that are entirely unavailable", {
  skip_if_not_installed("jmvcore")
  original = flexplot:::estimates.glm
  local_mocked_bindings(estimates.glm=function(model,...) {
    values = original(model,...)
    values$standardized.OR = values$inverse.standardized.OR = NA_real_
    values[["Prediction Difference (+/- 1 SD)"]] = NA_real_
    values
  },.package="flexplot")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",
    preds=c("x","a"),family="binomial",graphic=FALSE),small)
  expect_true(analysis$run())
  expect_null(analysis$results$coefficients$notes$differences)
  expect_false(grepl("Std. OR",paste(jamovi_test_table_notes(analysis$results$coefficients),collapse=" "),fixed=TRUE))
  expect_match(paste(jamovi_test_table_notes(analysis$results$coefficients),collapse=" "),"raw coefficients",fixed=TRUE)
})

test_that("fitting warnings remain visible independently of output preferences", {
  skip_if_not_installed("jmvcore")
  original = flexplot:::jamovi_glm_generalized_fit
  warn = TRUE
  local_mocked_bindings(jamovi_glm_generalized_fit=function(...) {
    state = original(...)
    if (warn) state$warnings = "Convergence warning <check specification>"
    state
  },.package="flexplot")
  for (compare in c(FALSE,TRUE)) {
    warn = TRUE
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",
      preds=c("x","a"),family="binomial",graphic=FALSE,compare=compare,comparisonX="x",
      showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE),small)
    expect_true(analysis$run())
    expect_true(analysis$results$explanation$visible)
    expect_identical(analysis$results$explanation$status,"complete")
    expect_identical(analysis$results$explanation$content,"<p>Convergence warning &lt;check specification&gt;</p>")
    for (name in c("coefficients","irt","probabilities","comparison")) expect_null(analysis$results[[name]]$notes$warnings)
    warn = FALSE
    expect_true(analysis$run())
    expect_false(analysis$results$explanation$visible)
    expect_identical(analysis$results$explanation$content,"")
  }
})

test_that("binary success coding is identified once on the first visible summary table", {
  skip_if_not_installed("jmvcore")
  for (predictors in list("x","a",c("x","a"))) {
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=predictors,
      family="binomial",showEstimates=FALSE,graphic=FALSE),small)
    expect_true(analysis$run())
    notes = unlist(lapply(c("coefficients","irt","probabilities"),function(name)
      vapply(analysis$results[[name]]$notes,function(note)note$note,character(1))),use.names=FALSE)
    expect_equal(sum(grepl("Success event: 1",notes,fixed=TRUE)),1L)
    expect_length(analysis$results$coefficients$notes,0L)
  }
})
