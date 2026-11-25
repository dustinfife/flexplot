test_that("Linear Model supplementary tables follow Model 1 summaries through live changes", {
  skip_if_not_installed("jmvcore")
  empty = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(), data=data.frame())
  empty$init()
  expect_false(empty$results$factorMeans$visible)
  expect_false(empty$results$diff$visible)
  analysis = lm_quadratic_analysis(preds=c("x","a"), modelTerms=list("x"))
  analysis$init()
  expect_false(analysis$results$factorMeans$visible)
  expect_false(analysis$results$diff$visible)
  expect_true(analysis$run())
  for (name in c("factorMeans","diff")) {
    expect_false(analysis$results[[name]]$visible)
    expect_equal(analysis$results[[name]]$rowCount, 0L)
  }
  expect_true(jamovi_reporting_update(analysis, list(modelTerms=list("x","a"))))
  for (name in c("factorMeans","diff")) {
    expect_true(analysis$results[[name]]$visible)
    expect_gt(analysis$results[[name]]$rowCount, 0L)
  }
  jamovi_expect_lm_coefficients(analysis, lm(y~x+a, small))
  factor_values = lapply(c("factorMeans","diff"), function(name) analysis$results[[name]]$asDF)
  expect_true(jamovi_reporting_update(analysis, list(estimates=FALSE)))
  expect_false(analysis$results$factorMeans$visible)
  expect_false(analysis$results$diff$visible)
  expect_true(jamovi_reporting_update(analysis, list(estimates=TRUE)))
  expect_equal(lapply(c("factorMeans","diff"), function(name) analysis$results[[name]]$asDF), factor_values)
  expect_true(jamovi_reporting_update(analysis, list(compare=TRUE, modelTerms=list("x"), comparisonTerms=list("x","a"))))
  for (name in c("factorMeans","diff")) {
    expect_false(analysis$results[[name]]$visible)
    expect_equal(analysis$results[[name]]$rowCount, 0L)
  }
  expect_true(jamovi_reporting_update(analysis, list(compare=FALSE, modelTerms=list("a"))))
  expect_true(analysis$results$factorMeans$visible)
  expect_true(analysis$results$diff$visible)
  expect_true(jamovi_reporting_update(analysis, list(preds="x", modelTerms=list("x"))))
  expect_false(analysis$results$factorMeans$visible)
  expect_false(analysis$results$diff$visible)
  expect_length(analysis$results$diff$notes, 0L)
  expect_true(jamovi_reporting_update(analysis, list(out=NULL)))
  expect_false(analysis$results$factorMeans$visible)
  expect_false(analysis$results$diff$visible)
})

test_that("interaction-only Linear Model factors do not invent supplementary summaries", {
  skip_if_not_installed("jmvcore")
  analysis = lm_quadratic_analysis(preds=c("x","a"), modelTerms=list(c("x","a")))
  expect_true(analysis$run())
  model = lm(y~x:a, small)
  jamovi_expect_lm_coefficients(analysis, model)
  expect_match(analysis$results$glmcat$notes$summaries$note, "Supplementary summaries are unavailable")
  expect_false(analysis$results$factorMeans$visible)
  expect_false(analysis$results$diff$visible)
  # Empty differences are valid even when a separate means summary is available.
  expect_invisible(analysis$.__enclos_env__$private$.diff(data.frame()))
  expect_equal(analysis$results$diff$rowCount, 0L)
})

test_that("restored Linear Model result records cannot keep empty factor tables visible", {
  skip_if_not_installed("jmvcore")
  for (terms in list(list("x"), list("x","a"))) {
    analysis = lm_quadratic_analysis(preds=c("x","a"), modelTerms=terms)
    analysis$init()
    # Restore old table records; the new run must derive visibility from current summaries.
    old = list(state=raw(), group=list(elements=lapply(c("factorMeans","diff"), function(name)
      list(name=name, state=raw(), table=list(columns=list(), rowNames=character(), notes=list())))))
    analysis$results$fromProtoBuf(old, character(), character())
    expect_true(analysis$run())
    for (name in c("factorMeans","diff")) {
      expect_identical(analysis$results[[name]]$visible, length(terms) == 2L)
    }
    jamovi_expect_lm_coefficients(analysis, lm(if (length(terms)==2L) y~x+a else y~x, small))
  }
})

test_that("GLM native header notes identify unchanged family-specific coefficient intervals", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(), small)
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial","gaussian")) {
    outcome = switch(family, gaussian="y", binomial="y_bin", gammaLog="y_gam", Gamma="y_gam", "y_pois")
    expect_true(jamovi_reporting_update(analysis, list(out=outcome, preds=c("x","a"), family=family)))
    results = analysis$results
    model = results$plot$state$model
    fit = summary(model)$coefficients
    is_t = colnames(fit)[3] == "t value"
    critical = if (is_t) qt(.975, df.residual(model)) else qnorm(.975)
    table = results$coefficients$asDF
    expect_equal(jamovi_reported_se(table),unname(if (family=="gaussian") round(fit[,2],3) else fit[,2]),tolerance=1e-12)
    expect_equal(table$lower, unname(fit[,1]-critical*fit[,2]), tolerance=1e-12)
    expect_equal(table$upper, unname(fit[,1]+critical*fit[,2]), tolerance=1e-12)
    expect_identical(results$coefficients$getColumn("lower")$superTitle,
      if (is_t) "95% t CI" else "95% normal Wald CI")
    expect_null(results$coefficients$notes$intervals)
    se_columns = intersect(c("SE","Std..Error","Std. Error"),names(results$coefficients$columns))
    se_columns = Filter(function(name)results$coefficients$getColumn(name)$visible,se_columns)
    expect_length(se_columns,1L)
    for (column in c(se_columns,"lower","upper")) {
      note = results$coefficients$notes$column_uncertainty$note
      expect_match(note,"SE and confidence limits refer to raw coefficients.",fixed=TRUE)
      if (is_t) expect_match(note,paste("t reference with",df.residual(model),"residual df."),fixed=TRUE) else
        expect_false(grepl("t reference",note,fixed=TRUE))
      if (family=="negativebinomial") expect_match(note,"Uncertainty conditions on estimated size (theta).",fixed=TRUE) else
        expect_false(grepl("estimated size",note,fixed=TRUE))
    }
    warnings = results$plot$state$warnings
    expect_identical(results$explanation$visible,length(warnings)>0L)
    expect_identical(results$explanation$content,if (length(warnings))
      paste0("<p>",paste(warnings,collapse="; "),"</p>") else "")
    expect_null(results$coefficients$notes$interpretation)
    expect_length(results$coefficients$getRefs(), 0L)
    expect_length(results$getRefs(),0L)
    expect_setequal(results$getRefs(recurse=TRUE),c("flexplot","graphicalAnalysis"))
    if (family == "binomial") {
      expect_match(results$coefficients$notes$model$note, "Success event")
      expect_match(results$irt$notes$column_reference$note,"reference levels",fixed=TRUE)
      for (column in results$irt$columns) for (row in seq_len(results$irt$rowCount))
        expect_length(column$getCell(row)$footnotes,0L)
      expect_length(results$probabilities$getCell("Probability",rowNo=1)$footnotes,0L)
      expect_length(results$probabilities$notes,0L)
    } else for (name in c("irt","probabilities")) {
      expect_false(results[[name]]$visible)
      expect_equal(results[[name]]$rowCount,0L)
      expect_length(jamovi_test_table_notes(results[[name]]),0L)
    }
    populated = c("coefficients","irt","probabilities")
    populated = populated[vapply(populated,function(name)results[[name]]$rowCount>0L,logical(1))]
    numbers = lapply(populated,function(name)results[[name]]$asDF)
    expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
    for (name in c("coefficients","irt","probabilities")) {
      expect_false(results[[name]]$visible)
      expect_length(results[[name]]$notes,0L)
      for (column in results[[name]]$columns) for (row in seq_len(results[[name]]$rowCount))
        expect_length(column$getCell(row)$footnotes,0L)
    }
    expect_true(jamovi_reporting_update(analysis,list(showEstimates=TRUE,showIRT=TRUE,showProbabilities=TRUE)))
    expect_equal(lapply(populated,function(name)results[[name]]$asDF),numbers,tolerance=0)
  }
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  for (name in c("coefficients","irt","probabilities")) {
    expect_equal(analysis$results[[name]]$rowCount,0L)
    expect_length(jamovi_test_table_notes(analysis$results[[name]]),0L)
  }
})

test_that("empty GLM guides and legacy interpretation records do not reserve result space", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y", preds="x"), small)
  analysis$init()
  expect_false(analysis$results$explanation$visible)
  expect_false(analysis$results$plotExplanation$visible)
  testthat::local_mocked_bindings(jamovi_glm_plot_guide=function(...) "", .package="flexplot")
  old = list(state=raw(), group=list(elements=list(
    list(name="explanation", state=raw(), html=list(content="old interpretation")),
    list(name="plotExplanation", state=raw(), html=list(content="old guide")))))
  analysis$results$fromProtoBuf(old, character(), character())
  expect_true(analysis$run())
  expect_false(analysis$results$explanation$visible)
  expect_false(analysis$results$plotExplanation$visible)
  expect_identical(analysis$results$explanation$content, "")
  expect_identical(analysis$results$plotExplanation$content, "")
  expect_true(analysis$results$plot$visible)
  expect_true(jamovi_reporting_update(analysis, list(graphic=FALSE)))
  expect_false(analysis$results$plot$visible)
  expect_false(analysis$results$plotExplanation$visible)
})
