test_that("GLM waits for a usable specification and clears results on removal", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(), small)
  analysis$init() # Host initialization evaluates schema visibility before presenting results.
  tables = Filter(function(item) inherits(item,"Table"), analysis$results$items)
  expect_true(analysis$results$waiting$visible)
  expect_match(analysis$results$waiting$content,"Select an outcome and predictors")
  expect_false(any(vapply(tables,function(table) table$visible,logical(1))))
  expect_false(analysis$results$plot$visible)
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_true(analysis$results$waiting$visible)
  expect_true(jamovi_reporting_update(analysis,list(preds="x",modelTerms=list("x"))))
  expect_false(analysis$results$waiting$visible)
  expect_true(analysis$results$coefficients$visible)
  expect_equal(analysis$results$coefficients$rowCount,2L)
  expect_equal(coef(analysis$results$plot$state$model),coef(glm(y~x,data=small)),tolerance=1e-12)
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_true(analysis$results$waiting$visible)
  expect_false(any(vapply(tables,function(table) table$visible,logical(1))))
  expect_null(analysis$results$plot$state)
  expect_equal(analysis$results$coefficients$rowCount,0L)
  expect_identical(analysis$results$explanation$content,"")
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_true(analysis$results$coefficients$visible)
  expect_true(jamovi_reporting_update(analysis,list(preds=character(),modelTerms=list())))
  expect_true(analysis$results$waiting$visible)
  expect_true(jamovi_reporting_update(analysis,list(preds="x")))
  expect_true(analysis$results$waiting$visible) # Explicit empty terms are not main effects.
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x"))))
  expect_true(analysis$results$coefficients$visible)
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list())))
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$plot$visible)
  expect_true(jamovi_reporting_update(analysis,list(quadratic="x")))
  expect_true(analysis$results$coefficients$visible)
  expect_true(jamovi_reporting_update(analysis,list(quadratic=NULL,modelTerms=NULL)))
  expect_true(analysis$results$coefficients$visible) # Old saved defaults still mean main effects.
})

test_that("a single family-aware estimates table has optional p-values", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome = switch(family,gaussian="y",binomial="y_bin",Gamma="y_gam",gammaLog="y_gam","y_pois")
    options = flexplot:::glmgeneralizedOptions$new(out=outcome,preds=c("x","a"),family=family)
    analysis = jamovi_reporting_host(options,small)
    expect_false(options$includeP)
    expect_true(analysis$run())
    results = analysis$results
    expect_equal(sum(results$itemNames == "coefficients"),1L)
    expect_false(any(c("fit","likelihood","effects","odds","standardized") %in% results$itemNames))
    expect_identical(results$coefficients$title,"Estimates")
    model = results$plot$state$model
    values = estimates(model)
    p_names = c("p",colnames(summary(model)$coefficients)[4],make.names(colnames(summary(model)$coefficients)[4]))
    expect_false(any(p_names %in% names(results$coefficients$asDF)))
    expected_names = setdiff(names(values),c(p_names,"instantaneous_slope","intercept_threshold"))
    for (name in expected_names) {
      expected = values[[name]]
      if(is.numeric(expected)) expected[!is.finite(expected)] = NA_real_
      expect_equal(results$coefficients$asDF[[name]],expected)
      expect_identical(results$coefficients$getColumn(name)$name,name)
    }
    original = results$coefficients$asDF
    expect_true(jamovi_reporting_update(analysis,list(includeP=TRUE)))
    table = results$coefficients$asDF
    field = intersect(p_names,names(table))
    expect_length(field,1L)
    expected = if(field %in% names(values)) values[[field]] else unname(summary(model)$coefficients[,4])
    expect_equal(table[[field]],expected,tolerance=1e-12)
    expect_equal(table[,names(original)],original)
    expect_true(jamovi_reporting_update(analysis,list(includeP=FALSE)))
    expect_false(any(p_names %in% names(results$coefficients$asDF)))
    expect_equal(results$coefficients$asDF,original)
    # Calculations remain available, although generic fit tables are retired.
    expect_equal(unname(results$plot$state$fitStats[c("AIC","BIC","Log likelihood")]),
      c(AIC(model),BIC(model),as.numeric(logLik(model))),tolerance=1e-12)
  }
})

test_that("undefined cells are blank without hiding literal category names", {
  skip_if_not_installed("jmvcore")
  table = jmvcore::Table$new(columns=list(
    list(name="term",type="text"),list(name="value",type="number"),list(name="difference",type="text")))
  values = data.frame(term=c("NA","NaN","observed"),value=c(NA_real_,NaN,Inf),difference=c(NA_character_,"NaN","3"))
  flexplot:::jamovi_glm_table_rows(table,values)
  expect_identical(table$asDF$term,values$term)
  for(i in 1:3) expect_true(table$getCell(rowNo=i,col="value")$isNotFilled())
  for(i in 1:2) expect_true(table$getCell(rowNo=i,col="difference")$isNotFilled())
  expect_identical(table$asDF$difference[3],"3")
  options=flexplot:::glmgeneralizedOptions$new(out="y_gam",preds=c("x","a"),family="Gamma")
  analysis=jamovi_reporting_host(options,small)
  expect_true(analysis$run())
  expect_true(analysis$results$coefficients$getCell(rowNo=1,col="std.mult.coef")$isNotFilled())
})

test_that("restoring obsolete result records cannot add a second coefficient table", {
  skip_if_not_installed("jmvcore")
  options=flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),family="binomial")
  analysis=jamovi_reporting_host(options,small)
  analysis$init()
  # Protocol-shaped records exercise the actual restoration methods without the GUI serializer.
  record=list(name="coefficients",state=raw(),table=list(columns=list(),rowNames=character(),notes=list()))
  old=list(state=raw(),group=list(elements=list(record,record,
    list(name="effects",state=raw()),list(name="fit",state=raw()),list(name="likelihood",state=raw()))))
  analysis$results$fromProtoBuf(old,character(),character())
  expect_equal(sum(analysis$results$itemNames=="coefficients"),1L)
  expect_false(analysis$results$coefficients$visible)
  expect_true(analysis$run())
  expect_equal(sum(analysis$results$itemNames=="coefficients"),1L)
  expect_equal(analysis$results$coefficients$rowCount,3L)
  expect_true(jamovi_reporting_update(analysis,list(preds=character(),modelTerms=list())))
  analysis$results$fromProtoBuf(old,c("preds","modelTerms"),character())
  expect_true(analysis$run())
  expect_false(analysis$results$coefficients$visible)
  expect_true(analysis$results$waiting$visible)
  expect_equal(analysis$results$coefficients$rowCount,0L)
})

test_that("changing GLM families hides inapplicable statistics without stale columns", {
  skip_if_not_installed("jmvcore")
  analysis=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
    out="y_bin",preds=c("x","a"),family="binomial"),small)
  expect_true(analysis$run())
  expect_true(all(c("OR","standardized.OR") %in% names(analysis$results$coefficients$asDF)))
  expect_true(analysis$results$irt$visible)
  expect_true(analysis$results$probabilities$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="y_pois",family="poisson")))
  expect_false(any(c("OR","standardized.OR") %in% names(analysis$results$coefficients$asDF)))
  expect_true("multiplicative.coef" %in% names(analysis$results$coefficients$asDF))
  expect_false(analysis$results$irt$visible)
  expect_false(analysis$results$probabilities$visible)
  expect_equal(analysis$results$irt$rowCount,0L)
  expect_true(jamovi_reporting_update(analysis,list(out="y",family="gaussian")))
  expect_false("multiplicative.coef" %in% names(analysis$results$coefficients$asDF))
  expect_true("Estimate" %in% names(analysis$results$coefficients$asDF))
  expect_true(jamovi_reporting_update(analysis,list(out="y_bin",family="binomial")))
  expect_true("OR" %in% names(analysis$results$coefficients$asDF))
  expect_false("Estimate" %in% names(analysis$results$coefficients$asDF))
  expect_true(analysis$results$irt$visible)
  expect_true(analysis$results$probabilities$visible)
  expect_equal(analysis$results$coefficients$rowCount,3L)
})
