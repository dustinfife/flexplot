test_that("actual Linear Model analyses report every interaction and factor coefficient", {
  skip_if_not_installed("jmvcore")
  cases = list(list(preds=c("x","z"),terms=list("x","z",c("x","z")),formula=y~x*z),
    list(preds=c("x","a"),terms=list("x","a",c("x","a")),formula=y~x*a),
    list(preds=c("a","b"),terms=list("a","b",c("a","b")),formula=y~a*b),
    list(preds="b",terms=list("b"),formula=y~b))
  for (case in cases) {
    options = flexplot:::glmbasicOptions$new(out="y",preds=case$preds,modelTerms=case$terms,
      graphic=FALSE,includeP=TRUE)
    analysis = flexplot:::glmbasicClass$new(options=options,data=small)
    expect_true(analysis$run())
    jamovi_expect_lm_coefficients(analysis,lm(case$formula,small),includeP=TRUE)
    expect_false(anyDuplicated(analysis$results$glmcat$asDF$var)>0L)
  }
})

test_that("coefficient inference preserves custom contrasts and polynomial bases", {
  skip_if_not_installed("jmvcore")
  data = small
  contrasts(data$b) = contr.sum(3)
  models = list(lm(y~x*b,data),lm(y~x+I(x^2)+b,data),lm(y~poly(x,2)*b,data))
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds=c("x","b"),graphic=FALSE,includeP=TRUE),data=data)
  # The same reporter is used by .run(); polynomial formulas are not exposed by the LM UI.
  for (model in models) {
    flexplot:::jamovi_lm_coefficients(analysis$results$glmcat,model,includeP=TRUE)
    jamovi_expect_lm_coefficients(analysis,model,includeP=TRUE)
  }
})

test_that("aliased coefficients remain explicit rather than being silently omitted", {
  skip_if_not_installed("jmvcore")
  data = small
  data$z = data$x
  for (terms in list(NULL,list("x","z"))) {
    options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","z"),modelTerms=terms,
      graphic=FALSE,includeP=TRUE)
    analysis = flexplot:::glmbasicClass$new(options=options,data=data)
    expect_true(analysis$run())
    model = lm(y~x+z,data)
    table = jamovi_expect_lm_coefficients(analysis,model,includeP=TRUE)
    expect_equal(table$status[is.na(coef(model))],"Aliased (not estimable)")
    expect_null(analysis$results$glmcat$getCell("means",rowNo=3)$value)
    expect_null(analysis$results$glmcat$getCell("se",rowNo=3)$value)
    expect_match(analysis$results$glmcat$notes$aliased$note,"not uniquely estimable")
    expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x"))))
    jamovi_expect_lm_coefficients(analysis,lm(y~x,data),includeP=TRUE)
    expect_false("status" %in% names(analysis$results$glmcat$asDF))
    expect_null(analysis$results$glmcat$notes$aliased)
  }
})

test_that("live term and p-value changes replace complete coefficient sets without stale rows", {
  skip_if_not_installed("jmvcore")
  data = small
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  jamovi_expect_lm_coefficients(analysis,lm(y~x+a,data))
  for (terms in list(list("x","a",c("x","a")),list("x"),list("x","a"))) {
    expect_true(jamovi_reporting_update(analysis,list(modelTerms=terms,includeP=TRUE)))
    formula = flexplot:::jamovi_glm_formula("y",c("x","a"),terms)
    jamovi_expect_lm_coefficients(analysis,lm(formula,data),includeP=TRUE)
  }
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x"),includeP=FALSE)))
  jamovi_expect_lm_coefficients(analysis,lm(y~x,data))
  expect_equal(analysis$results$factorMeans$rowCount,0L)
  expect_equal(analysis$results$diff$rowCount,0L)
})

test_that("actual analyses retain ordered-factor contrasts and incomplete interaction cells", {
  skip_if_not_installed("jmvcore")
  for (ordered in c(FALSE,TRUE)) {
    data = small
    data$b = factor(data$b,levels=c("z","x","y"),ordered=ordered)
    if (!ordered) contrasts(data$b) = contr.helmert(3)
    options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","b"),
      modelTerms=list("x","b",c("x","b")),includeP=TRUE,graphic=FALSE)
    analysis = flexplot:::glmbasicClass$new(options=options,data=data)
    expect_true(analysis$run())
    # Compare with a direct fit to the same data supplied to the actual adapter.
    jamovi_expect_lm_coefficients(analysis,lm(y~x*b,data),includeP=TRUE)
  }
  data = small[!(small$a=="a" & small$b=="x"),]
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("a","b"),
    modelTerms=list("a","b",c("a","b")),includeP=TRUE,graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(analysis$run())
  model = lm(y~a*b,data)
  expect_true(anyNA(coef(model)))
  jamovi_expect_lm_coefficients(analysis,model,includeP=TRUE)
})
