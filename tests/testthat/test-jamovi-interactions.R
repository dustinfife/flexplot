test_that("new interaction options preserve fitted LM and GLM design matrices", {
  skip_if_not_installed("jmvcore")
  withr::local_options(contrasts=c("contr.sum","contr.poly"))
  specifications = list(
    list(formula=y~x*z, predictors=c("x","z"), terms=list("x","z",c("x","z"))),
    list(formula=y~x*a, predictors=c("x","a"), terms=list("x","a",c("x","a"))),
    list(formula=y~a*b, predictors=c("a","b"), terms=list("a","b",c("a","b"))),
    list(formula=y~x:z, predictors=c("x","z"), terms=list(c("x","z"))))
  for (name in c("glmbasic","glmgeneralized")) {
    factory = get(paste0(name,"Options"),asNamespace("flexplot"))
    defaults = factory$new()
    expect_false(defaults$interactions)
    expect_false(defaults$comparisonInteractions)
    expect_equal(defaults$interactionSpecVersion,0L)
    data = small
    data$z[1:8] = NA_real_
    data$a[12:17] = NA
    if (name == "glmgeneralized") data$y = data$y_bin
    for (specification in specifications) {
      values = list(out="y",preds=specification$predictors,modelTerms=specification$terms,
        interactions=TRUE,interactionSpecVersion=1L,graphic=FALSE)
      if (name == "glmgeneralized") values$family = "binomial"
      options = do.call(factory$new,values)
      analysis = get(paste0(name,"Class"),asNamespace("flexplot"))$new(options=options,data=data)
      expect_true(analysis$run())
      expected = if (name == "glmbasic") lm(specification$formula,data) else
        glm(specification$formula,data,family=binomial())
      if (name == "glmbasic") {
        jamovi_expect_lm_coefficients(analysis,expected)
        state = analysis$results$plot$state
        actual = lm(state$formula,state$data)
      } else actual = analysis$results$plot$state$model
      expect_equal(model.matrix(actual),model.matrix(expected),tolerance=1e-12)
      expect_equal(coef(actual),coef(expected),tolerance=1e-12)
      expect_equal(predict(actual,newdata=data,type="response"),predict(expected,newdata=data,type="response"),tolerance=1e-12)
    }
  }
})
