test_that("neutral fixed-formula builders preserve literal terms, environments and compatibility", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(759)
  data = data.frame(y=rnorm(120),x=rnorm(120),z=rnorm(120))
  probe = FALSE
  for (name in c("x","has space","tick`name","{probe <<- TRUE; x}")) {
    current = data;current[[name]] = current$x
    options = list(out="y",preds=c(name,"z"),modelTerms=list(name,"z",c(name,"z")),quadratic=name)
    formula = flexplot:::jamovi_fixed_formula(options,current)
    legacy = flexplot:::jamovi_glm_generalized_formula(options,current)
    expect_identical(lapply(seq_along(formula),function(i)formula[[i]]),
      lapply(seq_along(legacy),function(i)legacy[[i]]))
    expect_identical(all.vars(formula),c("y",name,"z"))
    expect_identical(parent.env(environment(formula)),asNamespace("flexplot"))
    expect_identical(get("outcome",environment(formula)),"y")
    expect_identical(get("predictors",environment(formula)),options$preds)
    expect_identical(get("terms",environment(formula)),options$modelTerms)
    expected = lm(y~x*z+I(x^2),data)
    fitted = lm(formula,current)
    expect_equal(unname(coef(fitted)),unname(coef(expected)),tolerance=1e-12)
    expect_equal(unname(vcov(fitted)),unname(vcov(expected)),tolerance=1e-12)
    expect_equal(predict(fitted),predict(expected),tolerance=1e-12)
    expect_false(probe)
  }
  options = list(out="y",preds=c("x","z"),modelTerms=NULL,quadratic=NULL)
  cases = list(default=list(terms=NULL,quadratic=NULL,expected=y~x+z),
    empty=list(terms=list(),quadratic=NULL,expected=y~1),
    interaction=list(terms=list(c("x","z")),quadratic=NULL,expected=y~x:z),
    square=list(terms=list(),quadratic="x",expected=y~I(x^2)))
  for (case in cases) {
    formula = flexplot:::jamovi_fixed_formula(options,data,case$terms,case$quadratic)
    expect_equal(coef(lm(formula,data)),coef(lm(case$expected,data)),tolerance=1e-12)
    expect_equal(coef(glm(formula,data=data)),coef(glm(case$expected,data=data)),tolerance=1e-12)
  }
  expect_error(flexplot:::jamovi_fixed_formula(options,data,quadratic="missing"),
    "Quadratic terms require selected numeric predictors",fixed=TRUE)
})
