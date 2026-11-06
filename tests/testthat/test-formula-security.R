formula_security_coordinates = function(layer) {
  layer[intersect(c("x", "y", "xintercept", "yintercept"), names(layer))]
}

formula_security_names = c("x", "has space", "a+b", "tick`name", "back\\slash",
                           "log(y)", "1", "x\\`+I({formula_security_probe();y}) #")

formula_security_probe = function() stop("COLUMN_NAME_EXECUTED")

test_that("column-name builders preserve identifiers without interpreting expressions", {
  for (name in formula_security_names) {
    d = data.frame(y=small$y, check.names=FALSE)
    d[[name]] = small$x
    d$y_bin = small$y_bin
    f = make.formula("y", list(as.name(name)))
    gui = jamovi_formula("y", name)
    expect_identical(all.vars(f), c("y", name))
    expect_identical(all.vars(gui), c("y", name))
    expect_identical(f[[3]], as.name(name))
    expect_identical(gui[[3]], as.name(name))
    quoted = stats::as.formula(paste("y ~", escape_spaces_in_formulas(name)))
    expect_identical(quoted[[3]], as.name(name))
    expect_equal(unname(coef(lm(f, d))), unname(coef(lm(y ~ x, small))))
    expect_equal(unname(coef(glm(f, d, family=gaussian()))),
                 unname(coef(glm(y ~ x, small, family=gaussian()))))
    binomial_formula = make.formula("y_bin", list(as.name(name)))
    expect_equal(unname(coef(glm(binomial_formula, d, family=binomial()))),
                 unname(coef(glm(y_bin ~ x, small, family=binomial()))))
    if (name != "1") expect_identical(make.formula("y", name)[[3]], as.name(name))
  }
  expect_identical(make.formula("y", "1")[[3]], 1)
  expect_identical(make.formula("y", "0")[[3]], 0)
})

test_that("response names and conditioning remain literal in GUI formulas", {
  expect_equal(flexplot_axis_given(NULL), list(given=NA_character_, axis=NA_character_))
  for (name in formula_security_names) {
    f = jamovi_formula(name, "x", c("has space", "a+b"))
    expect_identical(f[[2]], as.name(name))
    expect_identical(all.vars(f), unique(c(name, "x", "has space", "a+b")))
    expect_equal(flexplot_axis_given(f), list(given=c("has space", "a+b"), axis="x"))
    expect_identical(jamovi_formula(name)[[3]], 1)
    d = data.frame(predictor=small$x, check.names=FALSE)
    d[[name]] = small$y
    expect_equal(unname(coef(lm(make.formula(name, "predictor"), d))),
                 unname(coef(lm(y ~ x, small))))
  }
})

test_that("trusted formula expressions and random-effects syntax remain supported", {
  expect_equal(coef(lm(make.formula("y", ~ x + I(z^2)), small)),
               coef(lm(y ~ x + I(z^2), small)))
  d = small
  d$positive = exp(d$y)
  expect_equal(coef(lm(make.formula(~ log(positive), "x"), d)),
               coef(lm(log(positive) ~ x, d)))
  expect_equal(coef(lm(make.formula("y", c("x", "z")), small)),
               coef(lm(y ~ x + z, small)))
  transformed = flexplot(y ~ log(x^2 + 1), small, suppress_smooth=TRUE, jitter=FALSE)
  expect_equal(sort(ggplot2::ggplot_build(transformed)$data[[1]]$x), sort(log(small$x^2 + 1)))
  expect_identical(all.vars(make.formula("y", "x", "(1 | id)")), c("y", "x", "id"))
  expect_error(make.formula("y", "x", "(1 | I({formula_security_probe(); id}))"),
               "Unsupported random-effects string")
  expect_error(make.formula("y", "x", "{formula_security_probe(); (1 | id)}"),
               "Unsupported random-effects string")
  skip_if_not_installed("lme4")
  for (random in c("(1 | id)", "(1 + x | id)", "(1 + x || id)")) {
    expected = lme4::lmer(stats::as.formula(paste("y ~ x +", random)), small_mixed)
    actual = lme4::lmer(make.formula("y", "x", random), small_mixed)
    expect_equal(lme4::fixef(actual), lme4::fixef(expected))
    expect_equal(as.numeric(logLik(actual)), as.numeric(logLik(expected)))
  }
  d = small_mixed
  names(d)[names(d) == "id"] = "group`\\name"
  random = call("(", call("|", call("+", 1, as.name("x")), as.name("group`\\name")))
  expected = lme4::lmer(y ~ x + (1 + x | id), small_mixed)
  actual = lme4::lmer(make.formula("y", "x", random), d)
  expect_equal(lme4::fixef(actual), lme4::fixef(expected))
  expect_equal(as.numeric(logLik(actual)), as.numeric(logLik(expected)))
})

test_that("jamovi plotting analysis and callback handle literal column names", {
  skip_if_not_installed("jmvcore")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add=TRUE)
  for (name in formula_security_names[formula_security_names != "1"]) {
    d = data.frame(y=small$y, check.names=FALSE)
    d[[name]] = small$x
    public = do.call(flexplota, list(data=d, out="y", preds=name, given=NULL, line="Regression", suppr=TRUE))
    expect_identical(all.vars(public$plot$state$formula), c("y", name))
    options = flexplotaOptions$new(out="y", preds=name, line="Regression", suppr=TRUE)
    analysis = flexplotaClass$new(options=options, data=d)
    expect_true(analysis$run())
    image = analysis$results$plot
    expect_identical(all.vars(image$state$formula), c("y", name))
    expect_true(analysis$.__enclos_env__$private$.plot(image))
    p = jamovi_plots(image$state$formula, image$state$data, options)
    expect_equal(sort(ggplot2::ggplot_build(p)$data[[1]]$y), sort(small$y))
  }
})

test_that("jamovi GLM entry point fits literal names before downstream summaries", {
  skip_if_not_installed("jmvcore")
  captured = new.env(parent=emptyenv())
  # The old summary/table code has independent nonstandard-name failures.
  # Stop after capturing the actual lm fit rather than mocking model fitting.
  local_mocked_bindings(estimates.lm=function(object, mc=TRUE) {
    captured$model = object
    stop("FIT_CAPTURED")
  }, .package="flexplot")
  for (name in formula_security_names) {
    d = data.frame(y=small$y, check.names=FALSE)
    d[[name]] = small$x
    expect_error(do.call(glmbasic, list(data=d, out="y", preds=name, graphic=FALSE)), "FIT_CAPTURED")
    expect_identical(all.vars(formula(captured$model)), c("y", name))
    expect_equal(unname(coef(captured$model)), unname(coef(lm(y ~ x, small))))
  }
})

test_that("JASP encoded identifiers retain their existing model meaning", {
  # Exercise identifiers used by the JASP callers without pretending to provide
  # the unavailable jaspBase/jaspResults host runtime.
  d = small
  names(d)[names(d) == "y"] = "JaspColumn_1_Encoded"
  names(d)[names(d) == "x"] = "JaspColumn_2_Encoded"
  f = make.formula("JaspColumn_1_Encoded", "JaspColumn_2_Encoded")
  expect_equal(unname(coef(lm(f, d))), unname(coef(lm(y ~ x, small))))
  expect_identical(all.vars(f), c("JaspColumn_1_Encoded", "JaspColumn_2_Encoded"))
})


test_that("literal categorical names remain safe in internally fitted magnet GLMs", {
  for (name in formula_security_names[!formula_security_names %in% c("x", "1")]) {
    d = data.frame(y_bin=small$y_bin, check.names=FALSE)
    d[[name]] = small$a
    p = flexplot(jamovi_formula("y_bin", name), d, jitter=FALSE)
    reference = flexplot(y_bin ~ a, small, jitter=FALSE)
    actual = ggplot2::ggplot_build(p)$data
    expected = ggplot2::ggplot_build(reference)$data
    expect_equal(lapply(actual, formula_security_coordinates), lapply(expected, formula_security_coordinates))
  }
})

test_that("GUI axes and facets retain unusual names and numerical plot values", {
  d = small
  names(d)[match(c("x", "b", "a"), names(d))] = c("has space", "a+b", "panel`\\name")
  f = jamovi_formula("y", c("has space", "a+b"), "panel`\\name")
  actual = ggplot2::ggplot_build(flexplot(f, d, method="lm", jitter=FALSE))
  expected = ggplot2::ggplot_build(flexplot(y ~ x + b | a, small, method="lm", jitter=FALSE))
  expect_equal(actual$data, expected$data)
  expect_equal(actual$layout$layout$PANEL, expected$layout$layout$PANEL)
  expect_equal(actual$layout$layout[["panel`\\name"]], expected$layout$layout$a)
})
