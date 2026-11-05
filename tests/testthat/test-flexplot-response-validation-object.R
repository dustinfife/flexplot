response_validation_unexpected = function(...) stop("Unexpected prediction generation or plotting")

test_that("ordinary responses and transformed predictors remain supported", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::lm(y ~ a + z, data=small)
  m2 = stats::lm(y ~ a + I(x^2), data=small)
  expect_identical(compare_fits, compare.fits)
  for (method in c("observed", "reference")) {
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      actual = compare_fits(y ~ a, small, m1, m2, prediction_method=method, return.preds=TRUE)
      expect_identical(actual, compare.fits(y ~ a, small, m1, m2,
                                            prediction_method=method, return.preds=TRUE))
      expect_equal(nrow(actual), 2 * nlevels(small$a))
      for (name in unique(actual$model)) {
        model = if (name == "m1 (lm)") m1 else m2
        block = actual[actual$model == name, ]
        if (method == "observed") {
          expected = tapply(stats::predict(model, newdata=small), small$a, mean)
          expect_equal(block$prediction, as.numeric(expected[as.character(block$a)]))
        } else {
          expect_equal(block$prediction, unname(stats::predict(model, newdata=block)))
        }
      }
      expect_no_warning(ggplot2::ggplot_build(compare_fits(y ~ a, small, m1, m2,
                                                           prediction_method=method, jitter=FALSE)))
    }
  }
})

test_that("transformed responses are rejected for either model before prediction or plotting", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$positive = exp(d$y + 5)
  raw = stats::lm(positive ~ x + z, data=d)
  logged = stats::lm(log(positive) ~ x + z, data=d)
  rooted = stats::lm(sqrt(positive) ~ x + z, data=d)
  squared = stats::lm(I(positive^2) ~ x + z, data=d)
  divided = stats::lm(positive / 2 ~ x + z, data=d)
  cases = list(
    list(first=logged, second=NULL, expression="log(positive)", offender="model1"),
    list(first=logged, second=raw, expression="log(positive)", offender="model1"),
    list(first=raw, second=logged, expression="log(positive)", offender="model2"),
    list(first=logged, second=logged, expression="log(positive)", offender="model1"),
    list(first=logged, second=rooted, expression="log(positive)", offender="model1"),
    list(first=rooted, second=raw, expression="sqrt(positive)", offender="model1"),
    list(first=raw, second=squared, expression="I(positive^2)", offender="model2"),
    list(first=divided, second=raw, expression="positive/2", offender="model1")
  )
  testthat::local_mocked_bindings(
    compare_fits_observed=response_validation_unexpected,
    compare_fits_reference=response_validation_unexpected,
    flexplot=response_validation_unexpected,
    .package="flexplot"
  )
  for (method in c("observed", "reference")) {
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      for (return_predictions in c(TRUE, FALSE)) {
        for (case in cases) {
          error = tryCatch(compare_fits(positive ~ x, d, case$first, case$second,
                                         prediction_method=method, return.preds=return_predictions),
                             error=identity)
          expect_s3_class(error, "error")
          message = conditionMessage(error)
          expect_match(message, "Transformed model responses are unsupported", fixed=TRUE)
          expect_match(message, case$offender, fixed=TRUE)
          expect_match(gsub(" ", "", message), case$expression, fixed=TRUE)
          expect_match(message, "different scale from the plotted outcome", fixed=TRUE)
          expect_error(compare.fits(positive ~ x, d, case$first, case$second,
                                      prediction_method=method, return.preds=return_predictions),
                         "Transformed model responses are unsupported", fixed=TRUE)
        }
      }
    }
  }
})

test_that("response validation retains plotting-formula and ordinary outcome checks", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ I(x^2) + z, data=small)
  other = stats::lm(z ~ x, data=small)
  for (method in c("observed", "reference")) {
    expect_error(compare_fits(y ~ I(x^2), small, m, prediction_method=method,
                               return.preds=TRUE), "original variable names")
    expect_error(compare_fits(log(y) ~ x, small, m, prediction_method=method,
                               return.preds=TRUE), "original variable names")
    expect_error(compare_fits(z ~ x, small, m, prediction_method=method,
                               return.preds=TRUE), "same outcome")
    expect_error(compare_fits(y ~ x, small, m, other, prediction_method=method,
                               return.preds=TRUE), "same outcome")
  }
})

test_that("response inspection handles supported model formula representations", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(12)
  d = small
  d$positive = exp(d$y + 5)
  ordinary = list(
    glm=stats::glm(y_bin ~ x + z, data=d, family=stats::binomial()),
    mixed=lme4::lmer(Reaction ~ Days + (1 | Subject), data=lme4::sleepstudy),
    ordinal=MASS::polr(Sat ~ Infl + Type + Cont, weights=Freq, data=MASS::housing),
    tree=rpart::rpart(positive ~ x + z, data=d),
    forest=party::cforest(positive ~ x + z, data=d,
                           controls=party::cforest_unbiased(ntree=5, mtry=1))
  )
  expected = c("y_bin", "Reaction", "Sat", "positive", "positive")
  for (i in seq_along(ordinary)) {
    expect_identical(compare_fits_model_response(ordinary[[i]], "model1"), expected[i])
  }
  transformed = list(
    stats::glm(log(positive) ~ x + z, data=d),
    lme4::lmer(log(Reaction) ~ Days + (1 | Subject), data=lme4::sleepstudy),
    rpart::rpart(log(positive) ~ x + z, data=d),
    party::cforest(log(positive) ~ x + z, data=d,
                    controls=party::cforest_unbiased(ntree=5, mtry=1))
  )
  if (requireNamespace("randomForest", quietly=TRUE)) {
    fit = randomForest::randomForest(positive ~ x + z, data=d, ntree=5)
    expect_identical(compare_fits_model_response(fit, "model1"), "positive")
    transformed = c(transformed, list(randomForest::randomForest(log(positive) ~ x + z,
                                                                 data=d, ntree=5)))
  }
  for (model in transformed) {
    for (method in c("observed", "reference")) {
      response = if (inherits(model, "merMod")) Reaction ~ Days else positive ~ x
      data = if (inherits(model, "merMod")) lme4::sleepstudy else d
      expect_error(compare_fits(response, data, model, return.preds=TRUE,
                                 prediction_method=method), "model1 uses response 'log(", fixed=TRUE)
    }
  }
})
