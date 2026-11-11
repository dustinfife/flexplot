context("test helper_compare_fits")

library(testthat)
library(randomForest)

test_that("public GLM comparisons preserve probabilities for numeric and factor outcomes", {
  for (factor_outcome in c(FALSE, TRUE)) {
    d = small
    if (factor_outcome) d$y_bin = factor(d$y_bin, levels=c(0, 1), labels=c("no", "yes"))
    fit = glm(y_bin ~ a, data=d, family=binomial())
    for (method in c("marginal", "observed", "reference")) {
      actual = compare_fits(y_bin ~ a, d, fit, return.preds=TRUE,
                              prediction_method=method)
      expected = unname(predict(fit, newdata=actual, type="response"))
      expect_equal(actual$prediction, expected, tolerance=1e-10)
      expect_true(all(actual$prediction >= 0 & actual$prediction <= 1))
      plot = compare_fits(y_bin ~ a, d, fit, prediction_method=method, jitter=FALSE)
      overlay = which(vapply(plot$layers,
                              function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),
                              logical(1)))
      expect_true(length(overlay) > 0)
      built = ggplot2::ggplot_build(plot)
      for (i in overlay) expect_equal(sort(built$data[[i]]$y), sort(expected), tolerance=1e-10)
    }
  }
})

test_that("public forest comparisons preserve zero and one success probabilities", {
  withr::local_seed(81)
  d = data.frame(a=factor(rep(c("low", "high"), each=24), levels=c("high", "low")),
                 binary=factor(rep(c("0", "1"), each=24), levels=c("0", "1")))
  forest = party::cforest(binary ~ a, data=d,
    controls=party::cforest_unbiased(ntree=10, mtry=1, minsplit=2, minbucket=1))
  probabilities = do.call(rbind, predict(forest, newdata=d, type="prob", OOB=FALSE))
  # Compare with the actual probability column, never recoded class predictions.
  expected = tapply(probabilities[, "binary.1"], d$a, mean)
  expect_setequal(unname(expected), c(0, 1))
  for (method in c("marginal", "observed", "reference")) {
    actual = compare_fits(binary ~ a, d, forest, return.preds=TRUE,
                            prediction_method=method)
    expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]), tolerance=1e-10)
  }
})



test_that("get_re works", {
  expect_true(get_re(lmer(MathAch~SES + (SES | School), data=math))=="School")
  expect_true(get_re(lmer(MathAch~SES + (SES |School), data=math))=="School")
  expect_true(get_re(lmer(MathAch~1 + (1 | School), data=math))=="School")
  expect_null(get_re(lm(MathAch~1 + (1 | School), data=math)))
})  

test_that("get_model_n works", {
  expect_equal(get_model_n(lm(weight.loss~therapy.type, data=exercise_data)), 200)
  expect_equal(get_model_n(rlm(weight.loss~therapy.type, data=exercise_data)), 200)
  suppressWarnings(expect_equal(get_model_n(party::cforest(weight.loss~therapy.type, data=exercise_data)), 200))
  expect_equal(get_model_n(randomForest::randomForest(weight.loss~therapy.type, data=exercise_data)), 200)
  expect_equal(get_model_n(rpart::rpart(weight.loss~motivation, data=exercise_data)), 200)
})

test_that("get_cforest_variables works", {
  expect_equal(get_cforest_variables(small_rf)              , names(small))
  expect_equal(get_cforest_variables(small_rf, "predictors"), names(small)[-1])
  expect_equal(get_cforest_variables(small_rf, "response")  , names(small)[1])
})

test_that("get_terms works", {
  expect_equal(get_terms(lm(y~a+b, data=small))$predictors, c("a", "b"))
  expect_equal(get_terms(small_rf)$predictors, names(small)[-1])
})

test_that("public one- and two-model comparisons use the appropriate eligible observations", {
  for (missing in c(FALSE, TRUE)) {
    d = small
    if (missing) {
      d$y[3] = NA
      d$b[5] = NA
    }
    d$unused = NA_real_
    reduced = lm(y ~ a, data=d)
    full = lm(y ~ a + b, data=d)
    for (two_models in c(FALSE, TRUE)) {
      eligible = d[complete.cases(d[if (two_models) c("y", "a", "b") else c("y", "a")]), ]
      for (method in c("marginal", "observed")) {
        actual = compare_fits(y ~ a, d, reduced, if (two_models) full else NULL,
                                return.preds=TRUE, prediction_method=method)
        expect_length(unique(actual$model), if (two_models) 2 else 1)
        for (label in unique(actual$model)) {
          curve = actual[actual$model == label, ]
          fit = if (label == "reduced (lm)") reduced else full
          expected = vapply(as.character(curve$a), function(level) {
            profile = if (method == "observed") eligible[eligible$a == level, ] else eligible
            profile$a = factor(rep(level, nrow(profile)), levels=levels(d$a))
            mean(predict(fit, newdata=profile))
          }, numeric(1))
          expect_equal(curve$prediction, unname(expected), tolerance=1e-10)
        }
        plot = compare_fits(y ~ a, d, reduced, if (two_models) full else NULL,
                              prediction_method=method, jitter=FALSE)
        raw = ggplot2::ggplot_build(plot)$data[[1]]
        expect_equal(nrow(raw), nrow(eligible))
        expect_equal(sort(raw$y), sort(eligible$y))
      }
    }
  }
})

test_that("comparison data errors identify missing predictors", {
  d = data.frame(y=1:3, x=3:1)
  expect_error(compare_fits_errors(d, "y", c("x", "missing"),
                                  testme=c("x", "missing")),
               "Specifically: missing\\.")
})
  
test_that("get_model_n works", {
  expect_equal(get_model_n(small_randomForest), nrow(small))
  expect_equal(get_model_n(small_rf), nrow(small))
  expect_equal(get_model_n(small_mixed_mod), nrow(small_mixed))
  expect_equal(get_model_n(lm(y~x, data=small)), nrow(small))
  expect_equal(get_model_n(rpart::rpart(y~a + b, data=small)), nrow(small))
})

test_that("bin_if_theres_a_flexplot_formula works", {
  expect_true(is.null(bin_if_theres_a_flexplot_formula(y~z+b+x, data=small)$x_binned))
  expect_true(!is.null(bin_if_theres_a_flexplot_formula(y~a+b|x, data=small)$x_binned))
})
