# Integration tests for the public model.comparison() interface.
# Numerical calculations are tested separately in helper tests.

test_that("model.comparison returns expected components for lm and glm", {
  reduced = lm(weight.loss ~ motivation, data = exercise_data)
  full = lm(weight.loss ~ motivation + therapy.type, data = exercise_data)
  result = model.comparison(reduced, full)
  expect_true("statistics" %in% names(result))
  expect_true("aic" %in% names(result$statistics))
  
  logistic_full = glm(injury ~ safety + attention, data = tablesaw.injury,
                      family = binomial)
  logistic_reduced = glm(injury ~ attention, data = tablesaw.injury,
                         family = binomial)
  logistic_result = model.comparison(logistic_full, logistic_reduced)
  expect_true("statistics" %in% names(logistic_result))
})

test_that("nonnested comparisons omit the nested-model p-value", {
  model1 = lm(weight.loss ~ motivation, data = exercise_data)
  model2 = lm(weight.loss ~ health + therapy.type, data = exercise_data)
  result = model.comparison(model1, model2)
  expect_false("p" %in% names(result$statistics))
})

test_that("random forest comparisons return predictions only", {
  set.seed(1212)
  forest = randomForest::randomForest(weight.loss ~ therapy.type,
                                      data = exercise_data, ntree = 20)
  linear = lm(weight.loss ~ therapy.type, data = exercise_data)
  result = model.comparison(forest, linear)
  expect_length(result, 1)
})

test_that("different estimation samples trigger a message", {
  model1 = lm(weight.loss ~ muscle.gain.missing + therapy.type,
              data = exercise_data)
  model2 = lm(weight.loss ~ therapy.type, data = exercise_data)
  expect_message(model.comparison(model1, model2),
                 "two different datasets")
})

test_that("mixed-model comparison reports ML AIC values", {
  reduced = lme4::lmer(ALCUSE ~ 1 + (1 | ID), data = alcuse)
  full = lme4::lmer(ALCUSE ~ AGE_14 + (1 | ID), data = alcuse)
  result = model.comparison(reduced, full)$statistics
  
  expected = c(AIC(lme4::refitML(reduced)), AIC(lme4::refitML(full)))
  expect_equal(as.numeric(result$aic), expected, tolerance = 0.001)
})
