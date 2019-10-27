context("model.comparison works")

data("exercise_data")

test_that("rlm and lm can be compared", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model2 = MASS::rlm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = model.comparison(model1, model2)
  expect_output(print(results), "0.000 0.004 0.012 0.183 0.194")
})

test_that("nested models have correct r squared", {
  a = lm(weight.loss~motivation, data=exercise_data)
  b = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = model.comparison(a,b)$statistics$r.squared[2]
  expect_equal(results, .217)
})

test_that("nonnested models correctly report null for p value", {
  a = lm(weight.loss~motivation + gender, data=exercise_data)
  b = lm(weight.loss~motivation + therapy.type, data=exercise_data)		
  results = model.comparison(a,b)
  expect_null(results$p.value, NULL)
})

test_that("glm vs lm works", {
  a = lm(health~motivation + gender, data=exercise_data)
  b = glm(health~motivation + gender, data=exercise_data, family=Gamma)		
  results = model.comparison(a,b)
  expect_equal(results$statistics$aic[1], 1382.206)
})


test_that("interaction vs lm works", {
  a = lm(weight.loss~motivation *therapy.type, data=exercise_data)
  b = lm(weight.loss~motivation+therapy.type, data=exercise_data)
  results = model.comparison(a,b)
  expect_equal(results$statistics$bayes.factor[1], .01)
})


test_that("random forest vs lm works returns predictions only", {
  set.seed(1212)
  a = randomForest(weight.loss~therapy.type, data=exercise_data)
  b = lm(weight.loss~therapy.type, data=exercise_data)
  results = model.comparison(a,b)
  expect_output(print(results), "0.001 0.032 0.058 0.102 0.256")
})

test_that("model comparisons with missing data", {
  
  a = lm(weight.loss~muscle.gain.missing+therapy.type, data=exercise_data)
  b = lm(weight.loss~therapy.type, data=exercise_data)
  expect_message(model.comparison(a,b), "Note: your models were fit to two different datasets.")
})
