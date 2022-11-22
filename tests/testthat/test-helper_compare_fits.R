context("test helper_compare_fits")

test_that("get_re works", {
  expect_true(get_re(lmer(MathAch~SES + (SES | School), data=math))=="School")
  expect_true(get_re(lmer(MathAch~SES + (SES |School), data=math))=="School")
  expect_true(get_re(lmer(MathAch~1 + (1 | School), data=math))=="School")
  expect_null(get_re(lm(MathAch~1 + (1 | School), data=math)))
})  

test_that("whats_model2 works", {
  expect_equal(whats_model2(lm(y~x, data=small), NULL), lm(y~x, data=small))
  expect_equal(whats_model2(lm(y~x + a, data=small), lm(y~x, data=small)), lm(y~x, data=small))
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

test_that("check_missing works", {
  small_missing = small 
  small_missing$b[5] = NA
  m1m = lm(y~a,     data=small_missing)
  m2m = lm(y~a + b, data=small_missing)
  m1  = lm(y~a,     data=small)
  m2  = lm(y~a + b, data=small)
  expect_equal(nrow(check_missing(m1m, m2m, small_missing, c("a", "b"))), 26)
  expect_equal(nrow(check_missing(m1 , m2 , small        , c("a", "b"))), 27)
  expect_equal(     check_missing(m1, data=small, variables="a"), small)
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
    