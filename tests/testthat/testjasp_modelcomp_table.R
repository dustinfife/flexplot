context("test estimates for model comparison functions in JASP")

test_that("filling in model comparison table", {
  
  set.seed(1212)
  x = rnorm(111); y = rnorm(111); b = sample(c("a", "b"), 111, T)
  moda = lm(y~x + b); mod = lm(y~x); mod2 = lm(y~1)
  testthat::expect_equal(round(return_baseline_rsq(moda)*100, digits=2), 1.87)
  testthat::expect_equal(return_baseline_rsq(mod), "")
  testthat::expect_equal(return_baseline_rsq(mod2), "")
  
  formula = formula(y~1); testthat::expect_equal(return_baseline_model(formula), "Baseline: Mean = 0")
  formula = formula(y~x); testthat::expect_equal(return_baseline_model(formula), "Baseline: Mean Model")
  formula = formula(y~x + b); testthat::expect_equal(return_baseline_model(formula), "Baseline: Full Model")
  
  testthat::expect_equal(return_baseline_df(lm(weight.loss~motivation, data=exercise_data)), 199)
  testthat::expect_equal(return_baseline_df(lm(weight.loss~motivation + therapy.type, data=exercise_data)), 196)
  testthat::expect_equal(return_baseline_df(lm(weight.loss~1, data=exercise_data)), 200)  
  
  testthat::expect_equal(
    complete_teststat_when_one_var(
      model = lm(weight.loss~therapy.type, data=exercise_data),
      "therapy.type")[[1]],
    "F")
  testthat::expect_equal(
    complete_teststat_when_one_var(
      model = lm(weight.loss~rewards, data=exercise_data),
      "rewards")[[1]],
    "t")
  testthat::expect_equal(
    complete_teststat_when_one_var(
      model = lm(weight.loss~motivation, data=exercise_data),
      "motivation")[[1]],
    "r")
}
)  