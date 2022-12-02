context("test estimates for model comparison functions in JASP")

test_that("filling in model comparison table", {
  
  set.seed(1212)
  x = rnorm(111); y = rnorm(111); b = sample(c("a", "b"), 111, T)
  moda = lm(y~x + b); mod = lm(y~x); mod2 = lm(y~1)
  testthat::expect_equal(round(return_baseline_rsq(moda)*100, digits=2), .21)
  testthat::expect_equal(return_baseline_rsq(mod), NA)
  testthat::expect_equal(return_baseline_rsq(mod2), 0)
  
  formula = formula(y~1); testthat::expect_equal(return_baseline_model(formula), "Baseline: Zero Model")
  formula = formula(y~x); testthat::expect_equal(return_baseline_model(formula), "Baseline: Mean Model")
  formula = formula(y~x + b); testthat::expect_equal(return_baseline_model(formula), "Baseline: Full Model")
  
  testthat::expect_equal(return_baseline_df(lm(weight.loss~motivation, data=exercise_data))[[2]], 199)
  testthat::expect_equal(return_baseline_df(lm(weight.loss~motivation + therapy.type, data=exercise_data))[[2]], 196)
  testthat::expect_equal(return_baseline_df(lm(weight.loss~1, data=exercise_data))[[2]], 200)  
  
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

test_that("model comparison figures out the right statistic to report", {
  model = lm(weight.loss~therapy.type*motivation, data=exercise_data)
  expect_equal(round(complete_teststat_when_one_var(model, "therapy.type:motivation", first.term=FALSE)[[2]]),3)
  
  model = lm(weight.loss~motivation, data=exercise_data)
  rval = complete_teststat_when_one_var(model, "motivation", first.term=T)$statval[[1]]
  expect_equal(rval, .3539907, tolerance = .002)
  
  model = lm(weight.loss~rewards + motivation, data=exercise_data)
  tval = complete_teststat_when_one_var(model, "rewards")$statval[[1]]
  expect_equal(tval, 10.8, tolerance = .01)
  tval = complete_teststat_when_one_var(model, term = "motivation", first.term = FALSE)$statval[[1]]
  expect_equal(tval, 7.2, tolerance = .01)
  
  model = lm(weight.loss~therapy.type + motivation, data=exercise_data)
  expect_equal(complete_teststat_when_one_var(model, "therapy.type")$statval[[1]], 9.9, .01)

  model = lm(weight.loss~therapy.type + motivation, data=exercise_data)
  expect_equal(return_term_df("F", model, "therapy.type")$df_denom[[1]], "196")
  expect_equal(return_term_df("t", model, "motivation")$df_denom[[1]], 196)
  
  
})

test_that("add panel options differentiates formulas", {
  testthat::expect_true(length(add_panel_options(as.formula("y~x | x2 + x3")))==2)
  testthat::expect_true(is.null(add_panel_options(as.formula("y~x + x2 + x3"))))
})