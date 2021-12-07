context("helper_estimates")

test_that("return_levels_or_mean", {
  expect_equal(return_levels_or_mean("a", c("a"), small)%>%as.character, "a")
  expect_equal(return_levels_or_mean("z", "a", small), .3853, tol=.01)
})

test_that("return_averages works", {
  model = lm(y~a + z +b , data=small)
  expect_equal(return_averages(model, "a", TRUE)$a%>%as.character, "a")
  expect_equal(return_averages(model, "z", TRUE)$z, .3853, tol=.01)
  expect_message(return_averages(model, "a", FALSE))
  expect_null(return_averages(model, character(0)))
})

test_that("return_factor_levels works", {
  expect_equal(return_factor_levels("a", small)$a%>%as.character, c("a", "b"))
  expect_null(return_factor_levels(character(0), small))  
  expect_equal(return_factor_levels(c("a", "b"), small)$b%>%as.character, c("z", "y", "x"))
})

test_that("return_plus_minus_one_sd works", {
  expect_equal(return_plus_minus_one_sd(1:10)[1], 8.52765, tol=.01)
  expect_equal(length(return_plus_minus_one_sd(1:10)), 2)
})

test_that("anchor.predictions works for categorical predictors", {
  linear.model = lm(weight.loss~health + gender, data= exercise_data)
  expect_message(anchor.predictions(linear.model, "gender")$prediction[1])
  linear.model = lm(weight.loss~therapy.type + gender, data= exercise_data)
  expect_equal(anchor.predictions(linear.model, c("therapy.type", "gender"))$prediction[1],
               4.3475, tolerance = .002)
})

test_that("bf.bif works", {
  expect_equal(bf_bic(lm(y~x + a, data=small), lm(y~x * a, data=small)), 3.426344, tol = .01)
  expect_equal(bf_bic(lm(y~x + a, data=small), lm(y~x * a, data=small), invert=T), 1/3.426344, tol = .01)
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model2 = lm(weight.loss~motivation * therapy.type, data=exercise_data)
  expect_equal(bf.bic(model1, model2), 95.29443) 
})  

test_that("return_mean_for_intercept_models works", {
  expect_equal(return_mean_for_intercept_models(lm(y~1, data=small))$Mean, .2988, tol=.01)
  expect_equal(return_mean_for_intercept_models(lm(y~1, data=small), outcome="y", data=small)$Mean, .2988, tol=.01)
})

test_that("get_factor_numeric_names works", {
  expect_equal(get_factor_numeric_names(small, c("x", "y", "a", "b"))$cat, c("a", "b"))
  expect_equal(get_factor_numeric_names(small, character(0))$cat, character(0))
})

test_that("delta_rsquare works", {
  a = 1:10; b = 1:10; c = c(1,2,3,4,5,5,4,3,2,1)
  suppressWarnings(expect_equal(delta_rsquare(lm(a~b))%>%as.numeric, 1))
  suppressWarnings(expect_equal(delta_rsquare(lm(a~c))%>%as.numeric, 0, tol=.01))
})

test_that("create_empty_estimates_matrices works", {
  expect_equal(dim(create_empty_estimates_matrices(small, c("b", "a"))$coef.matrix), c(5,5))
  expect_equal(dim(create_empty_estimates_matrices(small, c("b", "a"))$difference.matrix), c(4,6))
})

test_that("populate_estimates_matrix works", {
  populate_estimates_matrix(lm(y~a + b, data=small))
})

test_that("populate_estimates_numeric works", {
  expect_equal(populate_estimates_numeric(lm(y~a+z + I(z^2), data=small))$estimate[2], .548, tol=.01)
})

test_that("standardized.beta returns the correct value", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = standardized.beta(model1)
  names(results) = NULL
  expect_equal(results[2], 0.3708347) 
})

test_that("report_r_squared works", {
  expect_equal(report_r_squared(lm(y~x, data=small))[3], .6346)
})

test_that("report_correlation works", {
  expect_equal(report_correlation(lm(y~x, data=small)), .6061, tol=.01)
  expect_equal(report_correlation(lm(y~x+a, data=small)), NA)
})

mod_full    = lm(y~a + b + z, data=small)
mod_reduced = lm(y~b + z, data=small)
test_that("model_comparison_for_one_term works", {
  expect_equal(model_comparison_for_one_term(1, object = mod_full)$rsq, summary(mod_full)$r.squared -summary(mod_reduced)$r.squared)
})

test_that("model_comparison_all_terms works", {
  expect_null(model_comparison_all_terms(object=mod_full, mc=FALSE))
  expect_null(model_comparison_all_terms(object=lm(y~a, data=small), mc=FALSE))
  mc_mod = model_comparison_all_terms(lm(y~a + b, data=small))
  expect_equal(mc_mod$rsq[2], .0214, tol=.01)
  expect_equal(mc_mod$bayes.factor[2], .2688, tol=.01)
})

