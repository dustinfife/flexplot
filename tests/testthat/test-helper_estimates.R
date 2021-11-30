context("helper_estimates")

test_that("return_levels_or_mean", {
  expect_equal(return_levels_or_mean("a", c("a"), small), "a")
  expect_equal(return_levels_or_mean("z", "a", small), -.2065, tol=.01)
})

test_that("return_averages works", {
  model = lm(y~a + z +b , data=small)
  expect_equal(return_averages(model, "a", TRUE), list(a="a"))
  expect_equal(return_averages(model, "z", TRUE)$z, -.2065, tol=.01)
  expect_message(return_averages(model, "a", FALSE))
  expect_null(return_averages(model, character(0)))
})

test_that("return_factor_levels works", {
  expect_equal(return_factor_levels("a", small), list(a = c("a", "b")))
  expect_null(return_factor_levels(character(0), small))  
  expect_equal(return_factor_levels(c("a", "b"), small)[[2]], c("y", "z", "x"))
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