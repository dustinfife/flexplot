context("helper_functions")

test_that("return_factors_names", {
  expect_equal(return_factors_names(lm(y~a*b*z)), c("a", "b"))
  expect_equal(return_factors_names(lm(y~z)), character(0))
})

