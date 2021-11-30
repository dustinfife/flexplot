context("helper_functions")

test_that("return_factors_names", {
  expect_equal(return_factors_names(lm(y~a*b*z, data=small)), c("a", "b"))
  expect_equal(return_factors_names(lm(y~z, data=small)), character(0))
})

