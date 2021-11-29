context("helper_functions")

test_that("return_factors_names", {
  expect_equal(return_factors_names(lm(y~a*b*z)), c("a", "b"))
  expect_equal(return_factors_names(lm(y~z)), character(0))
})

test_that("return_levels_or_mean", {
  expect_equal(return_levels_or_mean("a", c("a"), small), "a")
  expect_equal(return_levels_or_mean("z", "a", small), -.2065, tol=.01)
})

test_that("return_averages works", {
  model = lm(y~a + z +b , data=small)
  expect_equal(return_averages(model, "a", TRUE), list(a="a"))
  expect_equal(return_averages(model, "z", TRUE)$z, -.2065, tol=.01)
  expect_message(return_averages(model, "a", FALSE))
  expect_true(is.na(return_averages(model, character(0))))
})
  
test_that("return_factor_levels works", {
  expect_equal(return_factor_levels("a", small), list(a = c("a", "b")))
  expect_null(return_factor_levels(character(0), small))  
  expect_equal(return_factor_levels(c("a", "b"), small)[[2]], c("y", "z", "x"))
})
  