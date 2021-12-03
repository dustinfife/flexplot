context("helper_functions")

test_that("return_factors_names", {
  expect_equal(return_factors_names(lm(y~a*b*z, data=small)), c("a", "b"))
  expect_equal(return_factors_names(lm(y~z, data=small)), character(0))
})

test_that("identify_method works", {
  expect_equal(identify_method(small, "y", "1"), "loess")
  expect_equal(identify_method(small, "y", "a"), "loess")
  expect_equal(identify_method(small, "y_bin", "x"), "logistic")
  expect_equal(identify_method(small, "y", "x", "asdf"), "asdf")
  expect_equal(identify_method(small, "y", "x", NULL), "loess")  
})
