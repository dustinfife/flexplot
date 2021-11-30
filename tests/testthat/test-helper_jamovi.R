context("helper_jamovi functions")

# create an R6 object

test_that("jamovi_formula works", {
  expect_equal(jamovi_formula("a", c("b", "c"), c("d", "e")), as.formula(a~b+c|d+e))
  expect_equal(jamovi_formula("a", c("b", "c")), as.formula(a~b+c))
  expect_equal(jamovi_formula("a"), as.formula(a~1))
  expect_equal(jamovi_formula("a "), as.formula(`a `~1))
})

test_that("escape_spaces_in_formulas works", {
  
  escape_spaces_in_formulas("a")
  expect_equal(escape_spaces_in_formulas("a"), "a")
  expect_equal(escape_spaces_in_formulas("a "), "`a `")
  expect_equal(escape_spaces_in_formulas("`a `"), "`\\`a \\``")
  expect_equal(escape_spaces_in_formulas("a b"), "`a b`")
})  

test_that("get_fitted_line works", {
  expect_equal(get_fitted_line(NULL),"loess")
  expect_equal(get_fitted_line("Loess"),"loess")
  expect_equal(get_fitted_line("Regression"),"lm")
  expect_equal(get_fitted_line("Logistic"),"logistic")
  expect_equal(get_fitted_line("Polynomial"),"polynomial")
  expect_equal(get_fitted_line("Cubic"),"cubic")
  expect_equal(get_fitted_line("Robust"),"rlm")
  expect_equal(get_fitted_line("Time Series"),"loess")
  expect_equal(get_fitted_line("asdfasdf"),"loess")
})

test_that("ifelse_null works", {
  expect_null(ifelse_null(aasd, a == 1, a, NULL))
  a = 1
  expect_equal(ifelse_null(NULL, a==1, a, 4), 4)
  expect_equal(ifelse_null(a, a == 1, a, NULL), 1)
  expect_equal(ifelse_null(a, a == 2, a, 4), 4)  
})
