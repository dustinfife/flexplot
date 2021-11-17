context("helper_jamovi functions")

# create an R6 object

test_that("jamovi_formula works", {
  expect_equal(jamovi_formula("a", c("b", "c"), c("d", "e")), as.formula(a~b+c|d+e))
  expect_equal(jamovi_formula("a", c("b", "c"))             , as.formula(a~b+c))
  expect_equal(jamovi_formula("a")             , as.formula(a~1))
  expect_equal(jamovi_formula("a ")             , as.formula(`a `~1))
})

test_that("escape_spaces_in_formulas works", {
  
  escape_spaces_in_formulas("a")
  expect_equal(escape_spaces_in_formulas("a"), "a")
  expect_equal(escape_spaces_in_formulas("a "), "`a `")
  expect_equal(escape_spaces_in_formulas("`a `"), "`\\`a \\``")
  expect_equal(escape_spaces_in_formulas("a b"), "`a b`")
})  


