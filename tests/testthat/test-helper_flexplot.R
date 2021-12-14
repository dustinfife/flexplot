test_that("formula_functions works", {
  expect_true(is.factor(formula_functions(y~a + as.factor(b), small)$data$b))
})

test_that("perform_function works", {
  expect_true(all(sqrt(avengers$kills) == perform_function("sqrt(kills)", avengers)))
})

test_that("get_var_names_within_function works", {
  expect_true(get_var_names_within_function("sqrt(a)")=="a")
  expect_true(get_var_names_within_function("sqrt(a)", return.var = F)(4)==2)
})

test_that("reorder_x_by_center works", {
  expect_equal(levels(reorder_x_by_center(small, formula = y~b)$b), c("z", "y", "x"))
  expect_equal(levels(reorder_x_by_center(small, formula = y~b, spread="sterr")$b), c("z", "y", "x"))
})

test_that("reorder_x_by_n works", {
  expect_true(is.factor(reorder_x_by_n(small, "b")$b))
})