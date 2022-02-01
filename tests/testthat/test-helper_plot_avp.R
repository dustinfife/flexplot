context("helper_plot_avp works")

test_that("label_avp_axis works", {
  expect_equal(label_avp_axis(y~x+z), "y | x + z")
})

test_that("make_avp_formula works", {
  expect_equal(make_avp_formula(y~x+z, y~a_b)[[3]], "y | a_b") 
  expect_equal(make_avp_formula(y~x+z, x=1)[[3]], "y | z")
  expect_equal(make_avp_formula(y~x+z, x=2)[[3]], "y | x") 
  # for when the y variable is a string in predictors as well
  expect_equal(deparse(make_avp_formula(y~y_old + b, y~y_old + b + c)[[2]]), "residuals ~ y_old + b")
})
test_that("check_variables_in_lm works", {
  expect_null(check_variables_in_lm(y~x+x2, y~x))
  expect_error(check_variables_in_lm("a", y~x))
  expect_null(check_variables_in_lm(y~xz, y~x))
  expect_error(check_variables_in_lm(x~y + z, y~x))
})

test_that("find_variable_of_interest works", {
  expect_equal(find_variable_of_interest(letters[1:3], NULL), "c")
  expect_error(find_variable_of_interest(letters[1:3], 4))
  expect_error(find_variable_of_interest(letters[1:3], "d"))
  expect_equal(find_variable_of_interest(letters[1:3], 2), "b")
  expect_equal(find_variable_of_interest(letters[1:3], "b"), "b")
})

test_that("prep_data_for_avp works", {
  expect_false(tibble::is_tibble(prep_data_for_avp(tibble::tibble(data.frame(x=1:3, y=1:3)), c("x", "y"))))
  suppressWarnings(expect_true(nrow(prep_data_for_avp(data.frame(x=c(1,2,NA), y=c(4,5,6)), c("x", "y")))==2))
  expect_true(nrow(prep_data_for_avp(data.frame(x=c(1,2,NA), y=c(4,5,6), z=5:7), c("y", "z")))==3)
})
