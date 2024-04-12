test_that("convert_ordered_factors works", {
  small_ordered = small %>% mutate(y_bin = ordered(y_bin))
  expect_true(convert_ordered_factors(small_ordered, small)$y_bin %>% max == 2)
  expect_true(convert_ordered_factors(small        , small)$y_bin %>% max == 1)
})

test_that("summary_function_depending_on_class works", {
  expect_equal(summary_function_depending_on_class(1:10), 5.5)
  expect_equal(summary_function_depending_on_class(letters[1:10]), "a")
  expect_true(summary_function_depending_on_class(letters[1:10]%>%as.factor) %>% is.factor)
  expect_true(summary_function_depending_on_class(letters[1:10]%>%ordered) %>% is.ordered)
})

test_that("identify_residualized_variables works", {
  expect_equal(identify_residualized_variables(y~a+b+c, ~a), "y | b + c")
  expect_equal(identify_residualized_variables(y~a+b+c, ~a+b), "y | c")
  expect_equal(identify_residualized_variables(y~a+b+c, NULL), "y | a + b + c")
  expect_equal(identify_residualized_variables(y~a+b+I(b^2), ~b + I(b^2)), "y | a")
})
  
test_that("find_predictors_in_formula works", {
  expect_equal(find_predictors_in_formula(y~a+b), "a + b")
  expect_equal(find_predictors_in_formula(~a+b), "a + b")
  expect_equal(find_predictors_in_formula(y+g~a+b), "a + b")
  expect_equal(find_predictors_in_formula(y~1), "1")
})

