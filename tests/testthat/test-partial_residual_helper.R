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