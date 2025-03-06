test_that("flexplot_convert_to_categorical works", {
  expect_true(is.ordered(flexplot_convert_to_categorical(exercise_data %>% mutate(gender = as.numeric(gender)), "gender")$gender))
  expect_true(is.ordered(flexplot_convert_to_categorical(exercise_data %>% mutate(gender = as.numeric(gender)), c("therapy.type", "gender"))$gender))
})

#data, col, check_pred = FALSE, pred = NULL
test_that("convert_if_less_than_five works", {
  expect_equal(class(convert_if_less_than_five(small, "y")$y), "numeric")
  expect_equal(class(convert_if_less_than_five(small, "y_pois")$y_pois)[1], "ordered")
})
