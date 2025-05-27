library(testthat)

test_that("return_ranges_or_unique returns sequence for numeric", {
  df = data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  result = return_ranges_or_unique("x", df, resolution = 50)
  
  expect_type(result, "double")
  expect_length(result, 50)
  expect_equal(min(result), 1)
  expect_equal(max(result), 10)
})

test_that("return_ranges_or_unique returns unique values for factor/character", {
  df = data.frame(x = factor(c("a", "b", "a", "c")), y = c("dog", "cat", "dog", "mouse"))
  
  result_factor = return_ranges_or_unique("x", df)
  result_char   = return_ranges_or_unique("y", df)
  
  expect_setequal(result_factor, levels(df$x))
  expect_setequal(result_char, unique(df$y))
})

test_that("create_prediction_grid creates full grid of combinations", {
  df = data.frame(
    num = 1:10,
    cat = rep(c("a", "b"), 5)
  )
  
  grid = create_prediction_grid(c("num", "cat"), df, resolution = 5)
  
  expect_s3_class(grid, "data.frame")
  expect_named(grid, c("num", "cat"))
  expect_equal(nrow(grid), 5 * length(unique(df$cat)))
})

test_that("shift_by_threshold subtracts group-specific values correctly", {
  x = c(10, 20, 30, 40)
  group_id = c("A", "B", "A", "B")
  thresholds = c(A = 5, B = 10)
  
  result = shift_by_threshold(x, group_id, thresholds) %>% as.numeric
  expect_equal(result, c(5, 10, 25, 30))
})
