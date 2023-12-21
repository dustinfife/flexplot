test_that("convert_ordered_factors works", {
  small_ordered = small %>% mutate(y_bin = ordered(y_bin))
  expect_true(convert_ordered_factors(small_ordered, small)$y_bin %>% max == 2)
  expect_true(convert_ordered_factors(small        , small)$y_bin %>% max == 1)
})
