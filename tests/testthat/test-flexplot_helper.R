test_that("factor.to.logistic works", {
  expect_true(class(factor.to.logistic(small_logistic, "y")$y)=="numeric")
  expect_equal(factor.to.logistic(small_logistic, "y_numb", labels=T), c(0,1))
  expect_equal(factor.to.logistic(small_logistic, "y_numb"), small_logistic)
  expect_equal(factor.to.logistic(small_logistic, "y_char", method="lm"), small_logistic)
  expect_true(factor.to.logistic(small_logistic, "y_ord", method="logistic")$y_ord %>% is.numeric)
  expect_true(factor.to.logistic(small_logistic, "y_char", method="logistic")$y_char %>% is.numeric)
})

test_that("return_labels_for_logistic_regression works", {
  expect_null(return_labels_for_logistic_regression(small_logistic, "y_numb", method="gbb"))
  expect_null(return_labels_for_logistic_regression(small_logistic, "y", method="logistic"))
  expect_equal(return_labels_for_logistic_regression(small_logistic, "y_ord", method="logistic"), c("no", "yes"))
  expect_equal(return_labels_for_logistic_regression(small_logistic, "y_numb", method="logistic"), c(0,1))
  expect_equal(return_labels_for_logistic_regression(small_logistic, "y_char", method="logistic"), c("0-yes", "1-no"))
})
