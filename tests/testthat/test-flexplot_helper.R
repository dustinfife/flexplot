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

test_that("flexplot_alpha_default", {
  expect_equal(flexplot_alpha_default(small, "1", .99977), .99977)
  expect_equal(flexplot_alpha_default(small, "x", .99977), .5)
  expect_equal(flexplot_alpha_default(small, "a", .99977), .2)
  expect_equal(flexplot_alpha_default(small, "z", .21), .21)
})

test_that("flexplot_generate_prediction_lines works", {
  mod1 = lm(y~a+x, data=small)
  mod2 = lm(y~a*x, data=small)
  mod3 = lm(y~a, data=small)
  preds = compare.fits(y~a|x, data=small, mod1, return.preds=T)
  testthat::local_edition(3)
  expect_snapshot(cat(flexplot_generate_prediction_lines(preds, "a", small)))
  expect_snapshot(cat(flexplot_generate_prediction_lines(preds, c("x"), small)))
  expect_snapshot(cat(flexplot_generate_prediction_lines(preds, c("x", "a"), small)))
})

test_that("flexplot_panel_variables works", {
  testthat::local_edition(3)
  expect_equal(flexplot_panel_variables(given = c(NA, NA)), "xxxx")
  expect_snapshot(cat(flexplot_panel_variables("a", c("b", "c"))))
  expect_snapshot(cat(flexplot_panel_variables("a", c(""))))
})
  
test_that("make_levels_same_for_prediction_dataset", {
  expect_equal(make_levels_same_for_prediction_dataset(small, small, "1"), small)
  expect_equal(make_levels_same_for_prediction_dataset(small, small, "x"), small)
  a = expand.grid(a=1:5, b=c("a", "b"))
  pred = a %>% mutate(b = as.character(b))
  expect_false(isTRUE(all.equal(a, pred)))
  expect_equal(levels(make_levels_same_for_prediction_dataset(a, pred, "b")$b), 
               levels(a$b)) 
})