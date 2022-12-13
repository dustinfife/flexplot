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
  expect_snapshot(cat(flexplot_panel_variables("", c(""))))
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

test_that("check_se works", {
  expect_true(check_se(se=T))
  expect_true(!check_se(axis=c("a", "b")))
  expect_true(check_se(axis=c("a")))
})

test_that("flexplot_histogram works", {
  testthat::local_edition(3)
  expect_snapshot(cat(flexplot_histogram(small, "a")))
  expect_snapshot(cat(flexplot_histogram(small, "x", "qq")))
  expect_snapshot(cat(flexplot_histogram(small, "x", "density")))
  expect_snapshot(cat(flexplot_histogram(small, "x")))
})

test_that("flexplot_related works", {
  a = rnorm(10)
  b = rnorm(10, 2)
  k = data.frame(a=a, b=b) %>%
    tidyr::pivot_longer(cols = a:b, values_to="Difference", names_to="group")
  test_plot = flexplot_related(k, plot.type="boxplot")
  testthat::local_edition(3)
  expect_snapshot(cat(test_plot$p))
  expect_snapshot(cat(test_plot$points))
  expect_snapshot(cat(flexplot_related(k, plot.type="boxplot")$fitted))
  expect_snapshot(cat(flexplot_related(k, plot.type="violin")$fitted))
  expect_snapshot(cat(flexplot_related(k)$fitted))
})

test_that("flexplot_bivariate_string works", {
  testplot = flexplot_bivariate_string(small, "y", "x")
  testthat::local_edition(3)
  expect_snapshot(cat(testplot$p))
  expect_snapshot(cat(testplot$points))
  expect_snapshot(cat(flexplot_bivariate_string(small, "a", "b")$p))
  expect_snapshot(cat(flexplot_bivariate_string(small, "y", "b", plot.type="boxplot")$fitted))
  expect_snapshot(cat(flexplot_bivariate_string(small, "y", "b", plot.type="violin")$fitted))
  expect_snapshot(cat(flexplot_bivariate_string(small, "y", "b", plot.type="line")$fitted))
  expect_snapshot(cat(flexplot_bivariate_string(small, "y", "b")$fitted))
})
  
test_that("flexplot_multivariate_aes works", {
  testthat::local_edition(3)
  expect_snapshot(cat(flexplot_multivariate_aes(small, "y", small, "x")))
  expect_snapshot(cat(flexplot_multivariate_aes(small, "y", axis=c("a", "x"))))
  expect_snapshot(cat(flexplot_multivariate_aes(small, "y", axis=c("x", "a"))))
  expect_snapshot(cat(flexplot_multivariate_aes(small, "y", axis=c("x", "z"))))
})