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
  expect_equal(flexplot_alpha_default(small, "a", .99977), .4)
  expect_equal(flexplot_alpha_default(small, "z", .21), .21)
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


test_that("make_prediction_dataset_same_type_on_x1 works", {
  small_modified = small %>% mutate(y = round(y) %>% factor)
  tested_y = make_prediction_dataset_same_type_on_x1(small, small_modified, "y")$y
  expect_true(class(tested_y)=="numeric")
  expect_true(class(small_modified$y)!="numeric")
})

# Tests for magnet-plot data preparation and logistic dispatch.
# Place in tests/testthat/. These use synthetic data to avoid fixture dependencies.

magnet_fixture = function() {
  data.frame(
    y = c(0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1),
    x = factor(rep(c("A", "B"), each=6), levels=c("A", "B")),
    panel = rep(c("P", "Q"), each=3, times=2)
  )
}

test_that("identify_method selects logistic for binary outcomes", {
  d = magnet_fixture()
  expect_equal(identify_method(d, "y", "x"), "logistic")
  expect_equal(identify_method(d, "y", "x", method="lm"), "lm")
  expect_equal(identify_method(d, "y", "1"), "loess")
  
  d$continuous = seq_len(nrow(d))
  expect_equal(identify_method(d, "continuous", "x"), "loess")
})

test_that("magnet preparation preserves observations and scales dots", {
  d = magnet_fixture()
  out = prepare_magnet_data(d, "y", axis="x")
  expect_equal(nrow(out), nrow(d))
  expect_true(all(c("n_total", "n_0", "n_1", "stack_pos", "y_scaled") %in% names(out)))
  expect_true(all(is.finite(out$y_scaled)))
  expect_true(all(out$y_scaled >= 0 & out$y_scaled <= 1))
  expect_true(all(out$y_scaled[out$y == 0] < .5))
  expect_true(all(out$y_scaled[out$y == 1] > .5))
  expect_equal(unique(out$n_total[out$x == "A"]), 6L)
  expect_equal(unique(out$n_total[out$x == "B"]), 6L)
  expect_equal(unique(out$n_1[out$x == "A"]), 3L)
  expect_equal(unique(out$n_1[out$x == "B"]), 5L)
})

test_that("magnet stacking handles unequal group sizes and pure groups", {
  d = data.frame(y=c(0, 0, 1, 1, 1, 1, 1),
                 x=factor(c("A", "A", "A", "A", "B", "B", "B")))
  out = prepare_magnet_data(d, "y", axis="x")
  expect_equal(unique(out$n_total[out$x == "A"]), 4L)
  expect_equal(unique(out$n_total[out$x == "B"]), 3L)
  expect_true(all(is.finite(out$y_scaled)))
  expect_true(all(out$y_scaled[out$x == "B"] > .5))
})

test_that("magnet stacking respects facets", {
  d = magnet_fixture()
  out = prepare_magnet_data(d, "y", axis="x", given="panel")
  totals = aggregate(data.frame(n=rep(1, nrow(d))),
                     by=list(x=d$x, panel=d$panel), FUN=sum)
  for (i in seq_len(nrow(totals))) {
    rows = out$x == totals$x[i] & out$panel == totals$panel[i]
    expect_equal(unique(out$n_total[rows]), as.integer(totals$n[i]))
  }
})

test_that("magnet preparation rejects nonbinary coding", {
  d = magnet_fixture()
  d$y[1] = 2
  expect_error(prepare_magnet_data(d, "y", axis="x"), "0/1")
})

test_that("logistic error check permits magnet categorical x", {
  d = magnet_fixture()
  expect_null(check_error_for_logistic(c("y", "x"), d,
                                       method="logistic", axis="x", use_magnet=TRUE))
  expect_error(check_error_for_logistic(c("y", "x"), d,
                                        method="logistic", axis="x", use_magnet=FALSE))
})
