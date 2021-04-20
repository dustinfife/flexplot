context("margin_plot functions works as expected")
options(warn=-1)
set.seed(12312)
test_that("marginal_plot works", {
  a = flexplot(ptsd~speed | agility + strength, data=avengers)
  vdiffr::expect_doppelganger("marginal plot", marginal_plot(a))
})

test_that("marginal_plot with lm/loess/quadratic", {
  b = flexplot(ptsd~speed | agility + strength, data=avengers, method="quadratic")
  c = flexplot(ptsd~speed | agility + strength, data=avengers, method="lm")
  vdiffr::expect_doppelganger("marginal plot with quadratic", marginal_plot(b))
  vdiffr::expect_doppelganger("marginal plot with lm", marginal_plot(c))
})

test_that("return_panel_vars works", {
  expect_error(return_panel_vars(y~x))
  expect_error(return_panel_vars(y~x+b))
  expect_equal("a", return_panel_vars(y~x|a))
  expect_true(length(return_panel_vars(y~x|a+b))==2)
})

test_that("make_paneled_formula works", {
  expect_equal(make_paneled_formula("y", "x", "a", c("b", "c")), y~x+a|b+c)
  expect_equal(make_paneled_formula("y", "x", panel= c("b", "c")), y~x|b+c)
  expect_equal(make_paneled_formula("y", "x", panel="a"), y~x|a)
})


test_that("replace_text_with_binned works", {
  d = data.frame("text" = 1:10, "b_binned" = 1:10, "c" = 1:10)
  expect_true(replace_text_with_binned(d, c("b", "c"))[1]=="b_binned")
  expect_true(replace_text_with_binned(d, c("c"))[1]=="c")
  expect_true(typeof(common_layers_margin_plot())=="list")
})

test_that("extract_plot_method works", {
  a = flexplot(ptsd~speed | agility + strength, data=avengers)
  expect_true(extract_plot_method(a)$method=="loess")
})