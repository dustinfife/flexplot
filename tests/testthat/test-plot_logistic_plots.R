context("logistic plots works")
options(warn = -1)
set.seed(233)
require(vdiffr)

test_that("logistic plots work", {
  expect_flexplot_snapshot("plot_logistic_plots/logistic-with-numbers.svg", "logistic with numbers", flexplot(y_bin~x + a | b, data=small, method="logistic"))
  expect_flexplot_snapshot("plot_logistic_plots/logistic-with-labels.svg", "logistic with labels", flexplot(a~x + b | z, data=small, method="logistic"))
  mod_numb = glm(y_bin~x + z, data=small, family=binomial)
  mod_cat  = glm(a~ z, data=small%>%mutate(a = factor(a)), family=binomial)
  expect_flexplot_snapshot("plot_logistic_plots/visualize-logistic-with-numbers.svg", "visualize logistic with numbers", visualize(mod_numb, plot="model"))
  expect_flexplot_snapshot("plot_logistic_plots/visualize-logistic-with-cat.svg", "visualize logistic with cat", visualize(mod_cat, plot="model"))
  expect_flexplot_snapshot("plot_logistic_plots/compare-fits-logistic-with-numbers.svg", "compare.fits logistic with numbers", compare.fits(y_bin~x | z, data=small, mod_numb))
  expect_flexplot_snapshot("plot_logistic_plots/compare-fits-logistic-with-factors.svg", "compare.fits logistic with factors", compare.fits(a ~ z, data=small, mod_cat))
})

test_that("logistic_overlay works", {
  p = flexplot(died ~ agility, data = avengers, method = "logistic")
  expect_flexplot_snapshot("plot_logistic_plots/logistic-overlay-from-plot.svg", "logistic overlay from plot", logistic_overlay(plot = p, n_bins = 15, type = "dot"))

  # Create new plot with overlay
  expect_flexplot_snapshot("plot_logistic_plots/logistic-overlay-with-bars.svg", "logistic overlay with bars", logistic_overlay(formula = died ~ agility, data = avengers,
                   n_bins = 8, type = "bar"))
})

options(warn = 0)
