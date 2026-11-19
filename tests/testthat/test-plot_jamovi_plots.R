context("plot_jamovi_plots works")

test_that("jamovi_plots works", {
  set.seed(2323)
  expect_flexplot_snapshot("plot_jamovi_plots/jamovi-plots-avp.svg", "jamovi_plots avp",   jamovi_plots(y~x+z,   data=small, options = list(resid=TRUE)))
  expect_flexplot_snapshot("plot_jamovi_plots/jamovi-plots-ghost.svg", "jamovi_plots ghost", jamovi_plots(y~x|z,   data=small, options = list(ghost=TRUE))%>%suppressMessages())
  expect_flexplot_snapshot("plot_jamovi_plots/jamovi-plots-line.svg", "jamovi_plots line",  jamovi_plots(y~x | b, data=small, options = list(line="Regression")))
  expect_flexplot_snapshot("plot_jamovi_plots/jamovi-plots-histogram.svg", "jamovi_plots histogram", jamovi_plots(y~1, data=small))
})

