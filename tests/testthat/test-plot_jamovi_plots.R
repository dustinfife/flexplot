context("plot_jamovi_plots works")

test_that("jamovi_plots works", {
  set.seed(2323)
  vdiffr::expect_doppelganger("jamovi_plots avp",   jamovi_plots(y~x+z,   data=small, options = list(resid=TRUE)))
  vdiffr::expect_doppelganger("jamovi_plots ghost", jamovi_plots(y~x|z,   data=small, options = list(ghost=TRUE))%>%suppressMessages())
  vdiffr::expect_doppelganger("jamovi_plots line",  jamovi_plots(y~x | b, data=small, options = list(line="Regression")))
  vdiffr::expect_doppelganger("jamovi_plots histogram", jamovi_plots(y~1, data=small)) 
})

