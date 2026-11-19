context("avps")

test_that("added.plot works", {
  set.seed(1212)
  expect_flexplot_snapshot("plot_avp/avp-based-on-formula.svg", "avp based on formula"          , avp(y ~ x + a, data=small))
  expect_flexplot_snapshot("plot_avp/avp-with-lm-formula.svg", "avp with lm formula"           , avp(y ~ x + a, lm_formula = y~z, data=small))
  expect_flexplot_snapshot("plot_avp/avp-with-x-specified.svg", "avp with x specified"          , avp(y ~ x + a, x=1, data=small))
  expect_flexplot_snapshot("plot_avp/avp-with-x-specified-as-string.svg", "avp with x specified as string", avp(y ~ x + a, x="a", data=small))
  expect_flexplot_snapshot("plot_avp/avp-with-logistic.svg", "avp with logistic"             , avp(y_bin ~ x, lm_formula = y_bin ~ z, data=small, method="logistic"))
})