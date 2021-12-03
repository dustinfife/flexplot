context("avps")

test_that("added.plot works", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp based on formula"          , avp(y ~ x + a, data=small))
  vdiffr::expect_doppelganger("avp with lm formula"           , avp(y ~ x + a, lm_formula = y~z, data=small))
  vdiffr::expect_doppelganger("avp with x specified"          , avp(y ~ x + a, x=1, data=small))
  vdiffr::expect_doppelganger("avp with x specified as string", avp(y ~ x + a, x="a", data=small))
  vdiffr::expect_doppelganger("avp with logistic"             , avp(y_bin ~ x, lm_formula = y_bin ~ z, data=small, method="logistic"))
})