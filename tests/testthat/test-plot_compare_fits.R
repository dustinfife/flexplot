context("compare.fits works")
set.seed(12323)
options(warn = -1)
test_that("compare_fits works with linear models", {
  full    = lm(y~a*x, data=small)
  reduced = lm(y~a+x, data=small)
  expect_doppelganger("compare_fits without model/formula", compare.fits(model1=full, model2=reduced))
})
options(warn = 0)