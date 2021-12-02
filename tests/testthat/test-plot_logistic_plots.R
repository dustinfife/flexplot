context("logistic plots works")
options(warn = -1)
require(vdiffr)

test_that("logistic plots work", {
  expect_doppelganger("logistic with numbers", flexplot(y_bin~x + a | b, data=small, method="logistic"))
  expect_doppelganger("logistic with labels", flexplot(a~x + b | z, data=small, method="logistic"))
  mod_numb = glm(y_bin~x + z, data=small, family=binomial)
  mod_cat  = glm(a~ z, data=small%>%mutate(a = factor(a)), family=binomial)
  expect_doppelganger("visualize logistic with numbers", visualize(mod_numb, plot="model"))
  expect_doppelganger("visualize logistic with numbers", visualize(mod_cat, plot="model"))
  expect_doppelganger("compare.fits logistic with numbers", compare.fits(y_bin~x | z, data=small, mod_numb))
  expect_doppelganger("compare.fits logistic with factors", compare.fits(a ~ z, data=small, mod_cat))
})

options(warn = 0)
