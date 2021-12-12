context("compute_estimates works")

set.seed(1212)
test_that("estimates from linear models", {

  expect_equal(estimates(lm(y~b,   data=small))$difference.matrix$cohens.d[1], -.4692, tolerance = 0.002)
  expect_equal(estimates(lm(y~a+b, data=small))$difference.matrix$cohens.d[1],  .30744, tolerance = .001)
  expect_equal(estimates(lm(y~a*b, data=small))$semi.p[3] %>% as.numeric,  .088, tolerance = .001)
  expect_equal(estimates(lm(y~x, data=small))$numbers.summary$estimate[1], .22819, tolerance = .001)
  expect_equal(estimates(lm(y~x + I(x^2), data=small))$numbers.summary$variables[3], "I(x^2)")

})

test_that("estimates from mixed models", {
  
  mod1 = lme4::lmer(y~x + (1 | id), data=small_mixed)  
  expect_true(all(names(estimates(mod1)) %in% c("fixed", "r.squared", "rand", "icc")))
  expect_equal(as.numeric(round(estimates(mod1)$icc[1]*1000)), expected=266, tolerance = .01)
  
  # with tibbles
  mod2 = update(mod1, data=as_tibble(small_mixed))
  expect_true(all(names(estimates(mod2)) %in% c("fixed", "r.squared", "rand", "icc")))
  
  # with missing data in some columns
  mod3 = lme4::lmer(y~z + (1|id), data=small_mixed)
  expect_message(estimates(mod3))
})  

test_that("estimates from generalized linear models", {
  data("criminal_data")
  d = criminal_data
  mod = glm(rape~ses , data=d, family=binomial)
  expect_equal(estimates(mod)$raw.coefficients[1], -6.45)
  
  mod = glm(convictions~ses + empathy + depression, data=d, family=poisson)
  expect_equal(estimates(mod)$raw.coefficients[1], -.798)
  
  mod = glm(aggression~ses + empathy + depression, data=d, family=Gamma)
  expect_equal(estimates(mod)$raw.coefficients[1], .164)
  
  mod = glm(aggression~ses*depression, data=d, family=Gamma)
  expect_equal(estimates(mod)$raw.coefficients[1], .161)
})  
