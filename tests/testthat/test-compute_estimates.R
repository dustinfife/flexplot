context("compute_estimates works")

set.seed(1212)
test_that("estimates from linear models", {

  expect_equal(estimates(lm(y~b,   data=small))$difference.matrix$cohens.d[1], -.4692, tolerance = 0.002)
  expect_equal(estimates(lm(y~a+b, data=small))$difference.matrix$cohens.d[1]
  #expect_equal(estimates(mod)$difference.matrix$cohens.d[4], -.1024, tolerance = 0.002)
  
  ##### interaction
  mod = lm(weight.loss~therapy.type * gender, data=exercise_data)	
  expect_true(estimates(mod)$semi.p[3]<0.01 & estimates(mod)$semi.p[3]>0.009)
  
  #### numeric variables
  cat1 = lm(weight.loss~motivation, data=exercise_data)	
  expect_equal(estimates(cat1)$numbers.summary$estimate[2], .1856, tolerance = 0.002)
  
  ##### categorical + numeric
  mod = lm(weight.loss~motivation + income, data=exercise_data)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], -.1898, tolerance = 0.002)
  
  ##### two categorical and one numeric
  mod = lm(weight.loss~motivation + therapy.type + gender, data=exercise_data)	
  expect_equal(estimates(mod)$difference.matrix$difference[4], -.93229, tolerance = 0.002)
  
  ##### two numeric and one categorical
  mod = lm(weight.loss~motivation + income + gender, data=exercise_data)
  expect_true(estimates(mod)$semi.p[3]<0.01 & estimates(mod)$semi.p[3]>0.007)	
  
  ##### polynomial
  mod = lm(weight.loss~motivation + I(motivation^2), data=exercise_data)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], -.53, tolerance = 0.002)
  
  mod = lm(weight.loss~1, data=exercise_data)
  expect_equal(estimates(mod)$Mean, 6.56, tolerance = 0.002)
})

test_that("estimates from mixed models", {
  
  mod1 = lme4::lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)  
  expect_true(all(names(estimates(mod1)) %in% c("fixed", "r.squared", "rand", "icc")))
  expect_equal(as.numeric(round(estimates(mod1)$icc[1]*1000)), expected=505, tolerance = .01)
  
  # with tibbles
  mod2 = update(mod1, data=as_tibble(alcuse))
  expect_true(all(names(estimates(mod2)) %in% c("fixed", "r.squared", "rand", "icc")))
  
  # with missing data in some columns
  mod3 = lme4::lmer(muscle.gain.missing~motivation + (1|health), data=exercise_data)  
  expect_true(all(names(estimates(mod3)) %in% c("fixed", "r.squared", "rand", "icc")))
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
  
test_that("bic works", {
  model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
  model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
  expect_equal(bf.bic(model.me, model.int, invert=T), .01049, tolerance=.001)
})  

test_that("icc works", {
  data(math)
  mod = lmer(MathAch~1 + (1|School), data=math)
  expect_equal(icc(mod)$icc, .18, tol=.01)
})

test_that("removing interactions works", {
  mod = lm(kills~agility*speed, data=avengers)
  expect_true(length(remove_interaction_terms(mod))==2)
})

test_that("generate_grid_predictions", {
  expect_equal(nrow(generate_grid_predictions(list(a=1:3, b=4:5), list(c = c('a', 'b')), NULL)), 12)
   expect_null(generate_grid_predictions(NULL, NULL, NULL))
})

