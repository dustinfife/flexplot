context("use estimates to report effect sizes")

data(exercise_data)
d = exercise_data
d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
set.seed(1212)
test_that("estimates from linear models", {

  cat1 = lm(weight.loss~therapy.type, data=d)	
  expect_equal(estimates(cat1)$difference.matrix$cohens.d[2], .6305667, tolerance = 0.002)
  
  ##### two categoricals
  mod = lm(weight.loss~therapy.type + gender, data=exercise_data)	
  expect_equal(estimates(mod)$difference.matrix$cohens.d[4], .1024, tolerance = 0.002)
  
  ##### interaction
  mod = lm(weight.loss~therapy.type * gender, data=exercise_data)	
  expect_true(estimates(mod)$semi.p[3]<0.01 & estimates(mod)$semi.p[3]>0.009)
  
  #### numeric variables
  cat1 = lm(weight.loss~motivation, data=exercise_data)	
  expect_equal(estimates(cat1)$numbers.summary$estimate[2], .1856, tolerance = 0.002)
  
  ##### categorical + numeric
  mod = lm(weight.loss~motivation + income, data=exercise_data)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], 0.06670681, tolerance = 0.002)
  
  ##### two categorical and one numeric
  mod = lm(weight.loss~motivation + therapy.type + gender, data=exercise_data)	
  expect_equal(estimates(mod)$difference.matrix$difference[4], .93229, tolerance = 0.002)
  
  ##### two numeric and one categorical
  mod = lm(weight.loss~motivation + income + gender, data=exercise_data)
  expect_true(estimates(mod)$semi.p[3]<0.01 & estimates(mod)$semi.p[3]>0.007)	
  
  ##### polynomial
  mod = lm(weight.loss~motivation + I(motivation^2), data=exercise_data)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], .68279, tolerance = 0.002)
  
  mod = lm(weight.loss~1, data=exercise_data)
  expect_equal(estimates(mod)$Mean, 6.56, tolerance = 0.002)
})

test_that("tests that previously produced errors", {
  mod2 = lm(y~x + z + q, 
            data=small %>%
              mutate(q =0)
  )
  expect_error(estimates(mod2))
})

test_that("estimates from generalized linear models", {
  data("criminal_data")
  d = criminal_data
  mod = glm(rape~ses , data=d, family=binomial)
  expect_equal(estimates(mod)$raw.coefficients[1], -6.45, tolerance = .001)
  
  
  mod = glm(convictions~ses + empathy + depression, data=d, family=poisson)
  expect_equal(estimates(mod)$raw.coefficients[1], -.798)
  
  mod = glm(aggression~ses + empathy + depression, data=d, family=Gamma)
  expect_equal(estimates(mod)$raw.coefficients[1], .164)
  
  mod = glm(aggression~ses*depression, data=d, family=Gamma)
  expect_equal(estimates(mod)$raw.coefficients[1], .161)
})  



