context("helper_functions")

test_that("boxcox_my_mod behaves correctly across multiple scenarios", {
  
  # --- Setup reusable datasets ---
  set.seed(123)
  x1 = rnorm(100)
  y1 = exp(1 + 0.5 * x1) + rnorm(100, sd = 0.1)
  mod1 = lm(y1 ~ x1)
  
  set.seed(1)
  x2 = rnorm(200)
  y2 = exp(2 + x2) + rnorm(200, 0.2)
  mod2 = lm(y2 ~ x2)
  
  set.seed(1)
  x3 = rnorm(50)
  z3 = rnorm(50)
  y3 = exp(x3 + z3) + rnorm(50, 0.1)
  mod3 = lm(y3 ~ x3 + z3)
  
  set.seed(1)
  x4 = rnorm(100)
  y4 = exp(x4) - 10 + rnorm(100)    # includes negatives
  mod4 = lm(y4 ~ x4)
  
  set.seed(999)
  x5 = rnorm(80)
  y5 = exp(2 + 0.3 * x5) + rnorm(80, sd = 0.05)
  mod5 = lm(y5 ~ x5)
  
  
  # --- 1. Returns a model object ---
  out1 = boxcox_my_mod(mod1)
  expect_s3_class(out1, "lm")
  
  
  # --- 2. Returns numeric lambda when requested ---
  lambda1 = boxcox_my_mod(mod1, return.lambda = TRUE)
  expect_type(lambda1, "double")
  
  
  # --- 3. Lambda should be near zero for log-like data ---
  lambda2 = boxcox_my_mod(mod2, return.lambda = TRUE)
  expect_lt(abs(lambda2 - 0), 0.3)
  
  
  # --- 4. Handles negative values by shifting ---
  out4 = boxcox_my_mod(mod4)
  expect_s3_class(out4, "lm")   # should not error
  
  
  # --- 5. Predictors remain unchanged ---
  out3 = boxcox_my_mod(mod3)
  expect_equal(out3$model$x3, mod3$model$x3)
  expect_equal(out3$model$z3, mod3$model$z3)
  
  
  # --- 6. Outcome must change after transformation ---
  expect_false(isTRUE(all.equal(out1$model$y1, mod1$model$y1)))
  
  
  # --- 7. Reproducible lambda values ---
  lambda_rep1 = boxcox_my_mod(mod1, return.lambda = TRUE)
  lambda_rep2 = boxcox_my_mod(mod1, return.lambda = TRUE)
  expect_equal(lambda_rep1, lambda_rep2)
  
  
  # --- 8. If lambda ≈ 0, transformation should behave like log ---
  lambda5 = boxcox_my_mod(mod5, return.lambda = TRUE)
  out5 = boxcox_my_mod(mod5)
  
  if (abs(lambda5) < 0.1) {
    dv = all.vars(formula(mod5))[1]
    orig = mod5$model[[dv]]
    shift = abs(min(orig)) + .01
    expect_true(cor(out5$model[[dv]], log(orig + shift)) > 0.99)
  }
  
})



test_that("return_factors_names", {
  expect_equal(return_factors_names(lm(y~a*b*z, data=small)), c("a", "b"))
  expect_equal(return_factors_names(lm(y~z, data=small)), character(0))
})

test_that("identify_method works", {
  expect_equal(identify_method(small, "y", "1"), "loess")
  expect_equal(identify_method(small, "y", "a"), "loess")
  expect_equal(identify_method(small, "y_bin", "x"), "logistic")
  expect_equal(identify_method(small, "y", "x", "asdf"), "asdf")
  expect_equal(identify_method(small, "y", "x", NULL), "loess")  
})
