test_that("estimates from mixed models", {
  
  mod = lme4::lmer(y~x + (1|id), data=small_mixed)
  estimates_mod = estimates(mod)
  expect_true(all(names(estimates_mod) %in% c("fixed", "r.squared", "rand", "icc")))
  expect_true(estimates_mod$icc[1]<1 & estimates_mod$icc[1]>0)
  
  # with tibbles
  mod2 = update(mod, data=as_tibble(small_mixed))
  expect_true(all(names(estimates(mod2)) %in% c("fixed", "r.squared", "rand", "icc")))
  
  # with missing data in some columns
  mod3 = lme4::lmer(muscle.gain.missing~motivation + (1|health), data=exercise_data)  
  expect_true(all(names(estimates(mod3)) %in% c("fixed", "r.squared", "rand", "icc")))
  
  # generalized mixed model
  mod3 = lme4::glmer(y_gam~x + a + (1|b), data=small, family="Gamma")  
  expect_true(all(names(estimates(mod3)) %in% 
                    c("raw.coefficients", "inverse.coef", "std.mult.coef", "Prediction Difference (+/- 1 SD)")))
})  