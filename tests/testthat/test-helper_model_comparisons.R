context("model.comparison helper works")
nlr = lm(weight.loss~motivation, data=exercise_data)
nlf = lm(weight.loss~motivation + therapy.type, data=exercise_data)
nnl = lm(weight.loss~health + therapy.type, data=exercise_data)
log_full = glm(injury~safety + attention, data=tablesaw.injury, family=binomial)
log_reduced = glm(injury~attention, data=tablesaw.injury, family=binomial)
pois_full = glm(convictions~ses + empathy + depression, data=criminal_data, family=poisson)
pois_reduced = glm(convictions~ses + empathy , data=criminal_data, family=poisson)
gamma_full = glm(aggression~ses + empathy + depression, data=criminal_data, family=Gamma)
gamma_reduced = glm(aggression~ses + empathy , data=criminal_data, family=Gamma)
mixed_full = lme4::lmer(MathAch~SES + Sex + (SES | School), data=math)
mixed_reduced = lme4::lmer(MathAch~ Sex + (1 | School), data=math)
# Small, reproducible Poisson GLMM fixture. The purpose is to exercise the
# glmerMod comparison path, not the convergence of a complex Gamma model.
set.seed(412)
glmm_data = expand.grid(id = factor(seq_len(12)), observation = seq_len(10))
glmm_data$x = rnorm(nrow(glmm_data))
glmm_data$z = rnorm(nrow(glmm_data))
glmm_data$y = rpois(
  nrow(glmm_data),
  lambda = exp(0.5 + 0.5 * glmm_data$x + 0.25 * glmm_data$z +
                 rnorm(12, sd = 0.3)[as.integer(glmm_data$id)])
)
mixed_pois = lme4::glmer(y ~ x + z + (1 | id), data = glmm_data,
                         family = poisson)
mixed_pois_reduced = lme4::glmer(y ~ x + (1 | id), data = glmm_data,
                                 family = poisson)
rf = randomForest::randomForest(weight.loss~therapy.type + motivation, data=exercise_data)
rf2 = randomForest::randomForest(weight.loss~therapy.type , data=exercise_data)
rlm = MASS::rlm(weight.loss~motivation, data=exercise_data)
rlm2 = MASS::rlm(weight.loss~motivation + therapy.type, data=exercise_data)

test_that("model_comparison_table works", {
  # identical models
  expect_true(
    model_comparison_table(nlf, nlf, "A", "B", TRUE)$bayes.factor[1]==1)
  # nested linear models
  mc = model_comparison_table(nlf, nlr, "A", "B", TRUE)
  expect_equal(mc[1,1], 1220.523, tol=.001)
  # linear models non nested
  mc = model_comparison_table(nlf, nnl, "A", "B", FALSE)
  expect_equal(mc[2,1], 1250.756, tol=.001)
  
  # logistic, poisson, gamma, glmer, lmer
  a = model_comparison_table(log_full, log_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 1749.818, tol=.001)
  a = model_comparison_table(pois_full, pois_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 430.5225, tol=.001)
  a = model_comparison_table(gamma_full, gamma_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 1127.837, tol=.001)
  # mixed models
  a = model_comparison_table(mixed_full, mixed_reduced, "A", "B", TRUE)
  
  expected_aic = AIC(
    update(mixed_full, REML = FALSE),
    update(mixed_reduced, REML = FALSE)
  )
  
  expect_equal(a$aic, expected_aic$AIC, tolerance = 1e-5)
  
  # glm mixed 
  a = model_comparison_table(mixed_pois, mixed_pois_reduced, "A", "B", TRUE)
  expect_s3_class(a, "data.frame")
  expect_true(is.finite(as.numeric(a[1, 1])))
  
  # rf
  expect_null(model_comparison_table(rf, rf2, "A", "B", TRUE))
  
  # rlm
  a = model_comparison_table(rlm, rlm2, "A", "B", TRUE)
  expect_equal(a[1,1], 1238.581, tol=.001)
  a = model_comparison_table(rlm, nlf, "A", "B", TRUE)
  expect_equal(a[1,1], 1238.581, tol=.001)
})

test_that("get_p_value works", {
  
  # linear nested
  expect_true(get_p_value(nlf, nlr, T)<.0001)
  
  # glm mixed
  p = get_p_value(mixed_pois, mixed_pois_reduced)
  expect_true(is.numeric(p) && length(p) == 1L &&
                is.finite(p) && p >= 0 && p <= 1)
  
  # logistic glm
  full = glm(died~superpower + speed, data=avengers, family=binomial)
  reduced = glm(died~superpower, data=avengers, family=binomial)
  expect_true(get_p_value(full, reduced)<0.001)
})

test_that("get_r_squared works", {
  # linear models
  r2 = get_r_squared(mixed_full, mixed_reduced)
  
  expect_type(r2, "double")
  expect_named(r2, c("Residual", "(Intercept)"))
  expect_true(all(is.finite(r2)))
  expect_true(all(r2 >= 0 & r2 <= 1))
  expect_lte(sum(r2), 1)
  
})
