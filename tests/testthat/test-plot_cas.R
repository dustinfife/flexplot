test_that(".get_re_term works", {
  expect_equal(.get_re_term(lme4::lmer(y~x + (x | id), data=small_mixed)), "id")
})

mod1 = lme4::lmer(y~x + (x | id), data=small_mixed)
mod2 = lme4::lmer(y~1 + (1 | id), data=small_mixed)

test_that("compute_cas_from_model works", {
  results = compute_cas_from_model(mod1)
  expect_true(all(c("y_adj", "pred_fixed", ".re_term") %in% names(results)))
  
  results = compute_cas_from_model(mod2)
  expect_true(sum(duplicated(results$pred_fixed)) + 1 == nrow(small_mixed))
})

test_that("cas_adjustment works", {
  
  # Pull the three kinds of predictions your function uses
  pred_fixed = predict(mod1, re.form = NA)           # fixed-only
  pred_cond  = predict(mod1, re.form = NULL)         # conditional (incl. REs)
  pred_int   = predict(mod1, re.form = ~(1 | id))   # random-intercept-only
  
  # --- intercept adjustment ---
  y_intercept_expected = small_mixed$y - (pred_int - pred_fixed)
  y_intercept_actual   = cas_adjustment(small_mixed, model = mod1, adjust = "intercept")
  
  expect_equal(y_intercept_actual, y_intercept_expected, tolerance = 1e-10, ignore_attr = TRUE)
  
  # --- all adjustment ---
  y_all_expected = (small_mixed$y - pred_cond) + pred_fixed
  y_all_actual   = cas_adjustment(small_mixed, model = mod1, adjust = "all")

  
  expect_equal(y_all_actual, y_all_expected,tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(cas_adjustment(small_mixed, mod1, adjust="none"), small_mixed$y)
})

test_that("cluser_adjusted_scatter works", {
  expect_error(cluster_adjusted_scatter(y~x+a, mod1))
  vdiffr::expect_doppelganger("cas normal", cluster_adjusted_scatter(y~x, object = mod1))
  vdiffr::expect_doppelganger("cas intercept", cluster_adjusted_scatter(y~x, object = mod1, adjust = "intercept"))
  vdiffr::expect_doppelganger("cas none", cluster_adjusted_scatter(y~x, object = mod1, adjust = "none"))
})