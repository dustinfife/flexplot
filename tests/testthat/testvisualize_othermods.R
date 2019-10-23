context("visualize function on other models (random forest, mixed, rlm, imputed)")
set.seed(1212)
test_that("visualize mixed models", {
  #### mixed models
  data(math)
  model = lme4::lmer(MathAch~ SES + Sex + (SES|School), data=math)
  vdiffr::expect_doppelganger("mixed row panels",visualize(model, formula = MathAch~ SES | Sex + School, plot="model"))
  vdiffr::expect_doppelganger("mixed diff lines",visualize(model, formula = MathAch~ SES + School| Sex, sample=100))
  vdiffr::expect_doppelganger("mixed small sample",visualize(model, formula = MathAch~ Sex | SES+ School, sample=3))
})