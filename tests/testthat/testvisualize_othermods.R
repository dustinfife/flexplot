context("visualize function on other models (random forest, mixed, rlm, imputed)")
set.seed(1212)
test_that("visualize mixed models", {
  #### mixed models
  data(math)
  math = math[1:100,]
  model = lme4::lmer(MathAch~ SES + Sex + (SES|School), data=math)
  set.seed(1212)
  vdiffr::expect_doppelganger("mixed row panels",visualize(model, formula = MathAch~ SES | Sex + School, plot="model"))
  vdiffr::expect_doppelganger("mixed diff lines",visualize(model, formula = MathAch~ SES + School| Sex, sample=11))
  vdiffr::expect_doppelganger("mixed small sample",visualize(model, formula = MathAch~ Sex | SES+ School, sample=10, plot="model"))
  
  mod = lme4::lmer(MathAch~1 + (1|School), data=math)
  vdiffr::expect_doppelganger("mixed anova with no formula",visualize(mod, plot="model"))
  
  mod1 = lme4::lmer(MathAch~SES + (SES|School), data=math)
  vdiffr::expect_doppelganger("mixed no formula one covariate", visualize(mod1, plot="model"))
  
  vdiffr::expect_doppelganger("mixed old error", 
                              visualize(model, plot = "model",
                                        formula = MathAch ~  Sex + School| SES, 
                                        sample = 30))
  
  mod = lme4::lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)
  vdiffr::expect_doppelganger("mixed when x axis has <5 levels", 
                              visualize(mod, plot="model"))
  
  mod = lme4::lmer(ALCUSE~1 + (1|ID), data=alcuse)
  vdiffr::expect_doppelganger("random effects anova", 
                              visualize(mod, plot="model"))
            
})

test_that("visualize rf models", {
  
  data(avengers)
  set.seed(1212)
  model = party::cforest(kills~died + ptsd + injuries, data=avengers[1:100,])
  vdiffr::expect_doppelganger("cforest",visualize(model))
  model = randomForest::randomForest(kills~died + ptsd + injuries, data=avengers[1:100,])
  vdiffr::expect_doppelganger("randomForest",visualize(model))                              
  

})