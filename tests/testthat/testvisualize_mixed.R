context("visualize function on mixed models)")
options(warn=-1)


set.seed(1212)
test_that("visualize mixed models", {
  #### mixed models
  data(math)
  math = math[1:1000,]
  model = lme4::lmer(MathAch~ SES + Sex + (SES|School), data=math)
  set.seed(1212)
  a = visualize(model, formula = MathAch~ SES | Sex + School, plot="model")
  b = visualize(model, formula = MathAch~ SES + School| Sex)
  c = visualize(model, formula = MathAch~ Sex | SES+ School, plot="model")
  
  mod = lme4::lmer(MathAch~1 + (1|School), data=math)
  a1 = visualize(mod, plot="model")
  mod1 = lme4::lmer(MathAch~SES + (SES|School), data=math)
  a2 = visualize(mod1, plot="model")
  a3 = visualize(model, plot = "model",
                 formula = MathAch ~  Sex + School| SES, 
                 sample = 30)
  vdiffr::expect_doppelganger("mixed row panels",a)
  vdiffr::expect_doppelganger("mixed diff lines",b)
  vdiffr::expect_doppelganger("mixed small sample",c)
  vdiffr::expect_doppelganger("mixed anova with no formula",a1)
  vdiffr::expect_doppelganger("mixed no formula one covariate", a2)
  vdiffr::expect_doppelganger("mixed old error", a3)
            
})

test_that("visualize mixed models with alcuse", {
  #### mixed models
  data(alcuse)
  mod1 = lme4::lmer(ALCUSE~1 + (1|ID), data=alcuse)
  #a5 = visualize(mod1, plot="model")  
  mod2 = lme4::lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)
  a4 = visualize(mod2, plot="model")  
  
  vdiffr::expect_doppelganger("mixed when x axis has <5 levels", a4)
  #vdiffr::expect_doppelganger("random effects anova", a5)
  
})

test_that("visualize based on issue 105 with scaled variables", {
  exercise_data <- exercise_data %>% 
    mutate(hi_weight_loss_category = case_when(weight.loss > 5 ~ 1,TRUE ~ 0))
  exercise_data$hi_weight_loss_category <-as.factor(exercise_data$hi_weight_loss_category)
  
  exercise_data_scaled <- exercise_data %>% 
    mutate(hi_weight_loss_category_c = as.character(hi_weight_loss_category)) %>% 
    mutate_if(is.numeric, function(x) {
      ## !!! error avoided by using
      ## as.numeric(scale(x, center = TRUE, scale = TRUE))}) %>%** 
      scale(x, center = TRUE, scale = TRUE)}) %>% 
    mutate(hi_weight_loss_category = 
             as.factor(as.numeric(hi_weight_loss_category_c))) %>% 
    dplyr::select(-hi_weight_loss_category_c)
  
  
  
  require(lme4)
  glm1 <- glmer(
    formula = hi_weight_loss_category ~ 
      health + gender + (1|therapy.type),
    family = binomial,
    data = exercise_data_scaled
  )
  
  vdiffr::expect_doppelganger("visualize scaled variable with mixed model", 
                              flexplot::visualize(glm1, plot = "residuals"))
})
options(warn=0)