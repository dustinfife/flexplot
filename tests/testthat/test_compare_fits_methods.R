context("test compare_fits across all models")


test_that("get_fitted works with various model types", {
  get_fitted(model = lm(y~x + a, data=small), pred.values = small)
  get_fitted(model = glm(y_bin~x + a + I(x^2), data=small), pred.values = small)
  get_fitted(model = lme4::lmer(y~x +a + (x | id), data=small_mixed), pred.values = small_mixed)
  get_fitted(model = lme4::glmer(a~x +y + (y | id), data=small_mixed, family=binomial), pred.values = small_mixed) %>% suppressWarnings()
  get_fitted(model = MASS::polr(weight.loss~rewards,
                                data=exercise_data %>% mutate(weight.loss = factor(weight.loss, ordered=T))), 
                                pred.values = exercise_data)
  model1 = randomForest::randomForest(wl~motivation + gender + rewards, 
                                      data=exercise_data %>% mutate(wl = weight.loss + .8*motivation*as.numeric(rewards)))
  get_fitted(model = model1, pred.values = exercise_data)
})

test_that("compare_fits works", {
  model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
  model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
  model.int2 = lm(weight.loss ~ motivation + therapy.type + motivation:therapy.type, data = exercise_data)
  model.poly = lm(weight.loss ~ motivation + therapy.type + I(motivation^2), data = exercise_data)
  
  compare_fits(weight.loss~motivation | therapy.type, data=exercise_data, model.me)
  compare_fits(weight.loss~motivation | therapy.type, data=exercise_data, model.int)
  compare_fits(weight.loss~motivation | therapy.type, data=exercise_data, model.int2)
  compare_fits(weight.loss~motivation | therapy.type, data=exercise_data, model.poly)
  
  compare_fits(y~x + a, data=small, lm(y~x + a, data=small))
  mod = glm(died~agility + speed, data=avengers, family=binomial)
  compare_fits(died~agility | speed, data=avengers, mod)
  
  full.mod = lm(satisfaction~communication * separated , data=relationship_satisfaction)
  reduced.mod = lm(satisfaction~communication + separated , data=relationship_satisfaction)
  compare_fits(satisfaction~communication|separated, data=relationship_satisfaction, full.mod, reduced.mod)

  
}) 

test_that("compare.fits with rlm and polr", {
  mod =         lm(weight.loss~rewards, data=exercise_data)
  mod2 = MASS::rlm(weight.loss~rewards, data=exercise_data)
  compare_fits(formula=weight.loss~rewards, data=exercise_data, model1=mod, model2=mod2)
  
  exercise_data_polr = exercise_data %>% mutate(weight.loss = factor(weight.loss, ordered=T))
  polyn =  MASS::polr(weight.loss~rewards,              data=exercise_data_polr)
  polyn2 = MASS::polr(weight.loss~rewards+therapy.type, data=exercise_data_polr)
  compare_fits(weight.loss~rewards|therapy.type, exercise_data_polr, polyn, polyn2)
  
})

test_that("compare.fits with randomForest",{
  exercise_data_wl = exercise_data
  exercise_data_wl$wl = exercise_data_wl$weight.loss + .8*exercise_data_wl$motivation*as.numeric(exercise_data_wl$rewards)
  model1 = randomForest::randomForest(wl~motivation + gender + rewards, data=exercise_data_wl)
  model2 = lm(wl~motivation * gender * rewards, data=exercise_data_wl)
  ### use the same predictors in both models
  exercise_data_wl = exercise_data
  exercise_data_wl$wl = exercise_data_wl$weight.loss + .8*exercise_data_wl$motivation*as.numeric(exercise_data_wl$rewards)
  compare_fits(wl~motivation | gender + rewards, exercise_data_wl, model1, model2)
})

test_that("compare_fits with glms", {
  ##### predictions with generalize lidnear model
  exercise_gamma = exercise_data
  exercise_gamma$weight.loss = exercise_gamma$weight.loss + 1 + abs(min(exercise_gamma$weight.loss, na.rm=T))
  mod1 = glm(weight.loss~motivation + health + gender, data=exercise_gamma, family="Gamma")
  compare_fits(weight.loss~motivation | health + gender, data=exercise_gamma, mod1)
  
  data(authors); d= authors[1:1000,]
  mod1 = lm(Daily.Units.Sold~Sale.Price*Publisher, data=d)
  mod2 = glm(Daily.Units.Sold~Sale.Price*Publisher, data=d, family=quasipoisson(link="log"))
  compare_fits(Daily.Units.Sold~Sale.Price|Publisher, data=d, mod1, mod2) %>% suppressWarnings()
  
  #### compare.fits with two glms
  mod1 = glm(injury~safety + attention, data=tablesaw.injury, family=binomial)  
  mod2 = glm(injury~safety * attention, data=tablesaw.injury, family=binomial)  
  compare_fits(injury~safety | attention, data=tablesaw.injury, mod1, mod2)
})

### compare.fits with mixed models
test_that("compare_fits with lmer", {
  require(lme4)
  mod1 = lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)  
  mod2 = lmer(ALCUSE~AGE_14 + (AGE_14|ID), data=alcuse)  
  compare_fits(ALCUSE~AGE_14 | ID, data=alcuse, mod1, mod2, re=T)
  
})

options(warn=0)