context("use compare.fits to visualize linear models")

data(exercise_data)
d = exercise_data
d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
options(warn=-1)
test_that("compare.fits linear models", {
  set.seed(1212)
  model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
  model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
  model.int2 = lm(weight.loss ~ motivation + therapy.type + motivation:therapy.type, data = exercise_data)
  model.poly = lm(weight.loss ~ motivation + therapy.type + I(motivation^2), data = exercise_data)
  suppressWarnings(vdiffr::expect_doppelganger("compare interaction vs. me",
            compare.fits(weight.loss ~ motivation | therapy.type, 
               data = exercise_data, model.me, model.int, ghost.line = "black", num_points=10)) %>% suppressMessages())
  expect_error(compare.fits(weight.loss ~ mottion+therapy.type, data=exercise_data, model.me, model.int))
  expect_error(compare.fits(weight.loss ~ mottion+therapy.type, data=relationship_satisfaction, model.me, model.int))
  expect_equal(compare.fits(weight.loss ~ motivation | therapy.type, 
               data = exercise_data, model.me, model.int2, return.preds = T)[1,3], 6.615958, tolerance = .001)
  

  ### compare interaction and non-interaction models
  mod = lm(wl ~motivation+rewards, data=d)
  mod2 = lm(wl ~motivation*rewards, data=d)
  vdiffr::expect_doppelganger("compare.fits with strong interaction",
                              suppressWarnings(compare.fits(wl~motivation|rewards, data=d, model1=mod, model2=mod2)))
  
  mod = lm(weight.loss~motivation, data=d)
  vdiffr::expect_doppelganger("compare.fits with one model",compare.fits(weight.loss~motivation, data=d, model1=mod))
  
  mod1 = lm(weight.loss~therapy.type * motivation * health * muscle.gain * I(motivation^2), data=d)
  mod2 = lm(weight.loss~therapy.type + motivation + health + muscle.gain + I(motivation^2), data=d)
  vdiffr::expect_doppelganger("compare.fits with many vars and polynomial",
                              compare.fits(weight.loss ~muscle.gain | motivation + health, data=d, model1=mod1, model2=mod2)%>%suppressMessages())
  vdiffr::expect_doppelganger("compare.fits with many vars and polynomial v2",
                              compare.fits(weight.loss ~muscle.gain +therapy.type | motivation + health, data=d, model1=mod1))
  data("relationship_satisfaction")
  full.mod = lm(satisfaction~communication * separated , data=relationship_satisfaction)
  reduced.mod = lm(satisfaction~communication + separated , data=relationship_satisfaction)
  vdiffr::expect_doppelganger("compare.fits where listwise deletion causes change in levels",
                              compare.fits(satisfaction~communication|separated, data=relationship_satisfaction, full.mod, reduced.mod))

  #expect_error(compare.fits(a~b, data=small, model1=glm(a~b, data=small, family=binomial)))
})


test_that("compare.fits for other models", {
  set.seed(1212)
  #### COMPARE.FITS FUNCTIONS -- linear models
  mod = lm(weight.loss~rewards, data=d)
  mod2 = MASS::rlm(weight.loss~rewards, data=d)
  vdiffr::expect_doppelganger("compare.fits with rlm",compare.fits(formula=weight.loss~rewards, data=d, model1=mod, model2=mod2))
  
  polyn = MASS::polr(weight.loss~rewards,               data=exercise_data %>% mutate(weight.loss = factor(weight.loss, ordered=T)))
  polyn2 = MASS::polr(weight.loss~rewards+therapy.type, data=exercise_data %>% mutate(weight.loss = factor(weight.loss, ordered=T)))
  vdiffr::expect_doppelganger("compare.fits with polr",
                  compare.fits(weight.loss~rewards|therapy.type, 
                                                        data=exercise_data %>% mutate(weight.loss = factor(weight.loss, ordered=T)), 
                               polyn, polyn2))
  
  
  ##### compare predictions with random forest
  d = exercise_data
  d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
  model1 = randomForest::randomForest(wl~motivation + gender + rewards, 
                                      data=exercise_data %>% mutate(wl = weight.loss + .8*motivation*as.numeric(rewards)))
  model2 = lm(wl~motivation * gender * rewards, 
                                      data=exercise_data %>% mutate(wl = weight.loss + .8*motivation*as.numeric(rewards)))
  ### use the same predictors in both models
  vdiffr::expect_doppelganger("compare.fits with rf",compare.fits(wl~motivation | gender + rewards, 
                                     data=exercise_data %>% mutate(wl = weight.loss + .8*motivation*as.numeric(rewards)), 
                              model1, model2))
  
  ##### predictions with generalize lidnear model
  d$weight.loss = d$weight.loss + 1 + abs(min(d$weight.loss, na.rm=T))
  mod1 = glm(weight.loss~motivation + health + gender, data=d, family="Gamma")
  mod2 = lm(weight.loss~motivation + health + gender, data=d)
  vdiffr::expect_doppelganger("compare.fits with glm",
                              compare.fits(weight.loss~motivation | health + gender, data=d, mod1, mod2))
  
  data(authors); d= authors[1:1000,]
  mod1 = lm(Daily.Units.Sold~Sale.Price*Publisher, data=d)
  mod2 = glm(Daily.Units.Sold~Sale.Price*Publisher, data=d, family=quasipoisson(link="log"))
  vdiffr::expect_doppelganger("compare.fits with loglink",
                              compare.fits(Daily.Units.Sold~Sale.Price|Publisher, data=d, mod1, mod2))
  
  #### compare.fits with two glms
  data("tablesaw.injury")
  mod1 = glm(injury~safety + attention, data=tablesaw.injury, family=binomial)  
  mod2 = glm(injury~safety * attention, data=tablesaw.injury, family=binomial)  
  vdiffr::expect_doppelganger("compare.fits with two glms",
                              compare.fits(injury~safety | attention, data=tablesaw.injury, mod1, mod2))
  
  ### compare.fits with mixed models
  data(alcuse)
  require(lme4)
  mod1 = lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)  
  mod2 = lmer(ALCUSE~AGE_14 + (AGE_14|ID), data=alcuse)  
  vdiffr::expect_doppelganger("compare.fits with mixed models",
                              compare.fits(ALCUSE~AGE_14 | ID, data=alcuse, mod1, mod2))
  vdiffr::expect_doppelganger("compare.fits with mixed models and RE = T",
                              compare.fits(ALCUSE~AGE_14 | ID, data=alcuse, mod1, mod2, re=T))  
  
  # compare.fits with tibbles
  d = as_tibble(alcuse)
  mod1 = lmer(ALCUSE~AGE_14 + (1|ID), data=d)  
  mod2 = lmer(ALCUSE~AGE_14 + (AGE_14|ID), data=d)  
  vdiffr::expect_doppelganger("compare.fits with tibbles",
                              compare.fits(ALCUSE~AGE_14 | ID, data=d, mod1, mod2))
  
  ## compare.fits returned error when data had integers
  d = round(data.frame(y=rnorm(100, 50, 20), x=rnorm(100, 50, 20), z=rnorm(100, 50, 20)))
  class(d$y) = "integer"; class(d$x) = "integer"; class(d$z) = "integer"; 
  mod = suppressWarnings(party::cforest(y~., data=d))
  testthat::expect_true(names(compare.fits(y~x+z, d, mod, return.preds=TRUE))[2]=="z")
  
  ### compare.fits with rpart
  fit = rpart::rpart(weight.loss~motivation + therapy.type, data=exercise_data, method="anova")
  vdiffr::expect_doppelganger("compare.fits with rpart numeric",
                              compare.fits(weight.loss~motivation + therapy.type, data=exercise_data, fit))
  fit2 = rpart::rpart(therapy.type~motivation + rewards, data=exercise_data, method="anova")
  vdiffr::expect_doppelganger("compare.fits with rpart categorical",
                              compare.fits(therapy.type~motivation + rewards, data=exercise_data, fit2))  
  
  ## compare.fits with labels/breaks (used to throw an error prior to may 2022)
  mod = lm(y~a + x, data=small)
  vdiffr::expect_doppelganger("compare.fits with labels/breaks", 
    compare.fits(y~a |  x, data=small, 
               model1=mod,
               breaks = list(x=c(-2, 0, Inf)), 
               labels = list(x=c("small", "large"))))
})



options(warn=0)