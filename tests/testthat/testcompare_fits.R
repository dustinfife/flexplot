context("use compare.fits to visualize linear models")

data(exercise_data)
d = exercise_data
d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
set.seed(1212)
test_that("compare.fits linear models", {
  model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
  model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
  vdiffr::expect_doppelganger("compare interaction vs. me",compare.fits(weight.loss ~ motivation | therapy.type, 
               data = exercise_data, model.me, model.int, ghost.line = "black"))            

  ### compare interaction and non-interaction models
  mod = lm(wl ~motivation+rewards, data=d)
  mod2 = lm(wl ~motivation*rewards, data=d)
  vdiffr::expect_doppelganger("compare.fits with strong interaction",
                              compare.fits(wl~motivation|rewards, data=d, model1=mod, model2=mod2))
  
  mod = lm(weight.loss~motivation, data=d)
  vdiffr::expect_doppelganger("compare.fits with one model",compare.fits(weight.loss~motivation, data=d, model1=mod))
  
  mod1 = lm(weight.loss~therapy.type * motivation * health * muscle.gain * I(motivation^2), data=d)
  mod2 = lm(weight.loss~therapy.type + motivation + health + muscle.gain + I(motivation^2), data=d)
  vdiffr::expect_doppelganger("compare.fits with many vars and polynomial",
                              compare.fits(weight.loss ~muscle.gain | motivation + health, data=d, model1=mod1, model2=mod2))
  vdiffr::expect_doppelganger("compare.fits with many vars and polynomial v2",
                              compare.fits(weight.loss ~muscle.gain +therapy.type | motivation + health, data=d, model1=mod1))
})

test_that("compare.fits for other models", {

  #### COMPARE.FITS FUNCTIONS -- linear models
  mod = lm(weight.loss~rewards, data=d)
  require(MASS)
  mod2 = rlm(weight.loss~rewards, data=d)
  vdiffr::expect_doppelganger("compare.fits with rlm",compare.fits(formula=weight.loss~rewards, data=d, model1=mod, model2=mod2))

  
  ##### compare predictions with random forest
  require(randomForest)
  model1 = randomForest(wl~motivation + gender + rewards, data=d)
  model2 = lm(wl~motivation * gender * rewards, data=d)		### use the same predictors in both models
  vdiffr::expect_doppelganger("compare.fits with rf",compare.fits(wl~motivation | gender + rewards, data=d, model1, model2))
  
  ##### predictions with generalize lidnear model
  d$weight.loss = d$weight.loss + 1 + abs(min(d$weight.loss, na.rm=T))
  mod1 = glm(weight.loss~motivation + health + gender, data=d, family="Gamma")
  mod2 = lm(weight.loss~motivation + health + gender, data=d)
  vdiffr::expect_doppelganger("compare.fits with glm",
                              compare.fits(weight.loss~motivation | health + gender, data=d, mod1, mod2))
  require(fifer)
  data(authors); d= authors[1:1000,]
  mod1 = lm(Daily.Units.Sold~Sale.Price*Publisher, data=d)
  mod2 = glm(Daily.Units.Sold~Sale.Price*Publisher, data=d, family=quasipoisson(link="log"))
  vdiffr::expect_doppelganger("compare.fits with loglink",
                              compare.fits(Daily.Units.Sold~Sale.Price|Publisher, data=d, mod1, mod2))

})
  

