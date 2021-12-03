context("visualize function on other models (random forest, rlm, imputed)")
options(warn=-1)
set.seed(1212)


test_that("visualize rf models", {
  
  data(avengers)
  set.seed(1212)
  model = party::cforest(kills~died + ptsd + injuries, data=avengers[1:100,])
  vdiffr::expect_doppelganger("cforest",visualize(model))
  model = randomForest::randomForest(kills~died + ptsd + injuries, data=avengers[1:100,])
  vdiffr::expect_doppelganger("randomForest",visualize(model))                              
  

})
options(warn=0)