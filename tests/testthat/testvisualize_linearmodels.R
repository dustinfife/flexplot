context("visualize function on linear models")
set.seed(1212)
data(exercise_data)
d = exercise_data


 
test_that("visualize function plots", {
  
  #### VISUALIZE FUNCTIONS -- Linear Models
  ## t-test
  mod = lm(weight.loss~rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("ttest",visualize(mod)))
  
  ### regression
  mod = lm(weight.loss~motivation, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("regression",visualize(mod)))
  suppressWarnings(vdiffr::expect_doppelganger("regression residuals only",visualize(mod, plot="residuals")))
  suppressWarnings(vdiffr::expect_doppelganger("regression model",visualize(mod, plot="model")))

  ### ancova
  mod = lm(weight.loss~motivation + rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("ancova",visualize(mod)))
  suppressWarnings(vdiffr::expect_doppelganger("ancova resids",visualize(mod, plot="residuals")))
  suppressWarnings(vdiffr::expect_doppelganger("ancova model",visualize(mod, plot="model")))
  
  ### factorial anova
  mod = lm(weight.loss~gender + rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("factorial anova",visualize(mod)))

  
  ### multiple regression
  mod = lm(weight.loss~gender + rewards + motivation, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("multiple regression",visualize(mod)))
  
  data(birthweight)
  mod = lm(Birthweight~mheight + fheight + motherage, data=birthweight)
  suppressWarnings(vdiffr::expect_doppelganger("four variables",visualize(mod)))

})

test_that("added.plot function", {
  suppressWarnings(vdiffr::expect_doppelganger("avp",added.plot(Birthweight~mheight + fheight + motherage + smoker, data=birthweight, method="lm")))
})