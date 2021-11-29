context("added.plots")

data(exercise_data)
d = exercise_data
nrow(d)

test_that("added.plot works", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp based on formula",
                              added.plot(weight.loss ~ motivation+therapy.type, data=d))
  vdiffr::expect_doppelganger("avp for motivation",
                              added.plot(weight.loss ~ therapy.type + motivation, data=d))
  vdiffr::expect_doppelganger("multivariate avp",
                              added.plot(weight.loss ~ therapy.type + motivation, 
                                         lm_formula = weight.loss~rewards*gender, data=d))
  vdiffr::expect_doppelganger("multivariate avp 2",
                              added.plot(weight.loss ~ motivation | therapy.type, 
                                         lm_formula = weight.loss~rewards*gender, data=d))
})

test_that("avp plots work with x specified", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp for x=1",
                              added.plot(weight.loss ~ motivation+therapy.type, data=d, x=1))
  vdiffr::expect_doppelganger("avp for x='motivation'",
                              added.plot(weight.loss ~ therapy.type + motivation, data=d, x="therapy.type"))
})

test_that("avp plots work for glms", {
  set.seed(1212)
  #### COMPARE.FITS FUNCTIONS -- linear models
  data("tablesaw.injury")
  model.glm = glm(injury~safety + gender, data=tablesaw.injury, family=binomial)
  vdiffr::expect_doppelganger("avp with glm",
                              added.plot(injury~attention + safety, data=tablesaw.injury, method="logistic"))
  
})



test_that("mediate_plot works", {
  #p = mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data)
  vdiffr::expect_doppelganger("mediate_plot with numeric",
                              mediate_plot(weight.loss~motivation + health, data=exercise_data))
  vdiffr::expect_doppelganger("mediate_plot with categorical",
                              mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data))
  
})
options(warn=0)