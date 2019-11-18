context("added.plots")

data(exercise_data)
d = exercise_data

test_that("avp plots work for linear models", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp for therapy.type",
                              added.plot(weight.loss ~ motivation+therapy.type, data=d))
  vdiffr::expect_doppelganger("avp for motivation",
                              added.plot(weight.loss ~ therapy.type + motivation, data=d))
})

test_that("avp plots work for glms", {

  #### COMPARE.FITS FUNCTIONS -- linear models
  data("tablesaw.injury")
  model.glm = glm(injury~safety + gender, data=tablesaw.injury, family=binomial)
  vdiffr::expect_doppelganger("avp with glm",
                              added.plot(injury~attention + safety, data=tablesaw.injury, method="logistic"))

})
