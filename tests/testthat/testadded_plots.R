context("added.plots")

data(exercise_data)
d = exercise_data

test_that("avp plots work for linear models", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp for therapy.type",
                              added.plot(weight.loss ~ motivation+therapy.type, data=d))
  vdiffr::expect_doppelganger("avp for motivation",
                              added.plot(weight.loss ~ therapy.type + motivation, data=d))
  avp(weight.loss ~ therapy.type + motivation, data=d)
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


test_that("find_variable_of_interest works", {
  expect_equal(find_variable_of_interest(letters[1:3], NULL), "c")
  expect_error(find_variable_of_interest(letters[1:3], 4))
  expect_error(find_variable_of_interest(letters[1:3], "d"))
  expect_equal(find_variable_of_interest(letters[1:3], 2), "b")
  expect_equal(find_variable_of_interest(letters[1:3], "b"), "b")
})

test_that("prep_data_for_avp works", {
  expect_false(tibble::is_tibble(prep_data_for_avp(tibble::tibble(data.frame(x=1:3, y=1:3)), c("x", "y"))))
  expect_true(nrow(prep_data_for_avp(data.frame(x=c(1,2,NA), y=c(4,5,6)), c("x", "y")))==2)
  expect_true(nrow(prep_data_for_avp(data.frame(x=c(1,2,NA), y=c(4,5,6), z=5:7), c("y", "z")))==3)
})