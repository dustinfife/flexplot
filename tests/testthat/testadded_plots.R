context("added.plots")

data(exercise_data)
d = exercise_data

test_that("avp plots work for linear models", {
  set.seed(1212)
  vdiffr::expect_doppelganger("avp for therapy.type",
                              added.plot(weight.loss ~ motivation+therapy.type, data=d))
  vdiffr::expect_doppelganger("avp for motivation",
                              added.plot(weight.loss ~ therapy.type + motivation, data=d))
  vdiffr::expect_doppelganger("multivariate avp",
                              added.plot(weight.loss ~ therapy.type + motivation, 
                                         lm_formula = weight.loss~rewards*gender, data=d))
  vdiffr::expect_doppelganger("multivariate avp 2",
                              added.plot(weight.loss ~ motivation | therapy.type, 
                                         lm_formula = weight.loss~rewards*gender, data=d))
  head(exercise_data)
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

test_that("label_avp_axis works", {
  expect_equal(label_avp_axis(y~x+z), "y | x + z")
})

test_that("make_avp_formula works", {
  expect_equal(make_avp_formula(y~x+z, y~a_b)[[3]], "y | x") 
  expect_equal(make_avp_formula(y~x+z, x=1)[[3]], "y | z")
  expect_equal(make_avp_formula(y~x+z, x=2)[[3]], "y | x") 
})
test_that("check_variables_in_lm works", {
  expect_null(check_variables_in_lm(y~x+x2, y~x))
  expect_error(check_variables_in_lm("a", y~x))
  expect_error(check_variables_in_lm(y~xz, y~x))
  expect_error(check_variables_in_lm(x~y + z, y~x))
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