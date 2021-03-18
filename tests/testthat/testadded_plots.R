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
  expect_equal(make_avp_formula(y~x+z, y~a_b)[[3]], "y | a_b") 
  expect_equal(make_avp_formula(y~x+z, x=1)[[3]], "y | z")
  expect_equal(make_avp_formula(y~x+z, x=2)[[3]], "y | x") 
})
test_that("check_variables_in_lm works", {
  expect_null(check_variables_in_lm(y~x+x2, y~x))
  expect_error(check_variables_in_lm("a", y~x))
  expect_null(check_variables_in_lm(y~xz, y~x))
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

test_that("return_term_location works", {
  model = lm(weight.loss~therapy.type + motivation + health, data=exercise_data)
  expect_error(return_term_location(model, NULL))
  expect_error(return_term_location(model, "motiv"))
  expect_error(return_term_location(model, c("motiv", "health")))  
  expect_equal(return_term_location(model, "therapy.type"), 2)
  expect_equal(return_term_location(model, c("therapy.type", "health")), c(2,4))
})

test_that("partial_residual works", {
  mod = lm(health~motivation + therapy.type + muscle.gain, data=exercise_data)
  expect_equal(dim(partial_residual(mod, c("motivation", "therapy.type"))), c(200,2)) 
  expect_equal(round(as.numeric(partial_residual(mod, ~motivation)[1])*100), -345)
})

test_that("partial_residual_plot works", {
  mod = lm(health~motivation + weight.loss , data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with one variable",
      partial_residual_plot(health~weight.loss, model=mod, data=exercise_data) + 
      geom_abline(slope = coef(mod)[3]))
  compare.fits(health~weight.loss, data=exercise_data, mod)
  mod = lm(health~motivation + weight.loss + therapy.type , data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with two variables",
      partial_residual_plot(health~weight.loss + therapy.type, model=mod, data=exercise_data)) 
  mod = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with formula term",
      partial_residual_plot(health~ motivation + therapy.type, 
                        model=mod,
                        added_term = ~motivation*therapy.type, data=exercise_data) +
      geom_abline(slope = -.17438))
  
})