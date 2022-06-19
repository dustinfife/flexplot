context("plot_modifiers work")
set.seed(2323)

test_that("modify_points works", {
    p = flexplot(weight.loss~therapy.type, data=exercise_data, alpha = .9)
    expect_doppelganger("modify_points changes shape", modify_points(p, shape = 15, colour="purple", size=2.5))
})


test_that("modify_labels works", {
  p = flexplot(weight.loss~motivation + therapy.type | rewards + gender, data=exercise_data)
  expect_doppelganger("modify_labels works", modify_labels(p, x="Motivation", y="Weight Loss", 
                                                            colors = "Therapy Type", 
                                                            panel_cols = "Rewards",
                                                            panel_rows = "Gender"))
})                       

test_that("modify_smooth works", {
  p = flexplot(weight.loss~motivation  | rewards + gender, data=exercise_data)
  expect_doppelganger("modify_smooth works", modify_smooth(p, se=T, linetype="dashed", method="quadratic",
                                                           size=3, color="green")
                                                           )
})

test_that("identify smoother works", {
  expect_null(identify_smoother("suppress"))
  expect_equal(identify_smoother("logistic"), geom_smooth(method = "glm", method.args = list(family = "binomial"), formula = y~x))
  expect_equal(identify_smoother("rlm"), geom_smooth(method = "rlm",  formula = y~x))
  expect_equal(identify_smoother("poisson"), geom_smooth(method = "glm", method.args = list(family = "poisson"), formula = y~x))
  expect_equal(identify_smoother("polynomial"), stat_smooth(method="lm",formula=y ~ poly(x, 2, raw=TRUE)))
  expect_equal(identify_smoother("cubic"), stat_smooth(method="lm",formula=y ~ poly(x, 3, raw=TRUE)))  
  expect_equal(identify_smoother("lm"), stat_smooth(method="lm", formula = y~x))
  expect_equal(identify_smoother("lesdsdfs"), geom_smooth(method="loess", formula = y~x))
})
  