context("modifying plots")
require(vdiffr)
set.seed(1212)
data(exercise_data)
options(warn=-1)

test_that("modify_points works", {
  p = flexplot(weight.loss~therapy.type, data=exercise_data, alpha = .9)
  #expect_doppelganger("modify_points changes shape", modify_points(p, shape = 15, colour="purple", size=2.5))
})  