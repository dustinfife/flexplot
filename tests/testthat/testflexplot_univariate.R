context("Univariate plots")
set.seed(1212)
data(exercise_data); data("relationship_satisfaction")
d = exercise_data

test_that("histograms vs bar charts", {
  histcont = flexplot(income~1, data=d)
  histcat = flexplot(gender~1, data=d)
  
  vdiffr::expect_doppelganger("histogram", histcont)
  vdiffr::expect_doppelganger("barchart", histcat)
})