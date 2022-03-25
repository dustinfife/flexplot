context("Univariate plots")
set.seed(1212)
data(exercise_data); data("relationship_satisfaction")
d = exercise_data

test_that("histograms vs bar charts", {
  histcont = flexplot(income~1, data=d)
  histcat = flexplot(gender~1, data=d)
  
  vdiffr::expect_doppelganger("histogram", histcont)
  vdiffr::expect_doppelganger("barchart", histcat)
  vdiffr::expect_doppelganger("histogram with bins", flexplot(y~1, data=small, bins=10))
  
  hist_paneled = flexplot(income~1 | gender, data=d)
  vdiffr::expect_doppelganger("paneled histogram", hist_paneled)
  
  hist_paneled = flexplot(income~1 | + gender, data=d)
  vdiffr::expect_doppelganger("paneled histogram but stacked in rows", hist_paneled)
  
  vdiffr::expect_doppelganger("qq plot", 
                              flexplot(speed~1 | +agility, data=avengers, plot.type="qq"))
  vdiffr::expect_doppelganger("density plot", 
                              flexplot(speed~1 | +agility, data=avengers, plot.type="density")) 

})