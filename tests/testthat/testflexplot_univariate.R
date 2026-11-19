context("Univariate plots")
set.seed(1212)
data(exercise_data); data("relationship_satisfaction")
d = exercise_data

test_that("histograms vs bar charts", {
  histcont = flexplot(income~1, data=d)
  histcat = flexplot(gender~1, data=d)
  
  expect_flexplot_snapshot("testflexplot_univariate/histogram.svg", "histogram", histcont)
  expect_flexplot_snapshot("testflexplot_univariate/barchart.svg", "barchart", histcat)
  expect_flexplot_snapshot("testflexplot_univariate/histogram-with-bins.svg", "histogram with bins", flexplot(y~1, data=small, bins=10))
  
  hist_paneled = flexplot(income~1 | gender, data=d)
  expect_flexplot_snapshot("testflexplot_univariate/paneled-histogram.svg", "paneled histogram", hist_paneled)
  
  hist_paneled = flexplot(income~1 | gender, data=d)
  expect_flexplot_snapshot("testflexplot_univariate/paneled-histogram-but-stacked-in-rows.svg", "paneled histogram but stacked in rows", hist_paneled)
  
  expect_flexplot_snapshot("testflexplot_univariate/qq-plot.svg", "qq plot",
                              flexplot(speed~1 | agility, data=avengers, plot.type="qq"))
  expect_flexplot_snapshot("testflexplot_univariate/density-plot.svg", "density plot",
                              flexplot(speed~1 | agility, data=avengers, plot.type="density")) 

})