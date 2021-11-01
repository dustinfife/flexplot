context("binning functions work")

test_that("bin.me works", {
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, bins=3))
  expect_output(print(res), "46.7-58")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = c(20, 60)))
  expect_output(print(res), "20-60")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = c(20, 60), check.breaks = F))
  expect_output(print(res), "20-60")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, labels = c("a", "b", "c")))  
  expect_output(print(res), "b")
  res = bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = list(c(20, 60, 80)), return.breaks=TRUE)
  expect_output(print(res), "-7  20  60  80 117")  
  expect_equal(levels(bin.me("weight.loss", exercise_data, bins=3, breaks = list(-5, 0, 5)))[1], "(-9)-(-5)")
  expect_equal(levels(bin.me("weight.loss", exercise_data, bins=3, labels = list("low", "mid", "high")))[1], "low")
  expect_true(length(bin.me("weight.loss", exercise_data, bins=3, return.breaks = T))==4)
})

test_that("choose_bins works", {
  expect_equal(choose_bins(NULL, NULL), 3)
  expect_equal(choose_bins(NULL, 1:3), 4)
  expect_equal(choose_bins(1:3, 1:3), 3)
})

test_that("prep.breaks works", {
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=NULL, bins=3)), "46.66667")
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=c(20, 60))), "-7  20  60 117")
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=NULL, bins=NULL)), "46.66667")
})

