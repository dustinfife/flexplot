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
})

test_that("bin_variables_loop works", {
  # this function bins a specific variable
  fake_labels = c("low", "mid", "high")
  d= data.frame(a = sample(c(1,5), size=8, replace=T), b=1:8, c = rnorm(8))
  bin_variables_loop(1, d, c("a", "b"), 3, 
     labels = list(a = fake_labels[1:2], b = fake_labels),
     breaks = list(c(1,5,8), c(-1, 0, 1)))                %>%
              levels          %>% 
              purrr::pluck(2) %>%
              expect_equal("mid")
  bin_variables_loop(2, d, c("a", "b"), 3, 
                     labels=NULL,
                     breaks = list(c(1,5,8), c(-1, 0, 1))) %>%
              levels          %>% 
              purrr::pluck(2) %>%
              expect_equal("0-1")
  expect_equal(levels(bin_variables_loop(1, d, "a", 3, labels = NULL, breaks = c(1,5,8)))[2], "5")
})

