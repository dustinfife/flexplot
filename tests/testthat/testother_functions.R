context("miscellaneous functions")

data("exercise_data")

test_that("standardized.beta returns the correct value", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = standardized.beta(model1)
  names(results) = NULL
  expect_equal(results[2], 0.3708347) 
})

test_that("bic inverts", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model2 = lm(weight.loss~motivation * therapy.type, data=exercise_data)
  expect_equal(bf.bic(model1, model2), 95.29443) 
})

test_that("subsetString returns correct values", {
  stringthing = ("I - am - a - string")
  subsetString(stringthing, sep = "*", position = 2, flexible=T)
  expect_output(print(subsetString(stringthing, sep = "-", position = 2)), " am ")
  expect_output(print(subsetString(stringthing, sep = "*", position = 4, flexible=T)), " am ")
  expect_true(is.na(subsetString(stringthing, sep = "*", position = 4, flexible=F)))
})


test_that("make.formula works", {
  make.formula("a", letters[2:5])
  expect_output(print(make.formula("a", letters[2:5])), "e")
  expect_output(print(make.formula("a", letters[2:5], random = "(1|School)")), "|")
})

test_that("calculate_bins_for_histograms works", {
  expect_equal(calculate_bins_for_histograms(3, levels=10), 5)
  expect_equal(calculate_bins_for_histograms(4, levels=10), 4)
  expect_equal(calculate_bins_for_histograms(3, levels=33), 16)
})