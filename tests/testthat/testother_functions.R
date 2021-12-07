context("miscellaneous functions")

data("exercise_data")



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
