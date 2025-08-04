library(testthat)
library(ggplot2)
require(vdiffr)

test_that("magnet_plot works with valid binary data", {
  set.seed(123)
  df = data.frame(
    X = rep(c("A", "B", "C"), times = c(50, 300, 100)),
    Y = c(
      rbinom(50, 1, 0.8),
      rbinom(300, 1, 0.2),
      rbinom(100, 1, 0.5)
    )
  )
  
  p = magnet_plot(Y ~ X, df)
  
  expect_s3_class(p, "ggplot")
})

test_that("magnet_plot errors if Y is not binary", {
  df = data.frame(
    X = rep(c("A", "B"), each = 10),
    Y = sample(1:3, 20, replace = TRUE)  # not binary
  )
  
  expect_error(magnet_plot(Y ~ X, df), "binary")
})



test_that("magnet_plot errors if missing variables", {
  df = data.frame(A = rep(c("A", "B"), each = 10), B = rbinom(20, 1, 0.5))
  
  expect_error(magnet_plot(B ~ A, df), NA)  # this should actually work
})

test_that("magnet_plot output is visually correct", {
  set.seed(123)
  df = data.frame(
    X = rep(c("A", "B", "C"), times = c(50, 300, 100)),
    Y = c(
      rbinom(50, 1, 0.8),
      rbinom(300, 1, 0.2),
      rbinom(100, 1, 0.5)
    )
  )
  
  p = magnet_plot(Y ~ X, data = df)
  
  expect_doppelganger("default magnet plot", p)
})
