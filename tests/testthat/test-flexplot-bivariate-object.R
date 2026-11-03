test_that("numeric bivariate renderer returns ggplot layers", {
  p = flexplot_bivariate_object(mtcars, "mpg", "wt", method="lm")
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$geom, "GeomSmooth")
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), nrow(mtcars))
})

test_that("smoothing can be suppressed", {
  p = flexplot_bivariate_object(mtcars, "mpg", "wt", suppress_smooth=TRUE)
  expect_length(p$layers, 1)
})


