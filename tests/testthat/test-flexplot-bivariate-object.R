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

testthat::test_that("Second-axis variables produce separate fitted curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = flexplot(y ~ x + b | a, data=small, method="lm")
  built = ggplot2::ggplot_build(p)
  
  smooth_idx = which(vapply(
    p$layers,
    function(layer) inherits(layer$stat, "StatSmooth"),
    logical(1)
  ))
  
  testthat::expect_length(smooth_idx, 1)
  
  smooth_data = built$data[[smooth_idx]]
  
  groups_per_panel = tapply(
    smooth_data$group,
    smooth_data$PANEL,
    function(x) length(unique(x))
  )
  
  testthat::expect_true(all(groups_per_panel == length(unique(small$b))))
})
