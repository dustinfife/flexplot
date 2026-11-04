test_that("object renderer integrates with flexplot without changing its API", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  p = flexplot(mpg~wt, data=mtcars, method="lm", se=FALSE)
  expect_s3_class(p, "ggplot")
  expect_identical(p$formula, mpg~wt)
  expect_equal(length(p$layers), 2)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("object renderer supports facets and suppressed smoothing", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  p = flexplot(mpg~wt | cyl, data=mtcars, suppress_smooth=TRUE)
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
  expect_silent(ggplot2::ggplot_build(p))
})

unexpected_legacy_renderer = function(...) {
  stop("The legacy renderer was called.")
}

unexpected_native_renderer = function(...) {
  stop("The native renderer was called.")
}

test_that("native opt-in selects the object renderer", {
  withr::local_options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(
    flexplot_bivariate_plot=unexpected_legacy_renderer, .package="flexplot"
  )

  p = flexplot(y ~ x, data=small, method="lm", se=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_equal(built$data[[1]]$y, small$y)
  fit = stats::lm(y ~ x, data=small)
  expect_equal(built$data[[2]]$y,
               unname(stats::predict(fit, newdata=built$data[[2]])))
})

test_that("native opt-out selects the legacy renderer", {
  withr::local_options(flexplot.object_renderer=FALSE)
  testthat::local_mocked_bindings(
    flexplot_render_object=unexpected_native_renderer, .package="flexplot"
  )

  p = flexplot(y ~ x, data=small, method="lm", se=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_equal(built$data[[1]]$y, small$y)
  fit = stats::lm(y ~ x, data=small)
  expect_equal(built$data[[2]]$y,
               unname(stats::predict(fit, newdata=built$data[[2]])))
})

test_that("plot.string forces legacy rendering even with native opt-in", {
  withr::local_options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(
    flexplot_render_object=unexpected_native_renderer, .package="flexplot"
  )

  expect_output(
    {
      p = flexplot(y ~ x, data=small, method="lm", se=FALSE,
                   jitter=FALSE, plot.string=TRUE)
    },
    "ggplot(small", fixed=TRUE
  )
  built = ggplot2::ggplot_build(p)
  expect_identical(p$formula, y ~ x)
  expect_equal(built$data[[1]]$x, small$x)
  expect_equal(built$data[[1]]$y, small$y)
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  fit = stats::lm(y ~ x, data=small)
  expect_equal(built$data[[2]]$y,
               unname(stats::predict(fit, newdata=built$data[[2]])))
})
