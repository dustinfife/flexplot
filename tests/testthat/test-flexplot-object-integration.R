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

test_that("unsupported plot families retain legacy behavior", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  p = flexplot(mpg~factor(cyl), data=mtcars)
  expect_s3_class(p, "ggplot")
})
