test_that("categorical predictor uses object renderer with summary layers", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  d = transform(mtcars, cyl=factor(cyl))
  p = flexplot(mpg ~ cyl, data=d)
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 3)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("categorical summaries respect suppression and spread", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  d = transform(mtcars, cyl=factor(cyl))
  p = flexplot(mpg ~ cyl, data=d, suppress_smooth=TRUE)
  expect_equal(length(p$layers), 1)
  expect_silent(ggplot2::ggplot_build(p))

  for (spread in c("quartiles", "stdev", "sterr")) {
    p = flexplot(mpg ~ cyl, data=d, spread=spread)
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("categorical boxplot, violin and faceting build", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  d = transform(mtcars, cyl=factor(cyl), am=factor(am))
  for (type in c("boxplot", "violin")) {
    p = flexplot(mpg ~ cyl | am, data=d, plot.type=type)
    expect_equal(length(p$layers), 2)
    expect_silent(ggplot2::ggplot_build(p))
  }
})
