test_that("Numeric outcomes produce histograms", {
  
  p = flexplot_distribution_object(mtcars, "mpg")
  
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")
})

test_that("Categorical outcomes produce bar charts", {
  
  d = mtcars
  d$cyl = factor(d$cyl)
  
  p = flexplot_distribution_object(d, "cyl")
  
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$stat, "StatCount")
})

test_that("Density plots work", {
  
  p = flexplot_distribution_object(
    mtcars, "mpg", plot.type="density"
  )
  
  expect_s3_class(p$layers[[1]]$stat, "StatDensity")
})

test_that("Q-Q plots work", {
  
  p = flexplot_distribution_object(
    mtcars, "mpg", plot.type="qq"
  )
  
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$stat, "StatQq")
})

test_that("Univariate boxplots work", {
  
  p = flexplot_distribution_object(
    mtcars, "mpg", plot.type="boxplot"
  )
  
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")
})

test_that("Distribution plots integrate with flexplot", {
  
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)
  
  p = flexplot(mpg ~ 1, data=mtcars)
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

