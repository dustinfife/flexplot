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

test_that("categorical second axes preserve summaries and connecting lines", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (spread in c("quartiles", "stdev", "sterr")) {
    p = flexplot(y ~ a + b, data=small, spread=spread, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_length(p$layers, 4)
    expect_s3_class(p$layers[[4]]$geom, "GeomLine")
    expect_s3_class(p$layers[[1]]$position, "PositionJitterdodged")

    center = if (spread == "quartiles") median else mean
    expected = aggregate(y ~ a + b, data=p$data, FUN=center)
    if (spread == "quartiles") {
      lower = aggregate(y ~ a + b, data=p$data, FUN=quantile, probs=.25)$y
      upper = aggregate(y ~ a + b, data=p$data, FUN=quantile, probs=.75)$y
    } else {
      interval = aggregate(y ~ a + b, data=p$data, FUN=sd)$y
      if (spread == "sterr") {
        counts = aggregate(y ~ a + b, data=p$data, FUN=length)$y
        interval = 1.96 * interval / sqrt(counts - 1)
      }
      lower = expected$y - interval
      upper = expected$y + interval
    }
    expected$lower = lower
    expected$upper = upper
    expected$colour = built$plot$scales$get_scales("colour")$map(expected$b)
    expected = expected[order(expected$colour, as.numeric(expected$a)), ]

    points = built$data[[2]]
    points = points[order(points$colour, round(points$x)), ]
    errors = built$data[[3]]
    errors = errors[order(errors$colour, round(errors$x)), ]
    lines = built$data[[4]]
    lines = lines[order(lines$colour, round(lines$x)), ]

    expect_equal(nrow(points), nrow(expected))
    expect_equal(points$y, expected$y)
    expect_equal(errors$ymin, expected$lower)
    expect_equal(errors$ymax, expected$upper)
    expect_equal(lines$y, expected$y)
    expect_equal(lines$colour, expected$colour)
    expect_equal(length(unique(lines$group)), nlevels(small$b))
    expect_equal(as.integer(table(lines$group)), rep(nlevels(small$a), nlevels(small$b)))
    expect_equal(length(unique(built$data[[1]]$shape)), nlevels(small$b))
    expect_equal(length(unique(lines$linetype)), nlevels(small$b))
  }
})

test_that("numeric second axes use binned groups within facets", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y ~ a + z | b, data=small, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_identical(rlang::as_label(p$mapping$colour), "z_binned")
  expect_identical(rlang::as_label(p$layers[[4]]$mapping$group), "z_binned")
  expected = aggregate(y ~ a + z_binned + b, data=p$data, FUN=median)
  expect_equal(sort(built$data[[2]]$y), sort(expected$y))
  expect_equal(sort(built$data[[4]]$y), sort(expected$y))
  expect_equal(length(unique(built$data[[4]]$PANEL)), nlevels(small$b))
  groups = unique(built$data[[4]][c("PANEL", "group")])
  expect_equal(as.integer(table(groups$PANEL)), rep(3L, nlevels(small$b)))
})

test_that("grouped categorical points respect suppression and sampling", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y ~ a + b, data=small, suppress_smooth=TRUE,
               sample=8, raw.data=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_length(p$layers, 1)
  expect_equal(nrow(built$data[[1]]), 8)
  expect_true(all(built$data[[1]]$alpha == 0))
  expect_equal(length(unique(built$data[[1]]$colour)), nlevels(small$b))
})
