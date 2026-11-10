test_that("categorical lines connect summary values in displayed factor order", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  for (spread in c("quartiles", "stdev", "sterr")) {
    p = flexplot(y ~ a, d, plot.type="line", spread=spread, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
                 c("GeomPoint", "GeomLine"))
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), c("b", "a"))
    expected = tapply(d$y, d$a, if (spread == "quartiles") median else mean)
    expect_equal(built$data[[2]]$y, as.numeric(expected))
    expect_equal(as.numeric(built$data[[2]]$x), c(1, 2))
    expect_equal(length(unique(built$data[[2]]$group)), 1L)
    expect_equal(sort(built$data[[1]]$y), sort(d$y))
  }
  # Unordered factors retain flexplot's existing response-based display order.
  d$a = factor(d$a, levels=c("b", "a"))
  ordinary = ggplot2::ggplot_build(flexplot(y ~ a, d, jitter=FALSE))
  lines = ggplot2::ggplot_build(flexplot(y ~ a, d, plot.type="line", jitter=FALSE))
  expect_equal(lines$layout$panel_scales_x[[1]]$get_limits(),
               ordinary$layout$panel_scales_x[[1]]$get_limits())
  expect_equal(lines$data[[2]]$y, ordinary$data[[2]]$y)
})

test_that("grouped categorical lines match independent centers and summary positions", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  for (spread in c("quartiles", "stdev", "sterr")) {
    p = flexplot(y ~ a + b, d, plot.type="line", spread=spread, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
                 c("GeomPoint", "GeomPoint", "GeomErrorbar", "GeomLine"))
    line = built$data[[4]]
    centers = aggregate(y ~ a + b, d, if (spread == "quartiles") median else mean)
    centers$colour = built$plot$scales$get_scales("colour")$map(centers$b)
    centers = centers[order(centers$colour, as.numeric(centers$a)), ]
    line = line[order(line$colour, line$x), ]
    points = built$data[[2]]
    points = points[order(points$colour, points$x), ]
    expect_equal(line$y, centers$y)
    expect_equal(line$x, points$x)
    expect_equal(line$colour, points$colour)
    expect_equal(length(unique(line$group)), 3L)
    expect_true(all(vapply(split(line$colour, line$group), function(x) length(unique(x)) == 1,
                           logical(1))))
  }
})

test_that("categorical line paths break at missing combinations and retain singleton points", {
  d = small[rep(seq_len(nrow(small)), each=5), ]
  d$category = ordered(rep(LETTERS[1:5], nrow(small)), levels=LETTERS[1:5])
  d = d[!(d$b == "x" & d$category == "C") &
          !(d$b == "y" & d$category %in% c("B", "D")), ]
  # A missing response also removes its cell from the eligible observations.
  d$y[d$b == "z" & d$category == "C" & d$a == "a"] = NA_real_
  p = flexplot(y ~ category + b | a, d, plot.type="line", jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  line = built$data[[4]]
  points = built$data[[2]]
  colours = built$plot$scales$get_scales("colour")$map(c("x", "y", "z"))
  expect_equal(sum(points$colour == colours[2]), 6L)
  expect_false(any(line$colour == colours[2]))
  expect_equal(sum(line$colour == colours[1]), 8L)
  for (path in split(line, interaction(line$PANEL, line$group, drop=TRUE))) {
    expect_gte(nrow(path), 2L)
    expect_equal(unique(path$colour), path$colour[1])
    expect_true(all(diff(sort(round(path$x))) == 1))
  }
  expected = aggregate(y ~ category + b + a, d, median)
  expect_equal(nrow(points), nrow(expected))
  expect_equal(sort(points$y), sort(expected$y))
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("categorical line summaries and facets survive binning and observation sampling", {
  cases = list(list(formula=y ~ a + b | z, group="b"),
               list(formula=y ~ a + z | b, group="z_binned"),
               list(formula=y ~ a | z, group=NULL))
  for (case in cases) {
    set.seed(123)
    full = flexplot(case$formula, small, plot.type="line", jitter=FALSE)
    set.seed(123)
    sampled = flexplot(case$formula, small, plot.type="line", sample=1, jitter=FALSE)
    full_built = ggplot2::ggplot_build(full)
    sampled_built = ggplot2::ggplot_build(sampled)
    index = which(vapply(full$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)))
    expect_equal(sampled_built$data[[index]], full_built$data[[index]])
    expect_equal(sampled_built$layout$layout, full_built$layout$layout)
    expect_equal(nrow(sampled_built$data[[1]]), 1L)
    line = full_built$data[[index]]
    # Independent cell medians: facet membership is taken from the prepared
    # display bins, not recomputed using a different binning convention.
    x = "a"
    group = case$group
    facets = intersect(names(full$data), names(full_built$layout$layout))
    expected = aggregate(full$data$y, full$data[c(x, group, facets)], median)
    for (i in seq_len(nrow(line))) {
      panel = full_built$layout$layout[match(line$PANEL[i], full_built$layout$layout$PANEL), ]
      category = full_built$layout$panel_scales_x[[1]]$get_limits()[round(line$x[i])]
      rows = as.character(expected[[x]]) == category
      for (facet in facets) rows = rows & expected[[facet]] == panel[[facet]]
      if (!is.null(group)) {
        colour = full_built$plot$scales$get_scales("colour")$map(expected[[group]])
        rows = rows & colour == line$colour[i]
      }
      expect_equal(line$y[i], expected$x[rows])
    }
    expect_silent(ggplot2::ggplotGrob(sampled))
  }
})
