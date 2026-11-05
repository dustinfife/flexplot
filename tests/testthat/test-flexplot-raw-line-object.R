raw_line_is_smooth = function(layer) {
  inherits(layer$stat, "StatSmooth") || inherits(layer$geom, "GeomRibbon")
}

expect_observed_line_paths = function(p, data, group=NULL, facets=character()) {
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_s3_class(p$layers[[2]]$geom, "GeomLine")
  expect_s3_class(p$layers[[2]]$stat, "StatIdentity")
  expect_false(any(vapply(p$layers, raw_line_is_smooth, logical(1))))
  points = built$data[[1]]
  lines = built$data[[2]]
  expect_equal(nrow(lines), nrow(points))
  expect_equal(p$layers[[2]]$data, p$layers[[1]]$data)
  if (!nrow(lines)) return(invisible(built))
  rows = match(lines$y, data$y)
  expect_false(anyNA(rows))
  if (anyNA(rows)) return(invisible(built))
  expect_equal(lines$x, data$x[rows], ignore_attr=TRUE)
  expect_equal(sort(lines$y), sort(points$y))
  expect_false(any(c("ymin", "ymax") %in% names(lines)))

  if (!is.null(group)) {
    colors = built$plot$scales$get_scales("colour")
    expect_equal(lines$colour, colors$map(data[[group]][rows]))
  }
  layout = built$layout$layout
  for (panel in seq_len(nrow(layout))) {
    block = lines[lines$PANEL == layout$PANEL[panel], ]
    selected = data[match(block$y, data$y), , drop=FALSE]
    for (facet in facets) {
      expect_true(all(as.character(selected[[facet]]) == as.character(layout[[facet]][panel])))
    }
    for (id in unique(block$group)) {
      path = block[block$group == id, ]
      original = data[match(path$y, data$y), , drop=FALSE]
      expect_true(all(diff(path$x) >= 0))
      if (!is.null(group)) {
        expect_equal(length(unique(original[[group]])), 1)
        expect_equal(length(unique(path$colour)), 1)
      }
    }
  }
  invisible(built)
}

test_that("native numeric lines match legacy observed coordinates and appearance", {
  withr::local_options(flexplot.object_renderer=TRUE)
  native = flexplot(y ~ x, small, plot.type="line", method="lm", se=TRUE, jitter=FALSE)
  built = expect_observed_line_paths(native, small)
  expect_equal(built$data[[2]]$x, sort(small$x))
  expect_equal(built$data[[2]]$y, small$y[order(small$x)])
  fitted = stats::predict(stats::lm(y ~ x, data=small), newdata=small[order(small$x), ])
  expect_true(any(abs(built$data[[2]]$y - fitted) > .01))
  expect_no_warning(ggplot2::ggplotGrob(native))
  options(flexplot.object_renderer=FALSE)
  legacy = flexplot(y ~ x, small, plot.type="line", method="lm", se=TRUE, jitter=FALSE)
  reference = ggplot2::ggplot_build(legacy)
  columns = c("x", "y", "colour", "linetype", "linewidth", "alpha", "group")
  expect_equal(built$data[[2]][columns], reference$data[[2]][columns])
})

test_that("raw lines override smoothing controls and keep observations unjittered", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (suppress in c(FALSE, TRUE)) {
    p = flexplot(y ~ x, small, plot.type="line", method="lm", se=TRUE,
                 suppress_smooth=suppress, jitter=c(.1, .1), alpha=.13, raw.data=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_false(any(vapply(p$layers, raw_line_is_smooth, logical(1))))
    expect_equal(built$data[[2]]$x, sort(small$x))
    expect_equal(built$data[[2]]$y, small$y[order(small$x)])
    expect_true(all(built$data[[1]]$alpha == 0))
    # As in legacy rendering, raw.data controls the point layer, not the requested line.
    expect_true(all(is.na(built$data[[2]]$alpha)))
  }
})

test_that("raw lines separate categorical and numeric groups within each facet", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  for (formula in list(y ~ x + b, y ~ x + b | a, y ~ x + z | a,
                       y ~ x + b | z, y ~ x | a)) {
    p = flexplot(formula, d, plot.type="line", method="lm", jitter=FALSE)
    expected = d
    if ("z_binned" %in% names(p$data)) {
      expected$z_binned = cut(d$z, stats::quantile(d$z, seq(0, 1, length.out=4)),
                               labels=levels(p$data$z_binned), include.lowest=TRUE)
    }
    axis = flexplot_axis_given(formula)$axis
    group = if (length(axis) < 2) NULL else if (axis[2] == "z") "z_binned" else "b"
    facets = if ("|" %in% all.names(formula)) {
      if (identical(flexplot_axis_given(formula)$given, "z")) "z_binned" else "a"
    } else character()
    built = expect_observed_line_paths(p, expected, group, facets)
    expect_equal(nrow(built$data[[2]]), nrow(d))
    if (!is.null(group)) {
      expect_equal(built$plot$scales$get_scales("colour")$get_limits(), levels(expected[[group]]))
    }
  }
})

test_that("raw line sampling connects only selected observations and preserves structure", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  d = small
  d$b = factor(d$b, levels=c("z", "x", "y"))
  for (formula in list(y ~ x, y ~ x + b | a)) {
    full = flexplot(formula, d, plot.type="line", method="lm", jitter=FALSE)
    reference = ggplot2::ggplot_build(full)
    for (size in c(0, 1, 8)) {
      set.seed(123)
      p = flexplot(formula, d, plot.type="line", method="lm", sample=size, jitter=FALSE)
      built = expect_observed_line_paths(p, d, if (length(all.vars(formula)) > 2) "b" else NULL,
                                          if (length(all.vars(formula)) > 2) "a" else character())
      expect_equal(nrow(built$data[[2]]), size)
      expect_equal(built$layout$layout, reference$layout$layout)
      expect_equal(built$layout$panel_params[[1]]$x.range, reference$layout$panel_params[[1]]$x.range)
      expect_equal(built$layout$panel_params[[1]]$y.range, reference$layout$panel_params[[1]]$y.range)
      if (is.null(p$mapping$colour)) next
      expect_equal(built$plot$scales$get_scales("colour")$map(levels(d$b)),
                     reference$plot$scales$get_scales("colour")$map(levels(d$b)))
    }
  }
})

test_that("raw lines exclude missing plotting values before sampling", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$x[2] = NA
  d$y[4] = NA
  d$b[6] = NA
  d$a[8] = NA
  eligible = d[stats::complete.cases(d[c("y", "x", "b", "a")]), ]
  p = flexplot(y ~ x + b | a, d, plot.type="line", method="lm", jitter=FALSE)
  built = expect_observed_line_paths(p, eligible, "b", "a")
  expect_equal(nrow(built$data[[2]]), nrow(eligible))
  expect_true(all(is.finite(built$data[[2]]$x)))
  expect_true(all(is.finite(built$data[[2]]$y)))
})

test_that("ties in numeric x retain observed values and stable input ordering", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = data.frame(
    x=rep(c(6, 1, 1, 2, 3, 4, 5), 2), y=seq_len(14),
    b=factor(rep(c("G2", "G1"), each=7), levels=c("G1", "G2"))
  )
  p = flexplot(y ~ x + b, d, plot.type="line", jitter=FALSE)
  built = expect_observed_line_paths(p, d, "b")
  for (group in levels(d$b)) {
    expected = d[d$b == group, ]
    expected = expected[order(expected$x), ]
    color = built$plot$scales$get_scales("colour")$map(group)
    expect_equal(built$data[[2]]$y[built$data[[2]]$colour == color], expected$y)
  }
})

test_that("numeric raw-line ghosts retain observed reference curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  p = flexplot(y ~ x + b | a, small, plot.type="line", method="lm", jitter=FALSE,
               ghost.line="gray", ghost.reference=list(a="a"))
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_s3_class(p$layers[[3]]$geom, "GeomLine")
  ghost = built$data[[3]]
  reference = small[small$a == "a", ]
  expect_equal(nrow(ghost), 2 * nrow(reference))
  expect_equal(length(unique(ghost$group)), nlevels(small$b))
  expect_true(all(ghost$colour == "gray"))
  expect_setequal(ghost$y, reference$y)
  rows = match(ghost$y, reference$y)
  expect_equal(ghost$x, reference$x[rows])
  for (id in unique(ghost$group)) {
    expect_equal(length(unique(reference$b[rows[ghost$group == id]])), 1)
  }
})

test_that("ordinary numeric scatterplots and fitted smooths are unchanged", {
  withr::local_options(flexplot.object_renderer=TRUE)
  scatter = flexplot(y ~ x, small, suppress_smooth=TRUE, jitter=FALSE)
  expect_length(scatter$layers, 1)
  expect_s3_class(scatter$layers[[1]]$geom, "GeomPoint")
  fitted = flexplot(y ~ x, small, method="lm", se=TRUE, jitter=FALSE)
  built = ggplot2::ggplot_build(fitted)
  expect_s3_class(fitted$layers[[2]]$stat, "StatSmooth")
  expect_equal(nrow(built$data[[1]]), nrow(small))
  model = stats::lm(y ~ x, data=small)
  expected = stats::predict(model, newdata=data.frame(x=built$data[[2]]$x), interval="confidence")
  expect_equal(built$data[[2]]$y, unname(expected[, "fit"]))
  expect_equal(built$data[[2]]$ymin, unname(expected[, "lwr"]))
  expect_equal(built$data[[2]]$ymax, unname(expected[, "upr"]))
})
