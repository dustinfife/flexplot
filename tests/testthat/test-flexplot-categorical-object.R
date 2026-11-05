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

high_cardinality_message_text = "It looks like you're trying to plot more than 6 groups. I gotta give it to you...you're ambitious! Alas, things get a little crowded with that many colors, lines, and symbols. So I'm keeping the colors, ditching the fancy symbols and line types, and hiding the legend. Don't worry, all your data are still there. I hope we can still be friends."

test_that("high-cardinality categorical groups keep raw points and summaries drawable", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$g = factor(rep(letters[1:7], length.out=nrow(d)))
  for (variable in c("g", "z")) {
    formula = stats::reformulate(c("a", variable), response="y")
    expect_message(
      { p = flexplot(formula, data=d, bins=7, jitter=FALSE) },
      high_cardinality_message_text, fixed=TRUE
    )
    group = if (variable == "g") "g" else "z_binned"
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
    expect_s3_class(grob, "gtable")
    guides = grob$grobs[grepl("^guide-box", grob$layout$name)]
    expect_false(any(vapply(guides, inherits, logical(1), "gtable")))
    expect_null(p$mapping$shape)
    expect_null(p$mapping$linetype)
    expect_identical(rlang::as_label(p$mapping$colour), group)
    expect_length(p$layers, 4)
    expect_equal(nrow(built$data[[1]]), nrow(d))
    expect_true(all(built$data[[1]]$alpha == .2))
    expect_equal(sort(built$data[[1]]$y), sort(d$y))
    expect_false(anyNA(built$data[[1]][c("x", "y", "shape", "colour")]))
    expect_length(unique(built$data[[1]]$shape), 1)

    expected = stats::aggregate(p$data$y, by=p$data[c("a", group)], FUN=median)
    expected$lower = stats::aggregate(p$data$y, by=p$data[c("a", group)],
                                       FUN=quantile, probs=.25)$x
    expected$upper = stats::aggregate(p$data$y, by=p$data[c("a", group)],
                                       FUN=quantile, probs=.75)$x
    expected$colour = built$plot$scales$get_scales("colour")$map(expected[[group]])
    expected = expected[order(expected$colour, as.numeric(expected$a)), ]
    for (layer in 2:4) {
      drawn = built$data[[layer]]
      drawn = drawn[order(drawn$colour, round(drawn$x)), ]
      expect_equal(nrow(drawn), nrow(expected))
      expect_equal(drawn$y, expected$x)
      expect_equal(drawn$colour, expected$colour)
      expect_false(anyNA(drawn[c("x", "y", "colour")]))
      if (layer == 3) {
        expect_equal(drawn$ymin, expected$lower)
        expect_equal(drawn$ymax, expected$upper)
      }
    }
    expect_false(anyNA(built$data[[2]]$shape))
    expect_length(unique(built$data[[2]]$shape), 1)
    expect_equal(length(unique(built$data[[4]]$group)), 7)
    expect_true(all(built$data[[4]]$linetype == 1))
    expect_equal(as.integer(table(built$data[[1]]$colour)),
                 as.integer(table(built$plot$scales$get_scales("colour")$map(p$data[[group]]))))
    if (variable == "g") categorical = p
  }

  # Color-only rendering retains the established legacy data and statistics.
  options(flexplot.object_renderer=FALSE)
  expect_message({ legacy = flexplot(y ~ a + g, data=d, jitter=FALSE) },
                  high_cardinality_message_text, fixed=TRUE)
  native_built = ggplot2::ggplot_build(categorical)
  legacy_built = ggplot2::ggplot_build(legacy)
  expect_no_warning({ legacy_grob = ggplot2::ggplotGrob(legacy) })
  legacy_guides = legacy_grob$grobs[grepl("^guide-box", legacy_grob$layout$name)]
  expect_false(any(vapply(legacy_guides, inherits, logical(1), "gtable")))
  for (layer in 1:4) {
    columns = c("x", "y", "colour", "shape", "linetype", "group", "PANEL")
    columns = intersect(columns, names(native_built$data[[layer]]))
    if (layer == 3) columns = c(columns, "ymin", "ymax")
    expect_equal(native_built$data[[layer]][columns], legacy_built$data[[layer]][columns])
  }
})

test_that("high-cardinality categorical groups preserve order and suppress legends", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (ordered_group in c(FALSE, TRUE)) {
    d = small
    group_levels = rev(letters[1:7])
    d$g = factor(rep(letters[1:7], length.out=nrow(d)),
                  levels=group_levels, ordered=ordered_group)
    expect_message({ p = flexplot(y ~ a + g, data=d, jitter=FALSE, suppress_smooth=TRUE) },
                    high_cardinality_message_text, fixed=TRUE)
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_identical(levels(p$data$g), group_levels)
    expect_identical(is.ordered(p$data$g), ordered_group)
    expect_identical(built$plot$scales$get_scales("colour")$get_limits(), group_levels)
    expect_length(p$layers, 1)
    expect_equal(nrow(built$data[[1]]), nrow(d))
    expect_false(anyNA(built$data[[1]]$shape))
    expect_no_warning(ggplot2::ggplotGrob(p))
    legends = cowplot::get_plot_component(p, "guide-box", return_all=TRUE)
    expect_false(any(vapply(legends, inherits, logical(1), "gtable")))
    hidden = p + ggplot2::theme(legend.position="none")
    hidden_legends = cowplot::get_plot_component(hidden, "guide-box", return_all=TRUE)
    expect_false(any(vapply(hidden_legends, inherits, logical(1), "gtable")))
    expect_no_warning(ggplot2::ggplotGrob(hidden))
  }
})

test_that("three categorical groups retain legacy shapes, line types, and colors", {
  withr::local_options(flexplot.object_renderer=TRUE)

  expect_silent({ p = flexplot(y ~ a + b, data=small, jitter=FALSE) })
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
  guides = grob$grobs[grepl("^guide-box", grob$layout$name)]
  expect_true(any(vapply(guides, inherits, logical(1), "gtable")))
  expect_identical(rlang::as_label(p$mapping$shape), "b")
  expect_identical(rlang::as_label(p$mapping$linetype), "b")
  expect_equal(sort(unique(built$data[[1]]$shape)), c(15, 16, 17))
  expect_setequal(built$data[[4]]$linetype, c("solid", "22", "42"))
  source_groups = small$b[match(built$data[[1]]$y, small$y)]
  expect_equal(built$data[[1]]$shape,
               unname(c(x=16, y=17, z=15)[as.character(source_groups)]))

  options(flexplot.object_renderer=FALSE)
  legacy = flexplot(y ~ a + b, data=small, jitter=FALSE)
  legacy_built = ggplot2::ggplot_build(legacy)
  for (layer in 1:4) {
    columns = c("x", "y", "colour", "shape", "linetype", "group", "PANEL")
    columns = intersect(columns, names(built$data[[layer]]))
    if (layer == 3) columns = c(columns, "ymin", "ymax")
    expect_equal(built$data[[layer]][columns], legacy_built$data[[layer]][columns])
  }
})

test_that("up to six observed categorical groups retain legends without diagnostics", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (n_groups in c(3, 6)) {
    d = small
    d$g = factor(rep(letters[seq_len(n_groups)], length.out=nrow(d)),
                  levels=letters[1:7])
    expect_silent({ p = flexplot(y ~ a + g, data=d, jitter=FALSE) })
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
    guides = grob$grobs[grepl("^guide-box", grob$layout$name)]
    expect_true(any(vapply(guides, inherits, logical(1), "gtable")))
    expect_equal(nrow(built$data[[1]]), nrow(d))
    expect_false(anyNA(built$data[[1]]$shape))
    expect_equal(length(unique(built$data[[1]]$shape)), n_groups)
    expect_equal(length(unique(built$data[[4]]$linetype)), n_groups)
  }
})
