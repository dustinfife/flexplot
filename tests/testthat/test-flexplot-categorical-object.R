test_that("categorical summary points and intervals equal base R statistics", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  values = split(d$y, d$a)
  for (spread in c("quartiles", "stdev", "sterr")) {
    p = flexplot(y ~ a, d, spread=spread, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
                   c("GeomPoint", "GeomPoint", "GeomErrorbar"))
    center = vapply(values, if (spread == "quartiles") median else mean, numeric(1))
    radius = vapply(values, sd, numeric(1))
    if (spread == "sterr") radius = 1.96 * radius / sqrt(lengths(values) - 1)
    lower = if (spread == "quartiles") vapply(values, quantile, numeric(1), probs=.25) else center - radius
    upper = if (spread == "quartiles") vapply(values, quantile, numeric(1), probs=.75) else center + radius
    expect_equal(sort(built$data[[1]]$y), sort(d$y))
    expect_equal(as.numeric(built$data[[2]]$x), seq_along(values))
    expect_equal(built$data[[2]]$y, unname(center))
    expect_equal(built$data[[3]]$ymin, unname(lower))
    expect_equal(built$data[[3]]$ymax, unname(upper))
  }
})

test_that("categorical summary suppression retains raw observations", {
  p = flexplot(y ~ a, small, suppress_smooth=TRUE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_equal(sort(built$data[[1]]$y), sort(small$y))
})

test_that("categorical boxplot, violin and faceting build", {

  d = transform(mtcars, cyl=factor(cyl), am=factor(am))
  for (type in c("boxplot", "violin")) {
    p = flexplot(mpg ~ cyl | am, data=d, plot.type=type)
    expect_equal(length(p$layers), 2)
    expect_s3_class(p$layers[[2]]$geom, if (type == "boxplot") "GeomBoxplot" else "GeomViolin")
    expect_silent({ built = ggplot2::ggplot_build(p) })
    expect_equal(sort(built$data[[1]]$y), sort(d$mpg))
    expect_equal(nrow(built$layout$layout), nlevels(d$am))
    if (type == "boxplot") {
      boxes = built$data[[2]]
      panel = built$layout$layout$am[match(boxes$PANEL, built$layout$layout$PANEL)]
      categories = built$layout$panel_scales_x[[1]]$get_limits()[round(boxes$x)]
      expected = vapply(seq_len(nrow(boxes)), function(i) {
        quantile(d$mpg[d$cyl == categories[i] & d$am == panel[i]], c(.25, .5, .75))
      }, numeric(3))
      expect_equal(boxes$lower, unname(expected[1, ]))
      expect_equal(boxes$middle, unname(expected[2, ]))
      expect_equal(boxes$upper, unname(expected[3, ]))
    } else {
      expect_true(all(is.finite(built$data[[2]]$density)))
      expect_true(all(built$data[[2]]$density >= 0))
    }
  }
})

test_that("categorical second axes preserve summaries and connecting lines", {

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

  p = flexplot(y ~ a + b, data=small, suppress_smooth=TRUE,
               sample=8, raw.data=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_false(any(vapply(p$layers, function(layer) inherits(layer$stat, "StatSummary"), logical(1))))
  expect_equal(nrow(built$data[[1]]), 8)
  expect_true(all(built$data[[1]]$alpha == 0))
  expect_equal(length(unique(built$data[[1]]$colour)), nlevels(small$b))
})

test_that("faceted categorical summaries use eligible rows and survive sampling", {
  d = small[rep(seq_len(nrow(small)), each=5), ]
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  d$panel = ordered(d$y_bin, levels=c(1, 0))
  d = d[!(d$a == "a" & d$b == "x" & d$panel == "1"), ]
  d$y[1] = NA
  d$b[2] = NA
  d$panel[3] = NA
  eligible = d[complete.cases(d[c("y", "a", "b", "panel")]), ]
  expected = aggregate(y ~ a + b + panel, eligible, mean)
  counts = aggregate(y ~ a + b + panel, eligible, length)$y
  interval = 1.96 * aggregate(y ~ a + b + panel, eligible, sd)$y / sqrt(counts - 1)
  for (sample in c(Inf, 1)) {
    p = withr::with_seed(36, flexplot(y ~ a + b | panel, d, spread="sterr",
                                       jitter=FALSE, sample=sample))
    built = ggplot2::ggplot_build(p)
    expect_equal(unname(vapply(p$layers[1:4], function(layer) class(layer$geom)[1], character(1))),
                   c("GeomPoint", "GeomPoint", "GeomErrorbar", "GeomLine"))
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
    expect_equal(as.character(built$layout$layout$panel), levels(d$panel))
    expect_equal(built$plot$scales$get_scales("colour")$get_limits(), levels(d$b))
    raw = built$data[[1]]
    expect_equal(nrow(raw), min(sample, nrow(eligible)))
    rows = match(raw$y, eligible$y)
    expect_false(anyNA(rows))
    panels = built$layout$layout
    expect_equal(as.character(raw$PANEL),
                   as.character(panels$PANEL[match(eligible$panel[rows], panels$panel)]))
    expect_equal(round(as.numeric(raw$x)), as.numeric(eligible$a[rows]))
    expect_equal(raw$colour, built$plot$scales$get_scales("colour")$map(eligible$b[rows]))
    if (is.infinite(sample)) expect_equal(sort(raw$y), sort(eligible$y))
    expected_panel = panels$PANEL[match(expected$panel, panels$panel)]
    expected_color = built$plot$scales$get_scales("colour")$map(expected$b)
    expected_key = paste(expected_panel, as.integer(expected$a), expected_color)
    for (layer in 2:4) {
      drawn = built$data[[layer]]
      rows = match(paste(drawn$PANEL, round(drawn$x), drawn$colour), expected_key)
      expect_equal(nrow(drawn), nrow(expected))
      expect_false(anyNA(rows))
      expect_equal(drawn$y, expected$y[rows])
      if (layer == 3) {
        expect_equal(drawn$ymin, (expected$y - interval)[rows])
        expect_equal(drawn$ymax, (expected$y + interval)[rows])
      }
      if (layer == 4) {
        identity = interaction(expected$panel[rows], expected$b[rows], drop=TRUE)
        expect_equal(length(unique(drawn$group)), nlevels(d$b))
        expect_true(all(vapply(split(identity, interaction(drawn$PANEL, drawn$group)),
                               function(values) length(unique(values)) == 1, logical(1))))
      }
    }
  }
})

high_cardinality_message_text = "It looks like you're trying to plot more than 6 groups. I gotta give it to you...you're ambitious! Alas, things get a little crowded with that many colors, lines, and symbols. So I'm keeping the colors, ditching the fancy symbols and line types, and hiding the legend. Don't worry, all your data are still there. I hope we can still be friends."

test_that("high-cardinality categorical groups keep raw points and summaries drawable", {

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
    expect_true(all(built$data[[1]]$alpha == .4))
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
  }


})

test_that("high-cardinality categorical groups preserve order and suppress legends", {

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

test_that("three categorical groups retain their shape line-type and color assignments", {

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

  expected_colors = built$plot$scales$get_scales("colour")$map(source_groups)
  expect_equal(built$data[[1]]$colour, expected_colors)
  expected_types = c(x="solid", y="22", z="42")
  summaries = aggregate(y ~ a + b, small, median)
  keys = paste(round(built$data[[4]]$x), built$data[[4]]$colour)
  summaries$a = factor(summaries$a, levels=built$layout$panel_scales_x[[1]]$get_limits())
  rows = match(keys, paste(as.integer(summaries$a),
                            built$plot$scales$get_scales("colour")$map(summaries$b)))
  expect_false(anyNA(rows))
  expect_equal(built$data[[4]]$y, summaries$y[rows])
  expect_equal(built$data[[4]]$linetype, unname(expected_types[as.character(summaries$b[rows])]))

})

test_that("up to six observed categorical groups retain legends without diagnostics", {

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
