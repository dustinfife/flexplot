test_that("grouped distribution outlines are solid without changing fitted-line encoding", {
  for (type in c("boxplot", "violin")) {
    for (formula in list(y ~ a + b, y ~ a + z | b)) {
      p = flexplot(formula, small, plot.type=type, jitter=FALSE)
      built = ggplot2::ggplot_build(p)
      expect_identical(unique(built$data[[2]]$linetype), "solid")
      expect_gt(length(unique(built$data[[2]]$colour)), 1)
    }
  }
  ordinary = ggplot2::ggplot_build(flexplot(y ~ a + b, small, jitter=FALSE))
  expect_setequal(ordinary$data[[4]]$linetype, c("solid", "22", "42"))
})

test_that("grouped boxes use independent quartiles and aligned ordered group slots", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  p = flexplot(y ~ a + b, d, plot.type="boxplot", jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
                 c("GeomPoint", "GeomBoxplot"))
  expect_s3_class(p$layers[[2]]$stat, "StatBoxplot")
  expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
  colors = built$plot$scales$get_scales("colour")$map(levels(d$b))
  expect_identical(built$plot$scales$get_scales("colour")$get_limits(), levels(d$b))
  expect_true(is.ordered(p$data$b))
  boxes = built$data[[2]]
  expect_equal(nrow(boxes), nlevels(d$a) * nlevels(d$b))
  for (i in seq_len(nrow(boxes))) {
    box = boxes[i, ]
    category = levels(d$a)[round(box$x)]
    group = match(box$colour, colors)
    values = d$y[d$a == category & d$b == levels(d$b)[group]]
    expected = quantile(values, c(.25, .5, .75))
    expect_equal(c(box$lower, box$middle, box$upper), unname(expected))
    expect_equal(as.numeric(box$x), match(category, levels(d$a)) +
                   .5 * ((group - .5) / nlevels(d$b) - .5))
    raw = built$data[[1]]
    raw = raw[round(raw$x) == round(box$x) & raw$colour == box$colour, ]
    expect_equal(as.numeric(raw$x), rep(as.numeric(box$x), length(values)))
    expect_equal(sort(raw$y), sort(values))
  }
  for (category in seq_len(nlevels(d$a))) {
    local = boxes[round(boxes$x) == category, ]
    local = local[order(local$x), ]
    expect_true(all(head(local$xmax, -1) < tail(local$xmin, -1)))
  }
  expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
  legends = grob$grobs[grepl("^guide-box", grob$layout$name)]
  expect_true(any(vapply(legends, inherits, logical(1), "gtable")))
})

test_that("binned grouped distributions retain full-data statistics under sampling", {
  reference = small
  reference$a = ordered(reference$a, levels=c("b", "a"))
  reference$z_bin = cut(reference$z, quantile(reference$z, seq(0, 1, length.out=4)),
                         include.lowest=TRUE, labels=FALSE)
  for (type in c("boxplot", "violin")) {
    full = flexplot(y ~ a + z | b, reference, plot.type=type, jitter=FALSE)
    full_built = ggplot2::ggplot_build(full)
    sampled = withr::with_seed(12, flexplot(y ~ a + z | b, reference,
                                             plot.type=type, jitter=FALSE, sample=1))
    built = ggplot2::ggplot_build(sampled)
    expect_s3_class(full$layers[[2]]$geom, if (type == "boxplot") "GeomBoxplot" else "GeomViolin")
    expect_equal(as.integer(full$data$z_binned), reference$z_bin)
    expect_equal(nrow(built$data[[1]]), 1)
    expect_equal(built$data[[2]], full_built$data[[2]])
    expect_equal(as.character(built$layout$layout$b), levels(reference$b))
    cells = split(full_built$data[[2]],
                    interaction(full_built$data[[2]]$PANEL, full_built$data[[2]]$group, drop=TRUE))
    colors = full_built$plot$scales$get_scales("colour")$map(levels(full$data$z_binned))
    for (cell in cells) {
      category = levels(reference$a)[round(cell$x[1])]
      bin = match(cell$colour[1], colors)
      panel = full_built$layout$layout$b[match(cell$PANEL[1], full_built$layout$layout$PANEL)]
      values = reference$y[reference$a == category & reference$z_bin == bin & reference$b == panel]
      expect_equal(unique(as.numeric(cell$x)), match(category, levels(reference$a)) +
                     .5 * ((bin - .5) / 3 - .5))
      if (type == "boxplot") {
        expect_equal(c(cell$lower, cell$middle, cell$upper), unname(quantile(values, c(.25, .5, .75))))
      } else {
        expect_gte(length(values), 2)
        expect_equal(unique(cell$n), length(values))
        expect_true(all(is.finite(cell$density) & cell$density >= 0))
        expect_equal(range(cell$y), range(values))
      }
    }
    expect_no_warning(ggplot2::ggplotGrob(full))
    expect_no_warning(ggplot2::ggplotGrob(sampled))
  }
})

test_that("incomplete grouped distributions keep singleton observations without fake densities", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  d$panel = ordered(d$y_bin, levels=c(1, 0))
  # One absent cell and one singleton cell; all declared levels remain observed.
  d = d[!(d$a == "a" & d$b == "x" & d$panel == "1"), ]
  singleton = which(d$a == "b" & d$b == "y" & d$panel == "0")
  if (length(singleton) > 1) d = d[-singleton[-1], ]
  d$y[1] = NA
  eligible = d[complete.cases(d[c("y", "a", "b", "panel")]), ]
  counts = aggregate(y ~ a + b + panel, eligible, length)
  expect_true(any(counts$y == 1))
  for (type in c("boxplot", "violin")) {
    p = flexplot(y ~ a + b | panel, d, plot.type=type, jitter=FALSE)
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(sort(built$data[[1]]$y), sort(eligible$y))
    drawn = unique(built$data[[2]][c("x", "colour", "PANEL")])
    expected = if (type == "boxplot") counts else counts[counts$y > 1, ]
    colors = built$plot$scales$get_scales("colour")$map(expected$b)
    panels = built$layout$layout$PANEL[match(expected$panel, built$layout$layout$panel)]
    expect_setequal(paste(drawn$PANEL, round(drawn$x), drawn$colour),
                     paste(panels, as.integer(expected$a), colors))
    expect_equal(as.numeric(drawn$x), round(as.numeric(drawn$x)) +
                   .5 * ((match(drawn$colour, built$plot$scales$get_scales("colour")$map(levels(d$b))) - .5) / 3 - .5))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
  singletons = small[!duplicated(small[c("a", "b")]), ]
  p = flexplot(y ~ a + b, singletons, plot.type="violin", jitter=FALSE)
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_equal(sort(built$data[[1]]$y), sort(singletons$y))
  expect_equal(nrow(built$data[[2]]), 0)
  expect_no_warning(ggplot2::ggplotGrob(p))
})
