testthat::test_that("Magnet renderer creates expected layers", {
  
  set.seed(123)
  
  d = data.frame(
    x=factor(rep(c("A", "B", "C"), each=100)),
    y=c(
      rbinom(100, 1, 0.2),
      rbinom(100, 1, 0.5),
      rbinom(100, 1, 0.8)
    )
  )
  
  p = flexplot_magnet_object(d, "y", "x")
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  
  geoms = vapply(
    p$layers,
    function(layer) class(layer$geom)[1],
    character(1)
  )
  
  expect_equal(sum(geoms == "GeomPoint"), 2)
  expect_true("GeomHline" %in% geoms)
  expect_true("GeomText" %in% geoms)
})


testthat::test_that("Magnet stacking respects sample sizes", {
  
  d = data.frame(
    x=factor(rep(c("A", "B"), times=c(20, 100))),
    y=c(rep(0, 10), rep(1, 10),
        rep(0, 50), rep(1, 50))
  )
  
  prepared = prepare_magnet_data(d, "y", "x")
  
  expect_true(all(prepared$y_scaled >= 0))
  expect_true(all(prepared$y_scaled <= 1))
  
  # Smaller groups should have shorter stacks.
  small = prepared$y_scaled[prepared$x == "A" & prepared$y == 0]
  large = prepared$y_scaled[prepared$x == "B" & prepared$y == 0]
  
  expect_lt(max(small), max(large))
})


testthat::test_that("Magnet renderer preserves ordered factors", {
  
  set.seed(123)
  
  d = data.frame(
    x=ordered(rep(c("Low", "Medium", "High"), each=100),
              levels=c("Low", "Medium", "High")),
    y=c(
      rbinom(100, 1, 0.8),
      rbinom(100, 1, 0.5),
      rbinom(100, 1, 0.2)
    )
  )
  
  p = flexplot_magnet_object(d, "y", "x")
  
  expect_equal(
    levels(p$layers[[2]]$data$x),
    c("Low", "Medium", "High")
  )
})


testthat::test_that("Magnet renderer supports conditioning", {
  
  set.seed(123)
  
  d = expand.grid(
    x=factor(c("A", "B")),
    group=factor(c("Control", "Treatment")),
    id=seq_len(50)
  )
  
  d$y = rbinom(
    nrow(d), 1,
    plogis(ifelse(d$x == "B", 1, -1) +
             ifelse(d$group == "Treatment", 0.5, 0))
  )
  
  p = flexplot_magnet_object(
    d, "y", "x", given="group"
  )
  
  p = p + ggplot2::facet_wrap(~group)
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})


testthat::test_that("native magnet integration preserves stacks, counts, and fitted probabilities", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y_bin ~ a, data=small, method="logistic")
  built = ggplot2::ggplot_build(p)
  expect_length(p$layers, 4)
  expect_s3_class(p$layers[[1]]$geom, "GeomHline")
  expect_s3_class(p$layers[[2]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[4]]$geom, "GeomText")
  expect_equal(built$data[[1]]$yintercept, .5)
  expect_true(all(built$data[[3]]$colour == "blue"))
  expect_true(all(built$data[[2]]$alpha == .4))

  raw = p$layers[[2]]$data
  expect_equal(nrow(built$data[[2]]), nrow(small))
  expect_equal(raw$y_bin, small$y_bin)
  expect_equal(built$data[[2]]$y, raw$y_scaled)
  expect_true(all(raw$y_scaled >= 0 & raw$y_scaled <= 1))
  expect_true(all(raw$y_scaled[raw$y_bin == 0] < .5))
  expect_true(all(raw$y_scaled[raw$y_bin == 1] > .5))

  counts = p$layers[[4]]$data
  expect_equal(counts$n_total, as.integer(table(small$a)[as.character(counts$a)]))
  expect_equal(built$data[[4]]$label, paste0("N=", counts$n_total))

  fitted = p$layers[[3]]$data
  fit = stats::glm(y_bin ~ a, data=small, family=stats::binomial())
  expected = unname(stats::predict(fit, newdata=fitted, type="response"))
  expect_equal(nrow(fitted), nlevels(small$a))
  expect_equal(built$data[[3]]$y, expected)
})

testthat::test_that("native magnet integration preserves ordered x and conditioning", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  p = flexplot(y_bin ~ a | b, data=d, method="logistic")
  built = ggplot2::ggplot_build(p)
  raw = p$layers[[2]]$data
  fitted = p$layers[[3]]$data
  expect_identical(levels(raw$a), levels(d$a))
  expect_equal(nrow(built$layout$layout), nlevels(d$b))
  expect_equal(nrow(fitted), nlevels(d$a) * nlevels(d$b))
  expect_true(all(built$data[[3]]$colour == "blue"))

  fit = stats::glm(y_bin ~ a + b, data=d, family=stats::binomial())
  expect_equal(unname(fitted$pred_prob),
               unname(stats::predict(fit, newdata=fitted, type="response")))
  for (i in seq_len(nrow(built$layout$layout))) {
    panel = built$layout$layout[i, ]
    points = built$data[[2]][built$data[[2]]$PANEL == panel$PANEL, ]
    probabilities = built$data[[3]][built$data[[3]]$PANEL == panel$PANEL, ]
    labels = built$data[[4]][built$data[[4]]$PANEL == panel$PANEL, ]
    expected = fitted[fitted$b == panel$b, ]
    counts = p$layers[[4]]$data
    counts = counts[counts$b == panel$b, ]
    expect_equal(nrow(points), sum(d$b == panel$b))
    expect_equal(sort(probabilities$y), sort(unname(expected$pred_prob)))
    expect_equal(length(unique(probabilities$x)), nlevels(d$a))
    expect_equal(sort(labels$label), sort(paste0("N=", counts$n_total)))
    expect_equal(sum(counts$n_total), sum(d$b == panel$b))
  }
})

testthat::test_that("native magnet opacity controls affect only raw observations", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (show_raw in c(TRUE, FALSE)) {
    p = flexplot(y_bin ~ a, data=small, alpha=.17, raw.data=show_raw,
                 jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expected_alpha = if (show_raw) .17 else 0
    expect_true(all(built$data[[2]]$alpha == expected_alpha))
    expect_equal(nrow(built$data[[2]]), nrow(small))
    expect_true(all(is.na(built$data[[3]]$alpha)))
    expect_true(all(is.na(built$data[[4]]$alpha)))
    expect_equal(sum(p$layers[[4]]$data$n_total), nrow(small))
    fit = stats::glm(y_bin ~ a, data=small, family=stats::binomial())
    expect_equal(built$data[[3]]$y,
                 unname(stats::predict(fit, newdata=p$layers[[3]]$data, type="response")))
  }
})

testthat::test_that("native magnet sampling preserves full-data stacks, counts, and fits", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  d$row_id = seq_len(nrow(d))
  full = flexplot(y_bin ~ a | b, data=d, jitter=FALSE)
  p = flexplot(y_bin ~ a | b, data=d, sample=8, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  raw = p$layers[[2]]$data
  all_raw = full$layers[[2]]$data
  expect_equal(nrow(built$data[[2]]), 8)
  expect_equal(length(unique(raw$row_id)), 8)
  expect_true(all(raw$row_id %in% all_raw$row_id))
  expect_equal(raw$y_scaled, all_raw$y_scaled[match(raw$row_id, all_raw$row_id)])
  expect_equal(built$data[[2]]$y, raw$y_scaled)
  expect_identical(levels(raw$a), levels(d$a))
  expect_equal(p$layers[[3]]$data, full$layers[[3]]$data)
  expect_equal(p$layers[[4]]$data, full$layers[[4]]$data)
  expect_equal(sum(p$layers[[4]]$data$n_total), nrow(d))
  expect_equal(nrow(built$layout$layout), nlevels(d$b))
  expect_equal(length(unique(built$data[[3]]$PANEL)), nlevels(d$b))
  expect_equal(length(unique(built$data[[4]]$PANEL)), nlevels(d$b))

  empty = flexplot(y_bin ~ a | b, data=d, sample=0, raw.data=FALSE, jitter=FALSE)
  expect_equal(nrow(ggplot2::ggplot_build(empty)$data[[2]]), 0)
  expect_equal(empty$layers[[3]]$data, full$layers[[3]]$data)
  expect_equal(empty$layers[[4]]$data, full$layers[[4]]$data)
})

testthat::test_that("native magnet jitter honors defaults, disabling, and numeric amounts", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  settings = list(NULL, TRUE, FALSE, .12, c(.12, .03), c(0, .04), c(0, 0),
                   1, c(1, .03))
  amounts = list(c(.05, 0), c(.05, 0), c(0, 0), c(.12, 0),
                  c(.12, .03), c(0, .04), c(0, 0), c(1, 0), c(1, .03))
  for (i in seq_along(settings)) {
    p = flexplot(y_bin ~ a, data=small, jitter=settings[[i]])
    built = ggplot2::ggplot_build(p)
    raw = p$layers[[2]]$data
    dx = as.numeric(built$data[[2]]$x) - as.numeric(raw$a)
    dy = built$data[[2]]$y - raw$y_scaled
    width = amounts[[i]][1]
    height = amounts[[i]][2]
    expect_true(all(abs(dx) <= width + 1e-10))
    expect_true(all(abs(dy) <= height + 1e-10))
    if (width > 0) expect_true(any(abs(dx) > 0))
    if (height > 0) expect_true(any(abs(dy) > 0))
    expect_s3_class(p$layers[[2]]$position,
                    if (width + height == 0) "PositionIdentity" else "PositionJitter")
    if (width + height > 0) {
      expect_equal(p$layers[[2]]$position$width, width)
      expect_equal(p$layers[[2]]$position$height, height)
    }
    expect_equal(built$data[[3]]$y, unname(p$layers[[3]]$data$pred_prob))
    expect_equal(built$data[[4]]$label, paste0("N=", p$layers[[4]]$data$n_total))
  }
})

testthat::test_that("Missing outcomes do not affect magnet stacking", {
  
  d = data.frame(
    x=factor(c("A", "A", "A", "B", "B", "B", "B")),
    y=c(0, 1, NA, 0, 0, 1, NA)
  )
  
  prepared = prepare_magnet_data(d, "y", "x")
  
  # Missing outcomes should be excluded.
  expect_equal(nrow(prepared), 5)
  expect_false(anyNA(prepared$y))
  
  # Group sizes should count only observed outcomes.
  expect_equal(unique(prepared$n_total[prepared$x == "A"]), 2)
  expect_equal(unique(prepared$n_total[prepared$x == "B"]), 3)
  
  # Largest group determines the scaling.
  expect_equal(max(prepared$n_total), 3)
  
  # All stack positions should be valid.
  expect_true(all(is.finite(prepared$y_scaled)))
  expect_true(all(prepared$y_scaled >= 0))
  expect_true(all(prepared$y_scaled <= 1))
})

test_that("native magnet scales and frames retain original factor order", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (ordered_x in c(FALSE, TRUE)) {
    for (x_levels in list(c("a", "b"), c("b", "a"))) {
      d = small
      d$a = factor(d$a, levels=x_levels, ordered=ordered_x)
      # Different medians expose upstream summary-based sorting as well as the
      # native helper's fitted-probability sorting.
      a_rows = which(d$a == "a")
      d$y_bin[a_rows] = rep(c(0L, 0L, 1L), length.out=length(a_rows))
      for (formula in list(y_bin ~ a, y_bin ~ a | z, y_bin ~ a | b,
                           y_bin ~ a + b | z, y_bin ~ a + x | z)) {
        p = flexplot(formula, data=d, jitter=FALSE)
        built = ggplot2::ggplot_build(p)
        for (scale in built$layout$panel_scales_x) {
          expect_identical(scale$get_limits(), x_levels)
          expect_identical(scale$get_labels(), x_levels)
        }
        for (index in 2:4) {
          expect_identical(levels(p$layers[[index]]$data$a), x_levels)
          expect_identical(is.ordered(p$layers[[index]]$data$a), ordered_x)
        }
        model_formula = stats::reformulate(all.vars(formula)[-1], response="y_bin")
        fit = stats::glm(model_formula, data=d, family=stats::binomial())
        fitted = p$layers[[3]]$data
        expect_equal(unname(fitted$pred_prob),
                     unname(stats::predict(fit, newdata=fitted, type="response")))
      }
      returned = flexplot(y_bin ~ a | z, data=d, jitter=FALSE, return_data=TRUE)
      expect_identical(levels(returned$a), x_levels)
      expect_identical(is.ordered(returned$a), ordered_x)
    }
  }
})

test_that("native magnet panel positions use the same factor order for every layer", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = factor(d$a, levels=c("b", "a"))
  for (formula in list(y_bin ~ a, y_bin ~ a | z,
                       y_bin ~ a + b | z, y_bin ~ a + x | z)) {
    p = flexplot(formula, data=d, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    layout = built$layout$layout
    expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), c("b", "a"))
    for (i in seq_len(nrow(layout))) {
      panel = layout[i, ]
      for (index in 2:4) {
        source = p$layers[[index]]$data
        rows = rep(TRUE, nrow(source))
        if ("z_binned" %in% names(layout)) rows = source$z_binned == panel$z_binned
        for (category in levels(d$a)) {
          selected = rows & source$a == category
          drawn = built$data[[index]]
          drawn = drawn[drawn$PANEL == panel$PANEL &
                          round(drawn$x) == match(category, levels(d$a)), ]
          expect_equal(nrow(drawn), sum(selected))
          if (index == 2) expect_equal(sort(drawn$y), sort(source$y_scaled[selected]))
          if (index == 3) expect_equal(sort(drawn$y), sort(unname(source$pred_prob[selected])))
          if (index == 4) {
            labels = if (any(selected)) paste0("N=", source$n_total[selected]) else character()
            expect_equal(sort(drawn$label), sort(labels))
          }
        }
      }
    }
    if (length(p$layers[[3]]$mapping$colour) == 0) next
    markers = built$data[[3]]
    labels = built$data[[4]]
    points = built$data[[2]]
    label_keys = paste(labels$PANEL, round(labels$x), labels$colour)
    marker_keys = paste(markers$PANEL, round(markers$x), markers$colour)
    point_keys = paste(points$PANEL, round(points$x), points$colour)
    expect_equal(as.numeric(markers$x), as.numeric(labels$x[match(marker_keys, label_keys)]))
    expect_equal(as.numeric(points$x), as.numeric(labels$x[match(point_keys, label_keys)]))
  }
})
test_that("binary magnets use observed factor levels in their original relative order", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (ordered in c(FALSE, TRUE)) {
    for (declared in list(c(0, 1, 2), c(2, 1, 3, 0, 4))) {
      d = small
      d$y_bin = factor(d$y_bin, levels=declared, ordered=ordered)
      observed = as.character(declared[declared %in% c(0, 1)])
      expected_code = match(as.character(d$y_bin), observed) - 1
      expect_true(is_binary(d$y_bin))
      converted = factor.to.logistic(d, "y_bin", method="logistic")
      expect_equal(converted$y_bin, expected_code)
      model = stats::glm(y_bin ~ a, data=d, family=stats::binomial())
      for (native in c(TRUE, FALSE)) {
        options(flexplot.object_renderer=native)
        expect_no_warning({ p = suppressMessages(flexplot(y_bin ~ a, data=d, jitter=FALSE)) })
        expect_no_warning({ built = ggplot2::ggplot_build(p) })
        expect_no_warning(ggplot2::ggplotGrob(p))
        raw = p$layers[[2]]$data
        expect_equal(raw$y_bin, expected_code)
        expect_equal(nrow(built$data[[2]]), nrow(d))
        expect_true(all(built$data[[2]]$shape == 1))
        expect_true(all(built$data[[3]]$colour == "blue"))
        expect_equal(p$scales$get_scales("y")$labels, observed)
        xlevels = built$layout$panel_scales_x[[1]]$get_limits()
        newdata = data.frame(a=factor(xlevels, levels=levels(d$a)))
        expected = stats::predict(model, newdata=newdata, type="response")
        expect_equal(built$data[[3]]$y,
                       unname(expected[as.integer(built$data[[3]]$x)]), tolerance=1e-7)
        counts = table(d$a)
        max_n = max(counts)
        stack = ave(seq_len(nrow(d)), d$a, expected_code, FUN=seq_along)
        expected_y = ifelse(expected_code == 0, (stack - .5) / (2 * max_n),
                             1 - (stack - .5) / (2 * max_n))
        expect_equal(raw$y_scaled, expected_y)
        expect_equal(sum(p$layers[[4]]$data$n_total), nrow(d))
        expect_equal(nrow(built$data[[4]]), nlevels(d$a))
      }
    }
  }
})
magnet_axis_grob_labels = function(grob) {
  labels = if (inherits(grob, "text")) as.character(grob$label) else character()
  for (child in grob$grobs) labels = c(labels, magnet_axis_grob_labels(child))
  for (child in grob$children) labels = c(labels, magnet_axis_grob_labels(child))
  labels
}

test_that("magnet outcome labels use one scale with both probability endpoints visible", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  dz = data.frame(
    a=factor(c("A", "A", "B", "B")),
    b=factor(c("X", "X", "X", "Y"))
  )
  for (ordered in c(FALSE, TRUE)) {
    for (declared in list(c("X", "Y"), c("Y", "unused", "X"))) {
      d = dz
      d$b = factor(d$b, levels=declared, ordered=ordered)
      labels = declared[declared %in% c("X", "Y")]
      coded = d
      coded$b = match(as.character(d$b), labels) - 1
      for (native in c(TRUE, FALSE)) {
        options(flexplot.object_renderer=native)
        expect_no_message({ p = flexplot(b ~ a, data=d, jitter=FALSE) })
        expect_no_warning({ built = ggplot2::ggplot_build(p) })
        expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
        y = built$layout$panel_params[[1]]$y
        expect_equal(y$get_breaks(), c(0, 1))
        expect_equal(y$get_labels(), labels)
        drawn_labels = character()
        for (axis in grob$grobs[grepl("^axis-l", grob$layout$name)]) {
          drawn_labels = c(drawn_labels, magnet_axis_grob_labels(axis))
        }
        expect_true(all(labels %in% drawn_labels))
        y_scales = 0L
        for (scale in p$scales$scales) {
          if ("y" %in% scale$aesthetics) y_scales = y_scales + 1L
        }
        expect_equal(y_scales, 1L)
        expect_equal(nrow(built$data[[2]]), 4)
        expect_true(all(built$data[[2]]$shape == 1))
        expect_equal(built$data[[1]]$yintercept, .5)
        expect_true(all(built$data[[1]]$linetype == "dashed"))

        # Outcome labels must not change coding, stacks, fits, or x ordering.
        baseline = flexplot(b ~ a, data=coded, jitter=FALSE)
        reference = ggplot2::ggplot_build(baseline)
        expect_equal(built$layout$panel_scales_x[[1]]$get_limits(),
                       reference$layout$panel_scales_x[[1]]$get_limits())
        for (layer in c(2, 3)) {
          expect_equal(built$data[[layer]]$y, reference$data[[layer]]$y)
        }
      }
    }
  }
})
