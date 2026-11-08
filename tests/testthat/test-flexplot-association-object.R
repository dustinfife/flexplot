association_expected_cells = function(data, variables) {
  data = data[stats::complete.cases(data[variables]), , drop=FALSE]
  cells = as.data.frame(table(data[variables]))
  expected = rep(nrow(data), nrow(cells))
  for (variable in variables) {
    margins = prop.table(table(data[[variable]]))
    expected = expected * as.numeric(margins[as.character(cells[[variable]])])
  }
  cells$Proportion = (cells$Freq - expected) / expected
  cells
}

test_that("association aggregation retains displayed numeric and categorical facet keys", {
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  d$g = ordered(d$y_bin, levels=c(1, 0))
  reference = d
  for (variable in c("x", "z")) {
    # Use existing display-bin conventions; expected frequencies below are
    # calculated independently from products of the observed marginal counts.
    reference[[paste0(variable, "_binned")]] = bin.me(variable, d, bins=3)
  }
  for (formula in list(b ~ a | x, b ~ a | g, b ~ a | x + g,
                       b ~ a | g + x, b ~ a | x + z)) {
    given = flexplot_axis_given(formula)$given
    display_given = ifelse(given %in% c("x", "z"), paste0(given, "_binned"), given)
    expected = association_expected_cells(reference, c("a", display_given, "b"))
    expect_no_warning({ p = flexplot(formula, data=d) })
    expect_no_error({ built = ggplot2::ggplot_build(p) })
    expect_true(all(display_given %in% names(p$data)))
    expect_equal(nrow(p$data), nrow(expected))
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
    expect_equal(built$plot$scales$get_scales("fill")$get_limits(), levels(d$b))
    expect_true(is.ordered(p$data$a))
    expect_true(is.ordered(p$data$b))
    if ("g" %in% given) {
      expect_true(is.ordered(p$data$g))
      expect_identical(levels(p$data$g), levels(d$g))
    }
    for (i in seq_along(display_given)) {
      variable = display_given[i]
      levels = levels(reference[[variable]])
      if (i == 2 && given[i] %in% c("x", "z")) levels = rev(levels)
      expect_identical(levels(p$data[[variable]]), levels)
      expect_equal(unique(as.character(built$layout$layout[[variable]])), levels)
    }
    expect_equal(nrow(built$layout$layout),
                   prod(vapply(reference[display_given], nlevels, integer(1))))
    for (i in seq_len(nrow(built$layout$layout))) {
      panel = built$layout$layout[i, ]
      keep = rep(TRUE, nrow(expected))
      for (variable in display_given) {
        keep = keep & as.character(expected[[variable]]) == as.character(panel[[variable]])
      }
      bars = built$data[[1]][built$data[[1]]$PANEL == panel$PANEL, ]
      expect_equal(sort(bars$y), sort(expected$Proportion[keep]), tolerance=1e-7)
      expect_equal(nrow(bars), nlevels(d$a) * nlevels(d$b))
    }
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("numeric-conditioned associations retain zero cells and custom bin order", {
  d = small[!(small$a == "a" & small$b == "x"), ]
  d$a = factor(d$a, levels=c("b", "unused", "a"))
  d$b = factor(d$b, levels=c("z", "unused", "x", "y"))
  labels = c("low", "middle", "high")
  breaks = c(min(d$x), -.5, .5, max(d$x))
  reference = droplevels(d)
  reference$x_binned = cut(d$x, breaks=breaks, labels=labels, include.lowest=TRUE)
  expected = association_expected_cells(reference, c("a", "x_binned", "b"))
  p = flexplot(b ~ a | x, d, breaks=list(x=c(-.5, .5)), labels=list(labels))
  expect_no_error({ built = ggplot2::ggplot_build(p) })
  expect_identical(levels(p$data$x_binned), labels)
  expect_equal(as.character(built$layout$layout$x_binned), labels)
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_true(any(expected$Freq == 0))
  expect_equal(p$data$Proportion[expected$Freq == 0], rep(-1, sum(expected$Freq == 0)))
  expect_equal(nrow(p$data), 2 * 3 * 3)
  expect_false("unused" %in% as.character(p$data$a))
  expect_false("unused" %in% as.character(p$data$b))
})

test_that("native association bars show relative deviations from independence", {

  p = flexplot(b ~ a, data=small)
  built = ggplot2::ggplot_build(p)
  expected = association_expected_cells(small, c("a", "b"))
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")
  expect_s3_class(p$layers[[1]]$stat, "StatIdentity")
  expect_s3_class(p$layers[[1]]$position, "PositionDodge")
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(built$data[[1]]$y, expected$Proportion, tolerance=1e-7)
  expect_true(any(built$data[[1]]$y < 0))
  expect_true(any(built$data[[1]]$y > 0))
  expect_equal(length(unique(built$data[[1]]$fill)), nlevels(small$b))
  expect_equal(length(unique(built$data[[1]]$group)), nrow(expected))
  expect_equal(built$data[[1]]$fill,
               built$plot$scales$get_scales("fill")$map(expected$b))
  expect_equal(built$plot$labels$x, "a")
  expect_equal(built$plot$labels$y, "Proportion")
  expect_equal(built$plot$labels$fill, "b")
})

test_that("association preparation keeps empty cells and excludes missing observations", {

  d = small[!(small$a == "a" & small$b == "x"), ]
  d$b[1] = NA
  p = flexplot(b ~ a, data=d)
  expected = association_expected_cells(d, c("a", "b"))
  expect_equal(nrow(p$data), nlevels(d$a) * nlevels(d$b))
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(p$data$Proportion[p$data$a == "a" & p$data$b == "x"], -1)
})

test_that("association facets retain independent deviations despite missing data and sampling", {
  d = small
  d$g = factor(d$y_bin, levels=c(1, 0))
  d = d[!(d$a == "a" & d$b == "x" & d$g == "1"), ]
  d$a[1] = NA
  d$b[2] = NA
  d$g[3] = NA
  expected = association_expected_cells(d, c("a", "g", "b"))
  expect_true(any(expected$Freq == 0))
  for (sample in c(Inf, 1)) {
    p = flexplot(b ~ a | g, d, sample=sample)
    built = ggplot2::ggplot_build(p)
    expect_s3_class(p$layers[[1]]$geom, "GeomBar")
    expect_s3_class(p$layers[[1]]$stat, "StatIdentity")
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(nrow(built$data[[1]]), nrow(expected))
    expect_equal(p$data$Proportion[expected$Freq == 0], rep(-1, sum(expected$Freq == 0)))
    panels = built$layout$layout
    bars = built$data[[1]]
    expected_panel = panels$PANEL[match(expected$g, panels$g)]
    expected_color = built$plot$scales$get_scales("fill")$map(expected$b)
    expected_x = match(expected$a, built$layout$panel_scales_x[[1]]$get_limits())
    rows = match(paste(bars$PANEL, round(bars$x), bars$fill),
                   paste(expected_panel, expected_x, expected_color))
    expect_setequal(rows, seq_len(nrow(expected)))
    expect_equal(bars$y, expected$Proportion[rows], tolerance=1e-7)
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits()[
      round(bars$x)], as.character(expected$a[rows]))
  }
})

test_that("association conditioning uses one additive independence model", {

  d = small
  d$g = factor(d$y_bin)
  d$h = factor(rep(c("P", "Q"), length.out=nrow(d)))
  p = flexplot(b ~ a | g + h, data=d)
  built = ggplot2::ggplot_build(p)
  expected = association_expected_cells(d, c("a", "g", "h", "b"))
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(nrow(built$layout$layout), nlevels(d$g) * nlevels(d$h))

  for (i in seq_len(nrow(built$layout$layout))) {
    panel = built$layout$layout[i, ]
    bars = built$data[[1]][built$data[[1]]$PANEL == panel$PANEL, ]
    values = expected$Proportion[expected$g == panel$g & expected$h == panel$h]
    expect_equal(sort(bars$y), sort(values), tolerance=1e-7)
    expect_equal(length(unique(bars$fill)), nlevels(d$b))
    expect_equal(nrow(bars), nlevels(d$a) * nlevels(d$b))
  }
})

test_that("association plots preserve ordered levels on axes, fills, and facets", {

  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  d$g = ordered(d$y_bin, levels=c(1, 0))
  p = flexplot(b ~ a | g, data=d)
  built = ggplot2::ggplot_build(p)
  for (variable in c("a", "b", "g")) {
    expect_true(is.ordered(p$data[[variable]]))
    expect_identical(levels(p$data[[variable]]), levels(d[[variable]]))
  }
  expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
  expect_equal(built$plot$scales$get_scales("fill")$get_limits(), levels(d$b))
  expect_equal(as.character(built$layout$layout$g), levels(d$g))
  expected = association_expected_cells(d, c("a", "g", "b"))
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(sort(built$data[[1]]$y), sort(expected$Proportion), tolerance=1e-7)
})

test_that("native categorical dispatch separates binary, association, and univariate plots", {

  d = small
  d$binary = factor(d$y_bin, levels=c(0, 1), labels=c("No", "Yes"))
  for (method in list(NULL, "lm")) {
    p = flexplot(binary ~ a, data=d, method=method)
    expect_length(p$layers, 4)
    expect_s3_class(p$layers[[1]]$geom, "GeomHline")
    expect_s3_class(p$layers[[3]]$geom, "GeomPoint")
    expect_true("pred_prob" %in% names(p$layers[[3]]$data))
    fit = stats::glm(binary ~ a, data=d, family=stats::binomial())
    expect_equal(unname(p$layers[[3]]$data$pred_prob),
                 unname(stats::predict(fit, newdata=p$layers[[3]]$data, type="response")))
  }
  d$b = as.character(d$b)
  p = flexplot(b ~ a, data=d)
  expect_s3_class(p$layers[[1]]$stat, "StatIdentity")
  expect_identical(rlang::as_label(p$mapping$fill), "b")
  distribution = flexplot(b ~ 1, data=d)
  expect_s3_class(distribution$layers[[1]]$stat, "StatCount")
})

test_that("association second-axis requests are explicitly unsupported", {

  d = small
  d$g = factor(d$y_bin)
  expect_error(flexplot(b ~ a + g, data=d),
                "Association plots with a second-axis predictor are not supported", fixed=TRUE)
})

test_that("association tables exclude unused levels while preserving observed order", {
  for (ordered in c(FALSE, TRUE)) {
    d = small
    d$a = factor(d$a, levels=c("b", "unused", "a"), ordered=ordered)
    d$b = factor(d$b, levels=c("z", "unused", "x", "y"), ordered=ordered)
    d$g = factor(d$y_bin, levels=c(1, "unused", 0), ordered=ordered)
    clean = droplevels(d)
    expected = association_expected_cells(clean, c("a", "g", "b"))
    expect_no_warning({ p = flexplot(b ~ a | g, data=d) })
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(nrow(built$data[[1]]), nrow(expected))
    for (variable in c("a", "b", "g")) {
      expect_identical(levels(p$data[[variable]]), levels(clean[[variable]]))
      expect_identical(is.ordered(p$data[[variable]]), ordered)
    }
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(clean$a))
    expect_equal(built$plot$scales$get_scales("fill")$get_limits(), levels(clean$b))
    expect_equal(as.character(built$layout$layout$g), levels(clean$g))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("dropping unused association levels retains legitimate zero cells", {
  d = small[!(small$a == "a" & small$b == "x"), ]
  d$a = factor(d$a, levels=c("b", "unused", "a"))
  d$b = factor(d$b, levels=c("z", "unused", "x", "y"))
  expected = association_expected_cells(droplevels(d), c("a", "b"))
  expect_equal(expected$Freq[expected$a == "a" & expected$b == "x"], 0)
  expect_no_warning({ p = flexplot(b ~ a, data=d) })
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_equal(nrow(p$data), 6)
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(p$data$Proportion[p$data$a == "a" & p$data$b == "x"], -1)
  expect_true(all(is.finite(built$data[[1]]$y)))
  expect_equal(nrow(built$data[[1]]), 6)
})

test_that("three observed outcome categories remain association plots", {
  d = small
  d$category = factor(as.integer(d$b) - 1, levels=c(2, 1, 3, 0), ordered=TRUE)
  expect_false(is_binary(d$category))
  expect_identical(factor.to.logistic(d, "category", method="logistic"), d)
  expected = association_expected_cells(droplevels(d), c("a", "category"))
  p = flexplot(category ~ a, data=d)
  built = ggplot2::ggplot_build(p)
  expect_length(p$layers, 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomBar")
  expect_equal(levels(p$data$category), c("2", "1", "0"))
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(length(unique(built$data[[1]]$fill)), 3)
})

test_that("association tables handle a single observed predictor or conditioning level", {
  d = small
  d$g = factor(rep("observed", nrow(d)), levels=c("unused", "observed"))
  one_x = d[d$a == "a", ]
  for (single_x in c(FALSE, TRUE)) {
    original = if (single_x) one_x else d
    expected = association_expected_cells(droplevels(original), c("a", "g", "b"))
    expect_no_warning({ p = flexplot(b ~ a | g, data=original) })
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(nrow(p$data), nrow(expected))
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(as.character(built$layout$layout$g), "observed")
    expect_equal(nrow(built$data[[1]]), nrow(expected))
    if (single_x) expect_equal(p$data$Proportion, rep(0, 3), tolerance=1e-7)
  }
})
