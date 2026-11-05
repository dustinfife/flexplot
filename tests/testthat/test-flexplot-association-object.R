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

test_that("native association bars show relative deviations from independence", {
  withr::local_options(flexplot.object_renderer=TRUE)

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
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small[!(small$a == "a" & small$b == "x"), ]
  d$b[1] = NA
  p = flexplot(b ~ a, data=d)
  expected = association_expected_cells(d, c("a", "b"))
  expect_equal(nrow(p$data), nlevels(d$a) * nlevels(d$b))
  expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
  expect_equal(p$data$Proportion[p$data$a == "a" & p$data$b == "x"], -1)
})

test_that("association conditioning uses one additive independence model", {
  withr::local_options(flexplot.object_renderer=TRUE)

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
  withr::local_options(flexplot.object_renderer=TRUE)

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
})

test_that("native categorical dispatch separates binary, association, and univariate plots", {
  withr::local_options(flexplot.object_renderer=TRUE)

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

test_that("native association bars and facets match the legacy renderer", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  d$g = factor(d$y_bin)
  d$h = factor(rep(c("P", "Q"), length.out=nrow(d)))
  for (formula in list(b ~ a, b ~ a | g, b ~ a | g + h)) {
    options(flexplot.object_renderer=TRUE)
    native = flexplot(formula, data=d)
    options(flexplot.object_renderer=FALSE)
    legacy = flexplot(formula, data=d)
    native_built = ggplot2::ggplot_build(native)
    legacy_built = ggplot2::ggplot_build(legacy)
    columns = c("x", "y", "ymin", "ymax", "xmin", "xmax", "fill", "group", "PANEL")
    expect_equal(native_built$data[[1]][columns], legacy_built$data[[1]][columns])
    expect_equal(native_built$layout$layout, legacy_built$layout$layout)
    expect_equal(native$data$Proportion, legacy$data$Proportion)
    expect_equal(native_built$plot$labels[c("x", "y", "fill")],
                 legacy_built$plot$labels[c("x", "y", "fill")])
  }
})

test_that("association second-axis requests are explicitly unsupported", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$g = factor(d$y_bin)
  expect_error(flexplot(b ~ a + g, data=d),
                "Association plots with a second-axis predictor are not supported", fixed=TRUE)
})

test_that("association tables exclude unused outcomes, predictors, and conditioning levels", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (ordered in c(FALSE, TRUE)) {
    for (unused in list("b", "a", c("a", "b", "g"))) {
      d = small
      d$a = factor(d$a, levels=c("b", "a"), ordered=ordered)
      d$b = factor(d$b, levels=c("z", "x", "y"), ordered=ordered)
      d$g = factor(d$y_bin, levels=c(1, 0), ordered=ordered)
      for (variable in unused) {
        d[[variable]] = factor(d[[variable]],
                                levels=append(levels(d[[variable]]), "unused", after=1),
                                ordered=ordered)
      }
      formula = if ("g" %in% unused) b ~ a | g else b ~ a
      variables = if ("g" %in% unused) c("a", "g", "b") else c("a", "b")
      clean = droplevels(d)
      expected = association_expected_cells(clean, variables)
      built_plots = list()
      for (native in c(TRUE, FALSE)) {
        options(flexplot.object_renderer=native)
        expect_no_warning({ p = flexplot(formula, data=d) })
        expect_no_warning({ built = ggplot2::ggplot_build(p) })
        expect_no_warning(ggplot2::ggplotGrob(p))
        expect_equal(nrow(p$data), nrow(expected))
        expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
        expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(clean$a))
        expect_equal(built$plot$scales$get_scales("fill")$get_limits(), levels(clean$b))
        for (variable in variables) {
          expect_identical(levels(p$data[[variable]]), levels(clean[[variable]]))
          expect_false(any(as.character(p$data[[variable]]) == "unused"))
        }
        if ("g" %in% unused) {
          expect_equal(as.character(built$layout$layout$g), levels(clean$g))
        }
        expect_equal(nrow(built$data[[1]]), nrow(expected))
        built_plots[[as.character(native)]] = built$data[[1]]
      }
      columns = c("x", "y", "fill", "group", "PANEL")
      expect_equal(built_plots[["TRUE"]][columns], built_plots[["FALSE"]][columns])
    }
  }
})

test_that("dropping unused association levels retains legitimate zero cells", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small[!(small$a == "a" & small$b == "x"), ]
  d$a = factor(d$a, levels=c("b", "unused", "a"))
  d$b = factor(d$b, levels=c("z", "unused", "x", "y"))
  expected = association_expected_cells(droplevels(d), c("a", "b"))
  expect_equal(expected$Freq[expected$a == "a" & expected$b == "x"], 0)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    expect_no_warning({ p = flexplot(b ~ a, data=d) })
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(nrow(p$data), 6)
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(p$data$Proportion[p$data$a == "a" & p$data$b == "x"], -1)
    expect_true(all(is.finite(built$data[[1]]$y)))
    expect_equal(nrow(built$data[[1]]), 6)
  }
})

test_that("three observed outcome categories remain association plots", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$category = factor(as.integer(d$b) - 1, levels=c(2, 1, 3, 0), ordered=TRUE)
  expect_false(is_binary(d$category))
  expect_identical(factor.to.logistic(d, "category", method="logistic"), d)
  expected = association_expected_cells(droplevels(d), c("a", "category"))
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    p = flexplot(category ~ a, data=d)
    built = ggplot2::ggplot_build(p)
    expect_length(p$layers, 1)
    expect_s3_class(p$layers[[1]]$geom, "GeomBar")
    expect_equal(levels(p$data$category), c("2", "1", "0"))
    expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
    expect_equal(length(unique(built$data[[1]]$fill)), 3)
  }
})

test_that("association tables handle a single observed predictor or conditioning level", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$g = factor(rep("observed", nrow(d)), levels=c("unused", "observed"))
  one_x = d[d$a == "a", ]
  for (single_x in c(FALSE, TRUE)) {
    original = if (single_x) one_x else d
    expected = association_expected_cells(droplevels(original), c("a", "g", "b"))
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      expect_no_warning({ p = flexplot(b ~ a | g, data=original) })
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      expect_equal(nrow(p$data), nrow(expected))
      expect_equal(p$data$Proportion, expected$Proportion, tolerance=1e-7)
      expect_equal(as.character(built$layout$layout$g), "observed")
      expect_equal(nrow(built$data[[1]]), nrow(expected))
      if (single_x) expect_equal(p$data$Proportion, rep(0, 3), tolerance=1e-7)
    }
  }
})
