sampling_is_fit = function(layer) {
  inherits(layer$stat, "StatSummary") || inherits(layer$stat, "StatSmooth")
}

sampling_is_prediction = function(layer) {
  is.data.frame(layer$data) && "prediction" %in% names(layer$data)
}

test_that("categorical sampling retains full-data slots, encodings, facets, and summaries", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  for (ordered_group in c(FALSE, TRUE)) {
    d = small
    d$a = ordered(d$a, levels=c("b", "a"))
    d$b = factor(d$b, levels=c("z", "x", "y"), ordered=ordered_group)
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      for (formula in list(y ~ a + b | z, y ~ a + z | b)) {
        group = if ("z" %in% flexplot_axis_given(formula)$axis) "z_binned" else "b"
        full = flexplot(formula, d, sample=Inf, jitter=FALSE)
        full_built = ggplot2::ggplot_build(full)
        fit_layers = which(vapply(full$layers, sampling_is_fit, logical(1)))
        for (size in c(0, 1, 8)) {
          set.seed(123)
          p = flexplot(formula, d, sample=size, jitter=FALSE)
          expect_no_warning({ built = ggplot2::ggplot_build(p) })
          expect_no_warning(ggplot2::ggplotGrob(p))
          expect_equal(nrow(built$data[[1]]), size)
          expect_equal(p$layers[[length(p$layers)]]$data, full$data)
          expect_equal(built$layout$layout, full_built$layout$layout)
          expect_equal(built$layout$panel_scales_x[[1]]$get_limits(),
                         full_built$layout$panel_scales_x[[1]]$get_limits())
          levels = levels(droplevels(full$data[[group]]))
          for (aesthetic in c("colour", "shape", "linetype")) {
            reference = full_built$plot$scales$get_scales(aesthetic)
            actual = built$plot$scales$get_scales(aesthetic)
            if (is.null(reference)) next
            expect_equal(actual$get_limits(), reference$get_limits())
            expect_equal(actual$map(levels), reference$map(levels))
          }
          for (layer in fit_layers) {
            expect_equal(built$data[[layer]], full_built$data[[layer]])
          }
          if (!size) next
          points = built$data[[1]]
          rows = match(points$y, full_built$data[[1]]$y)
          expect_false(anyNA(rows))
          expect_equal(as.numeric(points$x), as.numeric(full_built$data[[1]]$x[rows]))
          expect_equal(points$colour, full_built$data[[1]]$colour[rows])
          expect_equal(points$shape, full_built$data[[1]]$shape[rows])
          expect_equal(points$PANEL, full_built$data[[1]]$PANEL[rows])
          if (size == 1) expect_length(unique(points$colour), 1)
        }
      }
    }
  }
})

test_that("sampled numeric predictors keep full-data group aesthetics and fitted curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (formula in list(y ~ x + b | a, y ~ x + z | a)) {
      full = flexplot(formula, small, method="lm", se=FALSE, jitter=FALSE)
      reference = ggplot2::ggplot_build(full)
      set.seed(123)
      p = flexplot(formula, small, method="lm", se=FALSE, sample=1, jitter=FALSE)
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      expect_equal(nrow(built$data[[1]]), 1)
      row = match(built$data[[1]]$y, reference$data[[1]]$y)
      expect_false(anyNA(row))
      for (aesthetic in c("x", "colour", "shape", "linetype", "PANEL")) {
        expect_equal(built$data[[1]][[aesthetic]], reference$data[[1]][[aesthetic]][row])
      }
      expect_equal(built$data[[2]], reference$data[[2]])
      expect_equal(built$layout$layout, reference$layout$layout)
    }
  }
})

test_that("sampled axes train on eligible observations without adding displayed points", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  d = small
  d$y[1] = 100
  d$y[2] = -1000
  d$x[2] = NA
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (formula in list(y ~ x, y ~ a)) {
      eligible = d[stats::complete.cases(d[all.vars(formula)]), ]
      full = flexplot(formula, d, suppress_smooth=TRUE, jitter=FALSE)
      reference = ggplot2::ggplot_build(full)
      for (size in c(0, 1)) {
        set.seed(123)
        p = flexplot(formula, d, sample=size, suppress_smooth=TRUE, jitter=FALSE)
        expect_no_warning({ built = ggplot2::ggplot_build(p) })
        expect_equal(nrow(built$data[[1]]), size)
        expect_equal(built$layout$panel_params[[1]]$y.range,
                       reference$layout$panel_params[[1]]$y.range)
        expect_lte(min(built$layout$panel_params[[1]]$y.range), min(eligible$y))
        expect_gte(max(built$layout$panel_params[[1]]$y.range), max(eligible$y))
        expect_false(any(vapply(p$layers, sampling_is_fit, logical(1))))
        if (size) expect_true(all(built$data[[1]]$y %in% eligible$y))
        if (identical(formula[[3]], as.name("x"))) {
          expect_equal(built$layout$panel_params[[1]]$x.range,
                         reference$layout$panel_params[[1]]$x.range)
        }
      }
    }
  }
})

test_that("sampling preserves intervals, axis expansion, and explicit limits", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    full = flexplot(y ~ x, small, method="lm", se=TRUE, jitter=FALSE)
    reference = ggplot2::ggplot_build(full)
    p = flexplot(y ~ x, small, method="lm", se=TRUE, sample=1, jitter=FALSE)
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(built$data[[2]], reference$data[[2]])
    bounds = range(c(built$data[[2]]$ymin, built$data[[2]]$ymax))
    expect_lte(min(built$layout$panel_params[[1]]$y.range), bounds[1])
    expect_gte(max(built$layout$panel_params[[1]]$y.range), bounds[2])
    expect_equal(built$layout$panel_params[[1]]$y.range,
                   reference$layout$panel_params[[1]]$y.range)

    expanded = p + ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=.2))
    expanded_full = full + ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=.2))
    expect_equal(ggplot2::ggplot_build(expanded)$layout$panel_params[[1]]$y.range,
                   ggplot2::ggplot_build(expanded_full)$layout$panel_params[[1]]$y.range)
    limited = p + ggplot2::coord_cartesian(ylim=c(-.5, .5), expand=FALSE)
    expect_equal(ggplot2::ggplot_build(limited)$layout$panel_params[[1]]$y.range, c(-.5, .5))
    scale_limited = p + ggplot2::scale_y_continuous(limits=c(-10, 10), expand=ggplot2::expansion(0))
    expect_equal(ggplot2::ggplot_build(scale_limited)$layout$panel_params[[1]]$y.range, c(-10, 10))
  }
})

test_that("categorical sampling retains the existing jitter controls", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    set.seed(123)
    fixed = flexplot(y ~ a + b | z, small, sample=8, jitter=FALSE)
    reference = ggplot2::ggplot_build(fixed)$data[[1]]
    set.seed(123)
    p = flexplot(y ~ a + b | z, small, sample=8, jitter=c(.1, 0))
    expect_no_warning({ points = ggplot2::ggplot_build(p)$data[[1]] })
    expect_equal(sort(points$y), sort(reference$y))
    rows = match(points$y, reference$y)
    dx = as.numeric(points$x) - as.numeric(reference$x[rows])
    expect_true(any(abs(dx) > 0))
    expect_true(all(abs(dx) <= .1 / (nlevels(small$b) + 2)))
  }
})

test_that("magnet sampling and comparison prediction semantics remain intact", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  m = stats::glm(y_bin ~ a + b, data=small, family=stats::binomial())
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    full = flexplot(y_bin ~ a + b, small, jitter=FALSE)
    sampled = flexplot(y_bin ~ a + b, small, sample=1, jitter=FALSE)
    reference = ggplot2::ggplot_build(full)
    built = ggplot2::ggplot_build(sampled)
    expect_equal(nrow(built$data[[2]]), 1)
    expect_equal(built$data[[3]], reference$data[[3]])
    expect_equal(built$layout$panel_params[[1]]$y.range,
                   reference$layout$panel_params[[1]]$y.range)
    for (method in c("observed", "reference")) {
      expected = compare_fits(y_bin ~ a + b, small, m,
                              prediction_method=method, return.preds=TRUE)
      expect_identical(compare_fits(y_bin ~ a + b, small, m, sample=1,
                                     prediction_method=method, return.preds=TRUE), expected)
      p = compare_fits(y_bin ~ a + b, small, m, sample=1,
                        prediction_method=method, jitter=FALSE)
      full = compare_fits(y_bin ~ a + b, small, m, prediction_method=method, jitter=FALSE)
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      reference = ggplot2::ggplot_build(full)
      expect_equal(nrow(built$data[[2]]), 1)
      for (i in setdiff(seq_along(built$data), 2)) expect_equal(built$data[[i]], reference$data[[i]])
    }
  }
})

test_that("ordinary comparison overlays retain their values and groups under sampling", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  cases = list(
    list(formula=y ~ x | b, first=stats::lm(y ~ x + b + z, data=small),
         second=stats::lm(y ~ x, data=small)),
    list(formula=y ~ a + b | z, first=stats::lm(y ~ a + b + z, data=small), second=NULL)
  )
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (method in c("observed", "reference")) {
      for (case in cases) {
        full = compare_fits(case$formula, small, case$first, case$second,
                            prediction_method=method, jitter=FALSE)
        p = compare_fits(case$formula, small, case$first, case$second,
                         prediction_method=method, sample=1, jitter=FALSE)
        expect_no_warning({ built = ggplot2::ggplot_build(p) })
        reference = ggplot2::ggplot_build(full)
        expect_equal(nrow(built$data[[1]]), 1)
        expect_equal(built$layout$layout, reference$layout$layout)
        indices = which(vapply(full$layers, sampling_is_prediction, logical(1)))
        expect_true(length(indices) > 0)
        for (i in indices) {
          expect_equal(p$layers[[i]]$data, full$layers[[i]]$data)
          expect_equal(built$data[[i]], reference$data[[i]])
        }
      }
    }
  }
})
