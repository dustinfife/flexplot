test_that("continuous numeric x rejects categorical distribution geometries in both renderers", {
  withr::local_options(flexplot.object_renderer=TRUE)
  formulas = list(y ~ x, y ~ x + b, y ~ x + z, y ~ x | a, y ~ x | z,
                   y ~ x + b | a, y ~ x + z | a, y ~ I(x^2) | a)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (type in c("boxplot", "violin")) {
      message = paste0("plot.type='", type, "' requires a categorical x variable; 'x' is continuous numeric.")
      for (formula in formulas) {
        expect_error(flexplot(formula, small, plot.type=type, method="lm"), message, fixed=TRUE)
      }
      expect_error(flexplot(y ~ x + b | a, small, plot.type=type, sample=1,
                             suppress_smooth=TRUE, jitter=FALSE), message, fixed=TRUE)
      expect_error(flexplot(y ~ x, small, plot.type=type, plot.string=TRUE),
                     "String-based plotting has been retired", fixed=TRUE)
    }
  }
})

test_that("categorical boxplots and violins retain their actual grouped data and statistics", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (kind in c("factor", "ordered", "character")) {
      d = small
      if (kind == "ordered") d$a = ordered(d$a, levels=c("b", "a"))
      if (kind == "character") d$a = as.character(d$a)
      for (formula in list(y ~ a, y ~ a | b)) {
        for (type in c("boxplot", "violin")) {
          p = flexplot(formula, d, plot.type=type, jitter=FALSE)
          expect_no_warning({ built = ggplot2::ggplot_build(p) })
          expect_s3_class(p$layers[[2]]$geom, if (type == "boxplot") "GeomBoxplot" else "GeomViolin")
          expect_length(p$layers, 2)
          expect_equal(sort(built$data[[1]]$y), sort(d$y))
          layout = built$layout$layout
          levels = built$layout$panel_scales_x[[1]]$get_limits()
          if (kind == "ordered") expect_equal(levels, c("b", "a"))
          for (panel in seq_len(nrow(layout))) {
            eligible = d
            if ("b" %in% names(layout)) eligible = d[d$b == layout$b[panel], ]
            for (category in levels) {
              values = eligible$y[eligible$a == category]
              rows = built$data[[2]]$PANEL == layout$PANEL[panel] &
                as.numeric(built$data[[2]]$x) == match(category, levels)
              geometry = built$data[[2]][rows, ]
              expect_true(nrow(geometry) > 0)
              if (type == "boxplot") {
                expect_equal(nrow(geometry), 1)
                expect_equal(c(geometry$lower, geometry$middle, geometry$upper),
                               unname(stats::quantile(values, c(.25, .5, .75))))
              } else {
                expect_equal(unique(geometry$n), length(values))
              }
            }
          }
        }
      }
    }
  }
})

test_that("existing low-cardinality numeric categories keep their distribution plots", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$category = rep(1:3, length.out=nrow(d))
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (type in c("boxplot", "violin")) {
      p = flexplot(y ~ category, d, plot.type=type, jitter=FALSE)
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      expect_s3_class(p$layers[[2]]$geom, if (type == "boxplot") "GeomBoxplot" else "GeomViolin")
      expect_equal(length(unique(built$data[[2]]$group)), 3)
      expect_equal(sort(built$data[[1]]$y), sort(d$y))
    }
  }
})
