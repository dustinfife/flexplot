test_that("numeric bivariate renderer returns ggplot layers", {
  p = flexplot_bivariate_object(mtcars, "mpg", "wt", method="lm")
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$geom, "GeomSmooth")
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), nrow(mtcars))
})

test_that("smoothing can be suppressed", {
  p = flexplot_bivariate_object(mtcars, "mpg", "wt", suppress_smooth=TRUE)
  expect_length(p$layers, 1)
})

testthat::test_that("Second-axis variables produce separate fitted curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = flexplot(y ~ x + b | a, data=small, method="lm")
  built = ggplot2::ggplot_build(p)
  
  smooth_idx = which(vapply(
    p$layers,
    function(layer) inherits(layer$stat, "StatSmooth"),
    logical(1)
  ))
  
  testthat::expect_length(smooth_idx, 1)
  
  smooth_data = built$data[[smooth_idx]]
  expect_equal(length(unique(smooth_data$linetype)), nlevels(small$b))
  
  groups_per_panel = tapply(
    smooth_data$group,
    smooth_data$PANEL,
    function(x) length(unique(x))
  )
  
  testthat::expect_true(all(groups_per_panel == length(unique(small$b))))
})

expect_numeric_group_shapes = function(p, group, group_levels) {
  built = ggplot2::ggplot_build(p)
  points = built$data[[1]]
  source = p$layers[[1]]$data
  rows = match(points$x, source$x)
  expect_false(anyNA(rows))
  categories = source[[group]][rows]
  expected_shapes = stats::setNames(c(16, 17, 15), group_levels)
  expect_equal(points$shape, unname(expected_shapes[as.character(categories)]))
  expect_equal(points$colour, built$plot$scales$get_scales("colour")$map(categories))
  expect_equal(points$linetype, built$plot$scales$get_scales("linetype")$map(categories))
  expect_identical(built$plot$scales$get_scales("shape")$get_limits(), group_levels)
  expect_equal(built$plot$labels$shape, group)
  expect_equal(built$plot$labels$colour, group)
  expect_equal(built$plot$labels$linetype, group)
  invisible(built)
}

test_that("grouped numeric point shapes match legacy categories and bins", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (group in c("b", "z")) {
    options(flexplot.object_renderer=TRUE)
    formula = stats::reformulate(paste0("x + ", group, " | a"), response="y")
    p = flexplot(formula, data=small, method="lm", jitter=FALSE,
                 ghost.line="gray", ghost.reference=list(a="a"))
    display_group = if (group == "z") "z_binned" else "b"
    built = expect_numeric_group_shapes(p, display_group, levels(p$data[[display_group]]))
    expect_identical(rlang::as_label(p$mapping$shape), display_group)
    expect_equal(sort(unique(built$data[[1]]$shape)), c(15, 16, 17))
    expect_true(all(built$data[[3]]$colour == "gray"))
    expect_false(p$layers[[3]]$show.legend)

    legends = cowplot::get_plot_component(p, "guide-box", return_all=TRUE)
    if (inherits(legends, "gtable")) legends = list(legends)
    guides = 0L
    for (legend in legends) {
      if (!is.null(legend$layout)) guides = guides + sum(legend$layout$name == "guides")
    }
    expect_equal(guides, 1)

    options(flexplot.object_renderer=FALSE)
    legacy = flexplot(formula, data=small, method="lm", jitter=FALSE,
                      ghost.line="gray", ghost.reference=list(a="a"))
    legacy_built = ggplot2::ggplot_build(legacy)
    columns = c("x", "y", "shape", "colour", "linetype", "group", "PANEL")
    expect_equal(built$data[[1]][columns], legacy_built$data[[1]][columns])
    expect_equal(built$data[[2]]$y, legacy_built$data[[2]]$y)
    expect_equal(built$data[[2]]$group, legacy_built$data[[2]]$group)
  }
})

test_that("numeric plot shapes retain ordered and reordered second-axis levels", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (ordered_group in c(FALSE, TRUE)) {
    for (group_levels in list(c("x", "y", "z"), c("z", "x", "y"))) {
      d = small[rev(seq_len(nrow(small))), ]
      d$b = factor(d$b, levels=group_levels, ordered=ordered_group)
      p = flexplot(y ~ x + b | a, data=d, method="lm", jitter=FALSE)
      expect_numeric_group_shapes(p, "b", group_levels)
      expect_identical(levels(p$data$b), group_levels)
      expect_identical(is.ordered(p$data$b), ordered_group)
    }
  }
})

test_that("sampled numeric plots keep shapes tied to their full-data groups", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (formula in list(y ~ x + b, y ~ x + z, y ~ x + b | a,
                       y ~ x + z | a, y ~ x + b | z)) {
    withr::local_seed(123)
    p = flexplot(formula, data=small, method="lm", jitter=FALSE, sample=1)
    group = if ("b" %in% all.vars(formula)) "b" else "z_binned"
    built = expect_numeric_group_shapes(p, group, levels(p$data[[group]]))
    expect_equal(nrow(built$data[[1]]), 1)
    expect_equal(built$data[[1]]$y, p$layers[[1]]$data$y)
  }
})

test_that("numeric group shape encoding does not omit points above six groups", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small[rep(seq_len(nrow(small)), times=7), ]
  d$b = factor(rep(letters[1:7], each=nrow(small)))
  categorical = flexplot(y ~ x + b | a, data=d, method="lm", jitter=FALSE)
  binned = flexplot(y ~ x + z | a, data=small, method="lm", jitter=FALSE, bins=7)
  for (p in list(categorical, binned)) {
    built = ggplot2::ggplot_build(p)
    expect_null(p$mapping$shape)
    expect_false(anyNA(built$data[[1]]$shape))
    expect_equal(nrow(built$data[[1]]), nrow(p$data))
    expect_equal(length(unique(built$data[[1]]$shape)), 1)
  }
  expect_null(categorical$mapping$linetype)
})
