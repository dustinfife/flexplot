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


# Known public native magnet gaps, intentionally excluded from support claims:
# - alpha, raw.data, sample, and jitter are not forwarded by the dispatcher.
# - second-axis groups are absent from fitted models and visual mappings.
# These need production fixes and integration coverage when implemented.
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
