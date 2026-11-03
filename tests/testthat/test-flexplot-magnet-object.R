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


testthat::test_that("Magnet plots work through flexplot", {
  
  withr::local_options(flexplot.object_renderer=F)
  
  set.seed(123)
  
  d = data.frame(
    x=factor(rep(c("A", "B"), each=100)),
    y=c(
      rbinom(100, 1, 0.2),
      rbinom(100, 1, 0.8)
    )
  )
  
  p = flexplot(y~x, data=d, method="logistic")
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
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
