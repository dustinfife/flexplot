test_that("magnet_plot works", {
  set.seed(121)
  vdiffr::expect_doppelganger("magnet plot with numeric binary", magnet_plot(y_bin~a, data=small))
  expect_error(magnet_plot(b~a, data=small), "Your outcome variable must have exactly 2 levels")
  expect_message(magnet_plot(a~b, data=small), "your outcome variable levels")
  vdiffr::expect_doppelganger("magnet plot with categorical binary", magnet_plot(a~b, data=small)%>%suppressMessages())
  expect_error(magnet_plot(a~y, data=small))
})

test_that("check_magnet_errors works", {
  expect_equal(small, check_magnet_errors(small, "y_bin", "a"))
  expect_error(check_magnet_errors(small, "b", "a"))
  expect_equal(unique(check_magnet_errors(small, "a", "b")$a), c(0,1)) %>% suppressMessages()
  expect_message(check_magnet_errors(small, "a", "b"), "your outcome variable levels")
})


# Integration and visual regression tests for magnet plots.
# Run devtools::test(filter="magnet") after copying to tests/testthat/.

magnet_plot_fixture = function() {
  data.frame(
    y = c(0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1),
    group = factor(rep(c("control", "treated"), each=6),
                   levels=c("control", "treated")),
    z = rep(c("low", "high"), 6)
  )
}

test_that("simple magnet plot returns ggplot with scaled dots", {
  d = magnet_plot_fixture()
  p = flexplot(y ~ group, data=d)
  expect_s3_class(p, "ggplot")
  raw = p$layers[[2]]$data
  expect_true("y_scaled" %in% names(raw))
  expect_equal(nrow(raw), nrow(d))
  expect_true(all(raw$y_scaled >= 0 & raw$y_scaled <= 1))
  expect_equal(p$labels$y, "y")
  expect_true(any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1))))
})

test_that("magnet fitted dots equal observed success proportions", {
  d = magnet_plot_fixture()
  p = flexplot(y ~ group, data=d)
  built = ggplot2::ggplot_build(p)
  expected = aggregate(y ~ group, data=d, FUN=mean)$y
  # Find a point layer with exactly one fitted probability per x category.
  candidate = Filter(function(layer) {
    all(c("x", "y") %in% names(layer)) &&
      nrow(layer) == length(expected) &&
      all(is.finite(layer$y)) &&
      all(layer$y >= 0 & layer$y <= 1)
  }, built$data)
  expect_true(length(candidate) >= 1)
  expect_true(any(vapply(candidate, function(layer) {
    isTRUE(all.equal(sort(layer$y), sort(expected), tolerance=1e-8))
  }, logical(1))))
})

test_that("magnet plot renders without error", {
  set.seed(2323)
  d = magnet_plot_fixture()
  p = flexplot(y ~ group, data=d)
  expect_no_error(ggplot2::ggplot_build(p))
  vdiffr::expect_doppelganger("magnet: binary outcome categorical x", p)
})

test_that("continuous x still produces a logistic plot", {
  d = magnet_plot_fixture()
  d$x = seq_len(nrow(d))
  p = flexplot(y ~ x, data=d, method="logistic")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_false("y_scaled" %in% names(p$data))
})

test_that("nonbinary outcome retains ordinary categorical plot", {
  d = magnet_plot_fixture()
  d$score = seq_len(nrow(d))
  p = flexplot(score ~ group, data=d)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_false("y_scaled" %in% names(p$data))
})

test_that("magnet plots support paneling", {
  p = magnet_plot(injury ~ gender | safety, data = tablesaw.injury)
  
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
  
  layout = ggplot2::ggplot_build(p)$layout$layout
  expect_equal(
    nrow(layout),
    length(unique(tablesaw.injury$safety))
  )
})


test_that("magnet plots calculate sample sizes within panels", {
  p = magnet_plot(injury ~ gender | safety, data = tablesaw.injury)
  
  counts = Filter(
    function(layer) inherits(layer$geom, "GeomText"),
    p$layers
  )
  
  expect_length(counts, 1)
  counts = counts[[1]]$data
  
  expected = tablesaw.injury |>
    dplyr::count(safety, gender, name = "n_total")
  
  expect_equal(sum(counts$n_total), nrow(tablesaw.injury))
  expect_equal(sort(counts$n_total), sort(expected$n_total))
})


test_that("magnet plots calculate probabilities within panels", {
  p = magnet_plot(injury ~ gender | safety, data = tablesaw.injury)
  
  predictions = lapply(p$layers, function(layer) layer$data)
  predictions = Filter(
    function(d) "pred_prob" %in% names(d),
    predictions
  )
  
  expect_length(predictions, 1)
  predictions = predictions[[1]]
  
  expect_true(all(is.finite(predictions$pred_prob)))
  expect_true(all(predictions$pred_prob >= 0 & predictions$pred_prob <= 1))
  
  expected_groups = tablesaw.injury |>
    dplyr::distinct(safety, gender)
  
  expect_equal(nrow(predictions), nrow(expected_groups))
})


test_that("magnet plots handle panels with one outcome class", {
  d = data.frame(
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    x = rep(c("A", "B"), 4),
    panel = rep(c("first", "second"), each = 4)
  )
  
  p = magnet_plot(y ~ x | panel, data = d)
  
  predictions = p$layers[[3]]$data
  
  expect_equal(
    sort(predictions$pred_prob),
    c(0, 0, 1, 1)
  )
})


test_that("magnet plots exclude missing panel values", {
  d = tablesaw.injury
  d$safety[1:10] = NA
  
  p = magnet_plot(injury ~ gender | safety, data = d)
  
  counts = p$layers[[4]]$data
  
  expect_equal(
    sum(counts$n_total),
    sum(stats::complete.cases(d[, c("injury", "gender", "safety")]))
  )
})

test_that("flexplot supports ghost lines for magnet plots", {
  p = flexplot(
    injury ~ gender | safety,
    data = tablesaw.injury,
    ghost.line = "red"
  )
  
  expect_s3_class(p, "ggplot")
  
  ghost_lines = Filter(
    function(layer) {
      inherits(layer$geom, "GeomLine") &&
        identical(layer$aes_params$colour, "red")
    },
    p$layers
  )
  
  expect_length(ghost_lines, 1)
  expect_gt(nrow(ghost_lines[[1]]$data), 0)
})


test_that("magnet ghost lines reproduce reference predictions", {
  p = flexplot(
    injury ~ gender | safety,
    data = tablesaw.injury,
    ghost.line = "red"
  )
  
  ghost = Filter(
    function(layer) {
      inherits(layer$geom, "GeomLine") &&
        identical(layer$aes_params$colour, "red")
    },
    p$layers
  )[[1]]$data
  
  fitted = Filter(
    function(layer) {
      "pred_prob" %in% names(layer$data)
    },
    p$layers
  )[[1]]$data
  
  expect_true(all(ghost$pred_prob %in% fitted$pred_prob))
  expect_equal(
    length(unique(ghost$pred_prob)),
    length(unique(ghost[[names(ghost)[1]]]))
  )
})


test_that("magnet plots without ghost lines remain unchanged", {
  p = flexplot(
    injury ~ gender | safety,
    data = tablesaw.injury
  )
  
  ghost_lines = Filter(
    function(layer) {
      inherits(layer$geom, "GeomLine")
    },
    p$layers
  )
  
  expect_length(ghost_lines, 0)
})

