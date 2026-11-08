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

test_that("magnet ghosts copy the requested fitted curve to every panel", {
  for (reference in c("x", "z")) {
    p = flexplot(y_bin ~ a | b, small, jitter=FALSE, ghost.line="red",
                   ghost.reference=list(b=reference))
    built = ggplot2::ggplot_build(p)
    lines = which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomLine") &&
      identical(layer$aes_params$colour, "red"), logical(1)))
    expect_length(lines, 1)
    line = built$data[[lines]]
    points = built$data[[lines + 1L]]
    ghost = p$layers[[lines]]$data
    panels = built$layout$layout
    destination = panels$PANEL
    expect_setequal(line$PANEL, destination)
    expect_true(any(line$PANEL %in% panels$PANEL[as.character(panels$b) == reference]))
    expect_equal(sort(unique(as.character(ghost$b))), sort(levels(small$b)))
    profile = ghost
    profile$b = factor(reference, levels=levels(small$b))
    expect_equal(ghost$pred_prob, magnet_within_predictions(small, profile, "b"),
                   tolerance=1e-7, scale=1)
    expect_equal(nrow(line), nlevels(small$a) * length(destination))
    for (panel in destination) {
      curve = line[line$PANEL == panel, ]
      expect_equal(length(unique(curve$group)), 1)
      expect_equal(as.numeric(curve$x), seq_len(nlevels(small$a)))
      markers = points[points$PANEL == panel, ]
      expect_equal(curve$y, markers$y[order(markers$x)])
    }
    expect_true(all(line$colour == "red"))
    expect_equal(p$layers[[lines]]$aes_params$linewidth, .8)
    expect_false(p$layers[[lines]]$show.legend)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("default magnet references and sampling reuse the same fitted probabilities", {
  expect_message({
    default = flexplot(y_bin ~ a | b, small, jitter=FALSE, ghost.line="gray")
  }, "didn't specify a reference")
  explicit = flexplot(y_bin ~ a | b, small, jitter=FALSE, ghost.line="gray",
                       ghost.reference=list(b="y"))
  line = which(vapply(default$layers, function(layer) inherits(layer$geom, "GeomLine") &&
    identical(layer$aes_params$colour, "gray"), logical(1)))
  expect_equal(default$layers[[line]]$data, explicit$layers[[line]]$data)
  sampled = withr::with_seed(123, flexplot(y_bin ~ a | b, small, jitter=FALSE, sample=1,
                                            ghost.line="gray", ghost.reference=list(b="y")))
  expect_equal(sampled$layers[[line]]$data, explicit$layers[[line]]$data)
  expect_equal(nrow(sampled$layers[[2]]$data), 1)
  expect_error(flexplot(y_bin ~ a | b, small, ghost.line="gray",
                         ghost.reference=list(b="absent")), "No observations match")
})

test_that("grouped magnet ghosts keep fitted offsets and separate category or bin paths", {
  for (group in c("b", "z")) {
    formula = if (group == "b") y_bin ~ a + b | z else y_bin ~ a + z | b
    reference = if (group == "b") list(z=median(small$z)) else list(b="x")
    p = flexplot(formula, small, jitter=FALSE, ghost.line="gray", ghost.reference=reference)
    built = ggplot2::ggplot_build(p)
    line_index = which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomLine") &&
    identical(layer$aes_params$colour, "gray"), logical(1)))
    ghost = p$layers[[line_index]]$data
    curve = built$data[[line_index]]
    markers = built$data[[line_index + 1L]]
    ordinary = built$data[[3]]
    panels = built$layout$layout
    display_given = if (group == "b") "z_binned" else "b"
    reference_panel = if (group == "b") {
      bins = cut(small$z, quantile(small$z, seq(0, 1, length.out=4)),
                    labels=levels(p$layers[[2]]$data$z_binned), include.lowest=TRUE)
      as.character(bins[which.min(abs(small$z - median(small$z)))])
    } else "x"
    source = panels$PANEL[as.character(panels[[display_given]]) == reference_panel]
    expect_length(source, 1)
    expect_true(any(curve$PANEL == source))
    expect_setequal(curve$PANEL, panels$PANEL)
    original = ordinary[ordinary$PANEL == source, ]
    expect_equal(length(unique(curve$group)), sum(table(original$group) > 1))
    profile = ghost
    if (group == "z") {
      profile$b = factor("x", levels=levels(small$b))
    } else profile$z_binned = factor(reference_panel, levels=levels(ghost$z_binned))
    expect_equal(ghost$pred_prob,
                   magnet_within_predictions(p$layers[[2]]$data, profile, c("b", "z_binned")),
                   tolerance=1e-7, scale=1)
    expect_equal(length(unique(markers$group)), 3)
    for (panel in unique(curve$PANEL)) {
      copy = curve[curve$PANEL == panel, ]
      for (identity in unique(copy$group)) {
        path = copy[copy$group == identity, ]
        matched = match(as.numeric(path$x), as.numeric(original$x))
        expect_false(anyNA(matched))
        fitted = original[matched, ]
        expect_equal(length(unique(fitted$group)), 1)
        expect_gte(nrow(path), 2)
        expect_equal(as.numeric(path$x), as.numeric(fitted$x[order(fitted$x)]))
        expect_equal(path$y, fitted$y[order(fitted$x)], tolerance=1e-10)
      }
    }
    expect_equal(sort(unique(markers$x)), sort(unique(original$x)))
    expect_equal(sort(unique(markers$y)), sort(unique(original$y)))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("magnet ghosts honor a selected second-axis reference without recentering", {
  p = flexplot(y_bin ~ a + b | z, small, jitter=FALSE, ghost.line="gray",
                 ghost.reference=list(z=median(small$z), b="z"))
  built = ggplot2::ggplot_build(p)
  line = which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomLine") &&
    identical(layer$aes_params$colour, "gray"), logical(1)))
  expect_identical(unique(as.character(p$layers[[line]]$data$b)), "z")
  expect_equal(length(unique(built$data[[line]]$group)), 1)
  expect_equal(sort(unique(as.numeric(built$data[[line]]$x))), c(1, 2) + 1/6)
  expect_no_message(ggplot2::ggplotGrob(p))
})

test_that("magnet plots without ghosts preserve observations fits counts and facets", {
  plain = flexplot(y_bin ~ a | b, small, jitter=FALSE)
  ghost = flexplot(y_bin ~ a | b, small, jitter=FALSE, ghost.line="gray",
                    ghost.reference=list(b="x"))
  expect_false(any(vapply(plain$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))))
  expect_equal(length(ghost$layers), length(plain$layers) + 3L)
  plain_built = ggplot2::ggplot_build(plain)
  ghost_built = ggplot2::ggplot_build(ghost)
  expect_equal(plain_built$data, head(ghost_built$data, length(plain$layers)))
  expect_equal(plain_built$layout$layout, ghost_built$layout$layout)
  expect_equal(plain_built$data[[3]]$y[plain_built$data[[3]]$PANEL == 1],
                 ghost_built$data[[3]]$y[ghost_built$data[[3]]$PANEL == 1])
})

test_that("ordinary fitted magnet lines follow their own panel and group markers", {
  for (formula in list(y_bin ~ a | b, y_bin ~ a + b | z, y_bin ~ a + z | b)) {
    reference = if ("z" %in% flexplot_axis_given(formula)$given) list(z=median(small$z)) else list(b="x")
    plain = flexplot(formula, small, jitter=FALSE)
    expect_false(any(vapply(plain$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))))
    p = flexplot(formula, small, jitter=FALSE, ghost.line="gray", ghost.reference=reference)
    built = ggplot2::ggplot_build(p)
    fitted = built$data[[3]]
    if (length(flexplot_axis_given(formula)$axis) == 1) fitted$group = 1L
    lines = built$data[[5]]
    expect_equal(nrow(fitted), nrow(p$layers[[3]]$data))
    keys = interaction(fitted$PANEL, fitted$group, drop=TRUE)
    expected_lines = sum(table(keys)[table(keys) > 1])
    expect_equal(nrow(lines), as.numeric(expected_lines))
    for (panel in unique(lines$PANEL)) {
      local = lines[lines$PANEL == panel, ]
      points = fitted[fitted$PANEL == panel, ]
      for (group in unique(local$group)) {
        path = local[local$group == group, ]
        matched = match(as.numeric(path$x), as.numeric(points$x))
        expect_false(anyNA(matched))
        expect_equal(length(unique(points$group[matched])), 1)
        expect_gte(nrow(path), 2)
        expect_equal(path$y, points$y[matched])
        expect_equal(path$colour, points$colour[matched])
        expect_false(is.unsorted(path$x))
      }
    }
    expect_no_message(ggplot2::ggplotGrob(p))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("default grouped magnet ghosts repeat every attention bin in every safety panel", {
  expect_message({
    p = flexplot(injury ~ gender + attention | safety, tablesaw.injury,
                   jitter=FALSE, ghost.line="red")
  }, "didn't specify a reference")
  built = ggplot2::ggplot_build(p)
  index = which(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomLine") && identical(layer$aes_params$colour, "red")
  }, logical(1)))
  ghost = built$data[[index]]
  panels = built$layout$layout
  expect_setequal(ghost$PANEL, panels$PANEL)
  for (panel in panels$PANEL) {
    curves = ghost[ghost$PANEL == panel, ]
    expect_equal(length(unique(curves$group)), 3)
    expect_equal(sort(as.numeric(table(curves$group))), rep(2, 3))
    expect_true(all(curves$colour == "red"))
  }
  expect_equal(length(unique(p$layers[[index]]$data$attention_binned)), 3)
  expect_no_warning(ggplot2::ggplotGrob(p))
  expect_no_message(ggplot2::ggplotGrob(p))
})
