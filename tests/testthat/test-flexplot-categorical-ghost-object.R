categorical_ghost_index = function(p) {
  for (i in seq_along(p$layers)) {
    if (identical(p$layers[[i]]$aes_params$colour, "gray")) return(i)
  }
  stop("No gray ghost layer")
}

categorical_ghost_expected = function(reference, xlevels, spread, group=NULL) {
  center = if (spread == "quartiles") stats::median else mean
  keys = c("a", group)
  expected = stats::aggregate(reference$y, reference[keys], center)
  names(expected)[ncol(expected)] = "y"
  expected$position = match(as.character(expected$a), xlevels)
  if (is.null(group)) return(expected)
  groups = levels(droplevels(reference[[group]]))
  width = if (spread == "quartiles") .4 else .5
  for (category in unique(expected$a)) {
    rows = which(expected$a == category)
    observed = groups[groups %in% as.character(expected[[group]][rows])]
    index = match(as.character(expected[[group]][rows]), observed)
    expected$position[rows] = expected$position[rows] + width * ((index - .5) / length(observed) - .5)
  }
  expected
}

expect_categorical_ghost_coordinates = function(p, expected, panels, group=NULL) {
  index = categorical_ghost_index(p)
  expect_s3_class(p$layers[[index]]$geom, "GeomLine")
  expect_false(p$layers[[index]]$inherit.aes)
  expect_false(p$layers[[index]]$show.legend)
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  ghost = built$data[[index]]
  expect_equal(length(unique(ghost$PANEL)), panels)
  expect_equal(nrow(ghost), nrow(expected) * panels)
  expect_true(all(ghost$colour == "gray"))
  expect_true(all(is.na(ghost$alpha)))
  for (panel in unique(ghost$PANEL)) {
    block = ghost[ghost$PANEL == panel, ]
    rows = match(round(block$x, 10), round(expected$position, 10))
    expect_false(anyNA(rows))
    expect_equal(block$y, expected$y[rows])
    if (is.null(group)) {
      expect_equal(length(unique(block$group)), 1)
    } else {
      scale = built$plot$scales$get_scales("linetype")
      expect_equal(block$linetype, scale$map(expected[[group]][rows]))
      for (id in unique(block$group)) {
        expect_equal(length(unique(expected[[group]][rows[block$group == id]])), 1)
      }
    }
  }
  invisible(built)
}

test_that("categorical ghosts repeat legacy medians or means without duplicate summaries", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (spread in c("quartiles", "stdev", "sterr")) {
    options(flexplot.object_renderer=TRUE)
    base = flexplot(y ~ a | b, small, spread=spread, jitter=FALSE)
    p = flexplot(y ~ a | b, small, spread=spread, jitter=FALSE,
                 ghost.line="gray", ghost.reference=list(b="x"))
    reference = small[small$b == "x", ]
    expected = categorical_ghost_expected(reference, levels(base$data$a), spread)
    built = expect_categorical_ghost_coordinates(p, expected, panels=3)
    original = ggplot2::ggplot_build(base)
    expect_length(p$layers, length(base$layers) + 1)
    for (i in seq_along(base$layers)) expect_equal(built$data[[i]], original$data[[i]])
    if (spread == "sterr") next # Legacy error bars use a deprecated size aesthetic.
    options(flexplot.object_renderer=FALSE)
    legacy = flexplot(y ~ a | b, small, spread=spread, jitter=FALSE,
                      ghost.line="gray", ghost.reference=list(b="x"))
    old = ggplot2::ggplot_build(legacy)$data[[categorical_ghost_index(legacy)]]
    columns = c("x", "y", "PANEL", "colour", "linewidth", "linetype", "alpha")
    expect_equal(built$data[[categorical_ghost_index(p)]][columns], old[columns])
  }
})

test_that("numeric conditioning references select full bins and retain true grouped identities", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (ordered_group in c(FALSE, TRUE)) {
    d = small
    d$a = ordered(d$a, levels=c("b", "a"))
    d$b = factor(d$b, levels=c("z", "x", "y"), ordered=ordered_group)
    breaks = stats::quantile(d$z, seq(0, 1, length.out=4))
    bin = cut(d$z, breaks, include.lowest=TRUE, labels=FALSE)
    selected = cut(stats::median(d$z), breaks, include.lowest=TRUE, labels=FALSE)
    reference = d[bin == selected, ]
    for (spread in c("quartiles", "stdev", "sterr")) {
      base = flexplot(y ~ a + b | z, d, spread=spread, jitter=FALSE)
      p = flexplot(y ~ a + b | z, d, spread=spread, jitter=FALSE,
                   ghost.line="gray", ghost.reference=list(z=median(d$z)))
      expected = categorical_ghost_expected(reference, levels(base$data$a), spread, "b")
      built = expect_categorical_ghost_coordinates(p, expected, 3, "b")
      original = ggplot2::ggplot_build(base)
      expect_equal(built$layout$layout, original$layout$layout)
      for (i in seq_along(base$layers)) expect_equal(built$data[[i]], original$data[[i]])
    }
  }
})

test_that("selected second-axis ghosts keep their reference-panel dodge slots", {
  withr::local_options(flexplot.object_renderer=TRUE)
  p = flexplot(y ~ a + b | z, small, jitter=FALSE, ghost.line="gray",
               ghost.reference=list(z=median(small$z), b=c("x", "z")))
  edges = stats::quantile(small$z, seq(0, 1, length.out=4))
  bins = cut(small$z, edges, include.lowest=TRUE, labels=FALSE)
  selected = cut(stats::median(small$z), edges, include.lowest=TRUE, labels=FALSE)
  expected = categorical_ghost_expected(small[bins == selected, ], levels(p$data$a), "quartiles", "b")
  expected = expected[expected$b %in% c("x", "z"), ]
  expect_categorical_ghost_coordinates(p, expected, 3, "b")
})

test_that("binned numeric second-axis ghosts retain separate reference curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  p = flexplot(y ~ a + z | b, small, jitter=FALSE, ghost.line="gray",
               ghost.reference=list(b="x"))
  reference = p$data[p$data$b == "x", ]
  expected = categorical_ghost_expected(reference, levels(p$data$a), "quartiles", "z_binned")
  expect_categorical_ghost_coordinates(p, expected, 3, "z_binned")

  selected = flexplot(y ~ a + z | b, small, jitter=FALSE, ghost.line="gray",
                      ghost.reference=list(b="x", z=median(small$z)))
  edges = stats::quantile(small$z, seq(0, 1, length.out=4))
  bin = cut(stats::median(small$z), edges, include.lowest=TRUE, labels=FALSE)
  expected = expected[expected$z_binned == levels(reference$z_binned)[bin], ]
  expect_categorical_ghost_coordinates(selected, expected, 3, "z_binned")
})

test_that("categorical ghost statistics and facets are independent of raw sampling", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  d = small
  d$condition = factor(d$y_bin, levels=c(1, 0))
  full = flexplot(y ~ a | b + condition, d, ghost.line="gray",
                  ghost.reference=list(b="x", condition="1"), jitter=FALSE)
  reference = d[d$b == "x" & d$condition == "1", ]
  expected = categorical_ghost_expected(reference, levels(full$data$a), "quartiles")
  original = expect_categorical_ghost_coordinates(full, expected, 6)
  for (size in c(0, 1, 8)) {
    set.seed(123)
    p = flexplot(y ~ a | b + condition, d, ghost.line="gray",
                 ghost.reference=list(b="x", condition="1"), jitter=FALSE, sample=size)
    built = expect_categorical_ghost_coordinates(p, expected, 6)
    expect_equal(nrow(built$data[[1]]), size)
    expect_equal(built$layout$layout, original$layout$layout)
    expect_equal(p$layers[[categorical_ghost_index(p)]]$data,
                   full$layers[[categorical_ghost_index(full)]]$data)
    expect_equal(built$layout$panel_params[[1]]$y.range, original$layout$panel_params[[1]]$y.range)
  }
})

test_that("reference subsets cannot drop or reorder categorical x positions", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = ordered(rep(c("first", "middle", "last"), length.out=nrow(d)),
                 levels=c("first", "last", "middle"))
  d$panel = factor(rep(c("reference", "other"), length.out=nrow(d)), levels=c("other", "reference"))
  d = d[!(d$a == "first" & d$panel == "reference"), ]
  p = flexplot(y ~ a | panel, d, ghost.line="gray", ghost.reference=list(panel="reference"), jitter=FALSE)
  expected = categorical_ghost_expected(d[d$panel == "reference", ], levels(d$a), "quartiles")
  built = expect_categorical_ghost_coordinates(p, expected, 2)
  expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
  ghost = built$data[[categorical_ghost_index(p)]]
  expect_setequal(as.numeric(ghost$x), c(2, 3))
  expect_equal(nrow(p$layers[[categorical_ghost_index(p)]]$data), 2)
})

test_that("ghost coordinate fields cannot become conditioning variables", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (condition in c("x", "y", ".ghost_group", ".ghost_x")) {
    d = small
    d$response = d$y
    d[[condition]] = d$b
    formula = stats::as.formula(paste("response ~ a |", condition))
    p = flexplot(formula, d, ghost.line="gray",
                 ghost.reference=stats::setNames(list("x"), condition), jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_equal(nrow(built$layout$layout), 3)
    expect_equal(nrow(built$data[[categorical_ghost_index(p)]]), 6)
    expect_equal(length(unique(built$data[[categorical_ghost_index(p)]]$PANEL)), 3)
    expect_equal(as.character(built$layout$layout[[condition]]), levels(d$b))
  }
})

test_that("categorical ghosts reject ambiguous reference or geometry requests", {
  withr::local_options(flexplot.object_renderer=TRUE)
  expect_error(flexplot(y ~ a | b, small, ghost.line="gray", ghost.reference=list(b=c("x", "y"))),
                 "one reference category or bin per conditioning variable", fixed=TRUE)
  for (type in c("boxplot", "violin", "line")) {
    expect_error(flexplot(y ~ a | b, small, plot.type=type, ghost.line="gray", ghost.reference=list(b="x")),
                   "Categorical ghost summaries require a summary plot", fixed=TRUE)
  }
  expect_error(flexplot(y ~ a | b, small, ghost.line="gray", ghost.reference=list(b="unknown")),
                 "No observations match the ghost reference", fixed=TRUE)
})
