
testthat::test_that(
  "Ghost line repeats the reference fit across facets", {
    
    d = data.frame(
      x=rep(1:10, 2),
      group=factor(rep(c("A", "B"), each=10)),
      y=c(
        2 * (1:10) + 3,
        4 * (1:10) + 10
      )
    )
    
    withr::local_options(flexplot.object_renderer=TRUE)
    
    p = flexplot(
      y ~ x | group,
      data=d,
      ghost.line="gray",
      ghost.reference=list(group="A"),
      method="lm"
    )
    
    testthat::expect_s3_class(p, "ggplot")
    
    built = ggplot2::ggplot_build(p)
    
    ghost_idx = which(vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomLine") &&
          identical(layer$aes_params$colour, "gray")
      },
      logical(1)
    ))
    
    testthat::expect_length(ghost_idx, 1)
    
    ghost_data = built$data[[ghost_idx]]
    
    testthat::expect_equal(
      length(unique(ghost_data$PANEL)),
      2
    )
    
    testthat::expect_equal(
      unique(round(ghost_data$y - 2 * ghost_data$x, 6)),
      3
    )
  }
)


testthat::test_that(
  "Ghost lines work with multiple conditioning variables", {
    
    d = expand.grid(
      x=1:10,
      group=factor(c("A", "B")),
      treatment=factor(c("Control", "Treatment"))
    )
    
    d$y = with(d,
               2*x +
                 ifelse(group == "B", 10, 0) +
                 ifelse(treatment == "Treatment", 20, 0)
    )
    
    withr::local_options(flexplot.object_renderer=TRUE)
    
    p = flexplot(
      y ~ x | group + treatment,
      data=d,
      ghost.line="gray",
      ghost.reference=list(
        group="A",
        treatment="Control"
      ),
      method="lm"
    )
    
    testthat::expect_s3_class(p, "ggplot")
    
    built = ggplot2::ggplot_build(p)
    
    ghost_idx = which(vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomLine") &&
          identical(layer$aes_params$colour, "gray")
      },
      logical(1)
    ))
    
    testthat::expect_length(ghost_idx, 1)
    
    ghost_data = built$data[[ghost_idx]]
    
    # Four panels: A/Control, B/Control,
    # A/Treatment, B/Treatment
    testthat::expect_equal(
      length(unique(ghost_data$PANEL)),
      4
    )
    
    # The reference relationship is y = 2*x.
    # It should be identical in all four panels.
    for (panel in unique(ghost_data$PANEL)) {
      
      panel_data = ghost_data[
        ghost_data$PANEL == panel,
        ,
        drop=FALSE
      ]
      
      testthat::expect_equal(
        panel_data$y,
        2 * panel_data$x,
        tolerance=1e-6
      )
    }
  }
)


testthat::test_that(
  "Second-axis ghost lines respect the selected reference", {
    
    d = expand.grid(
      x=1:10,
      b=factor(c("x", "y", "z")),
      a=factor(c("a", "b"))
    )
    
    d$y = with(d,
               2*x +
                 ifelse(b == "y", 10, 0) +
                 ifelse(b == "z", 20, 0) +
                 ifelse(a == "b", 30, 0)
    )
    
    withr::local_options(flexplot.object_renderer=TRUE)
    
    p = flexplot(
      y ~ x + b | a,
      data=d,
      ghost.line="grey",
      ghost.reference=list(a="a", b="x"),
      method="lm"
    )
    
    built = ggplot2::ggplot_build(p)
    
    ghost_idx = which(vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomLine") &&
          identical(layer$aes_params$colour, "grey")
      },
      logical(1)
    ))
    
    testthat::expect_length(ghost_idx, 1)
    
    ghost_data = built$data[[ghost_idx]]

    
    testthat::expect_equal(
      length(unique(ghost_data$PANEL)),
      2
    )
    
    # Reference curve: a = a, b = x
    # Therefore y = 2*x in both panels.
    testthat::expect_equal(
      ghost_data$y,
      2 * ghost_data$x,
      tolerance=1e-6
    )
  }
)

testthat::test_that("Ghost lines support numeric conditioning variables", {
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = flexplot(y ~ x | z, data=small,
               ghost.line="grey", method="lm")
  
  built = ggplot2::ggplot_build(p)
  
  ghost_idx = which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomLine") &&
      identical(layer$aes_params$colour, "grey"),
    logical(1)
  ))
  
  testthat::expect_length(ghost_idx, 1)
  testthat::expect_equal(
    length(unique(built$data[[ghost_idx]]$PANEL)),
    3
  )
})

testthat::test_that("Ghost lines support numeric conditioning and second-axis grouping", {
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = flexplot(y ~ x + b | z, data=small,
               ghost.line="grey", method="lm")
  
  built = ggplot2::ggplot_build(p)
  
  ghost_idx = which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomLine") &&
      identical(layer$aes_params$colour, "grey"),
    logical(1)
  ))
  
  testthat::expect_length(ghost_idx, 1)
  testthat::expect_equal(
    length(unique(built$data[[ghost_idx]]$PANEL)),
    3
  )
})

is_gray_ghost_layer = function(layer) {
  inherits(layer$geom, "GeomLine") && identical(layer$aes_params$colour, "gray")
}

expect_separate_ghost_curves = function(p, reference, group=NULL, panels=2) {
  indices = which(vapply(p$layers, is_gray_ghost_layer, logical(1)))
  expect_length(indices, 1)
  if (length(indices) != 1) return(invisible(NULL))

  layer = p$layers[[indices]]
  ids = rlang::eval_tidy(layer$mapping$group, data=layer$data)
  groups = if (is.null(group)) NA_character_ else if (is.factor(reference[[group]])) {
    levels(droplevels(reference[[group]]))
  } else {
    sort(unique(reference[[group]]))
  }
  expect_equal(length(unique(ids)), length(groups))
  expect_s3_class(layer$stat, "StatIdentity")
  expect_false(layer$inherit.aes)
  expect_false(layer$show.legend)

  built = ggplot2::ggplot_build(p)
  ghost = built$data[[indices]]
  expect_equal(nrow(built$layout$layout), panels)
  expect_equal(nrow(ghost), nrow(layer$data) * panels)
  expect_true(all(ghost$colour == "gray"))
  expect_true(all(is.na(ghost$alpha)))
  if (is.null(p$mapping$linetype)) expect_true(all(ghost$linetype == 1))

  for (i in seq_along(groups)) {
    curve = layer$data[ids == sort(unique(ids))[i], c("x", "y")]
    curve = curve[order(curve$x), ]
    expected = reference
    if (!is.null(group)) expected = reference[reference[[group]] == groups[i], ]
    fit = stats::lm(y ~ x, data=expected)
    expect_equal(range(curve$x), range(expected$x))
    expect_equal(curve$y, unname(stats::predict(fit, newdata=curve)))

    for (panel in built$layout$layout$PANEL) {
      drawn = ghost[ghost$PANEL == panel, ]
      expect_equal(length(unique(drawn$group)), length(groups))
      drawn = drawn[drawn$group == sort(unique(drawn$group))[i], ]
      drawn = drawn[order(drawn$x), ]
      expected_linetype = if (is.null(p$mapping$linetype)) 1 else
        built$plot$scales$get_scales("linetype")$map(groups[i])
      expect_true(all(drawn$linetype == expected_linetype))
      expect_equal(drawn$x, curve$x)
      expect_equal(drawn$y, curve$y)
    }
  }
}

test_that("all categorical second-axis reference curves remain distinct across facets", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y ~ x + b | a, data=small, method="lm", alpha=.17,
               ghost.line="gray", ghost.reference=list(a="a"))
  expect_separate_ghost_curves(p, small[small$a == "a", ], "b")

  options(flexplot.object_renderer=FALSE)
  legacy = flexplot(y ~ x + b | a, data=small, method="lm",
                    ghost.line="gray", ghost.reference=list(a="a"))
  native_data = ggplot2::ggplot_build(p)$data[[length(p$layers)]]
  legacy_data = ggplot2::ggplot_build(legacy)$data[[length(legacy$layers)]]
  # Group numbers are local to each ggplot build, so compare the fitted points
  # independently of numbering while requiring three distinct legacy curves.
  legacy_groups = unique(legacy_data[c("PANEL", "group")])
  expect_equal(as.integer(table(legacy_groups$PANEL)), c(3L, 3L))
  native_data = native_data[order(native_data$PANEL, native_data$x, native_data$y), ]
  legacy_data = legacy_data[order(legacy_data$PANEL, legacy_data$x, legacy_data$y), ]
  expect_equal(native_data$x, legacy_data$x)
  expect_equal(native_data$y, legacy_data$y)
  expect_equal(native_data$PANEL, legacy_data$PANEL)
})

test_that("binned numeric second axes retain separate ghost fits and facet membership", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y ~ x + z | a, data=small, method="lm",
               ghost.line="gray", ghost.reference=list(a="a"))
  d = small
  d$z_binned = cut(d$z, quantile(d$z, seq(0, 1, length.out=4)),
                    labels=levels(p$data$z_binned), include.lowest=TRUE)
  expect_separate_ghost_curves(p, d[d$a == "a", ], "z_binned")
})

test_that("ungrouped and single-selected ghost references keep their fitted coordinates", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y ~ x | a, data=small, method="lm",
               ghost.line="gray", ghost.reference=list(a="a"))
  expect_separate_ghost_curves(p, small[small$a == "a", ])

  selected = flexplot(y ~ x + b | a, data=small, method="lm",
                      ghost.line="gray", ghost.reference=list(a="a", b="y"))
  expect_separate_ghost_curves(selected, small[small$a == "a" & small$b == "y", ], "b")

  midpoint = median(small$z)
  conditioned = flexplot(y ~ x | z, data=small, method="lm",
                         ghost.line="gray", ghost.reference=list(z=midpoint))
  breaks = quantile(small$z, seq(0, 1, length.out=4))
  bin = cut(midpoint, breaks, labels=levels(conditioned$data$z_binned), include.lowest=TRUE)
  reference = small[conditioned$data$z_binned == bin, ]
  expect_separate_ghost_curves(conditioned, reference, panels=3)
})

test_that("ghost group identifiers cannot become conditioning variables", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (condition in c(".ghost_group", "lookup")) {
    d = small
    d[[condition]] = d$a
    formula = stats::reformulate(paste0("x + b | ", condition), response="y")
    p = flexplot(formula, data=d, method="lm", ghost.line="gray",
                 ghost.reference=stats::setNames(list("a"), condition))
    expect_separate_ghost_curves(p, d[d[[condition]] == "a", ], "b")
    expect_false(condition %in% names(p$layers[[length(p$layers)]]$data))
  }
})

test_that("primary and ghost curves share categorical and binned line-type encoding", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (group in c("b", "z")) {
    options(flexplot.object_renderer=TRUE)
    formula = stats::reformulate(paste0("x + ", group, " | a"), response="y")
    p = flexplot(formula, data=small, method="lm", ghost.line="gray",
                 ghost.reference=list(a="a"))
    built = ggplot2::ggplot_build(p)
    display_group = if (group == "z") "z_binned" else "b"
    expect_identical(rlang::as_label(p$mapping$linetype), display_group)
    expect_equal(length(unique(built$data[[2]]$linetype)), 3)
    expect_equal(length(unique(built$data[[3]]$linetype)), 3)
    expect_true(all(built$data[[3]]$colour == "gray"))
    expect_false(p$layers[[3]]$show.legend)
    expect_equal(built$plot$labels$colour, display_group)
    expect_equal(built$plot$labels$linetype, display_group)
    legends = cowplot::get_plot_component(p, "guide-box", return_all = TRUE)
    if (inherits(legends, "gtable")) legends = list(legends)
    
    guides = 0L
    for (legend in legends) {
      if (!is.null(legend$layout)) {
        guides = guides + sum(legend$layout$name == "guides")
      }
    }
    expect_equal(guides, 1)

    # Every reference curve retains the line type of its corresponding primary
    # group in every panel, with independent coordinate checks in the helper.
    reference = p$data[p$data$a == "a", ]
    expect_separate_ghost_curves(p, reference, display_group)

    options(flexplot.object_renderer=FALSE)
    legacy = flexplot(formula, data=small, method="lm", ghost.line="gray",
                      ghost.reference=list(a="a"))
    legacy_built = ggplot2::ggplot_build(legacy)
    actual = built$data[[2]]
    expected = legacy_built$data[[2]]
    actual = actual[order(actual$PANEL, actual$group, actual$x), ]
    expected = expected[order(expected$PANEL, expected$group, expected$x), ]
    expect_equal(actual$linetype, expected$linetype)
    expect_equal(actual$x, expected$x)
    expect_equal(actual$y, expected$y)
    expect_equal(actual$group, expected$group)
    expect_equal(actual$PANEL, expected$PANEL)
  }
})

test_that("selected ghost references retain their full-plot line type", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$b = factor(d$b, levels=c("z", "x", "y"))
  categorical = flexplot(y ~ x + b | a, data=d, method="lm", ghost.line="gray",
                         ghost.reference=list(a="a", b="x"))
  expect_separate_ghost_curves(categorical, d[d$a == "a" & d$b == "x", ], "b")
  cat_built = ggplot2::ggplot_build(categorical)
  expect_equal(unique(cat_built$data[[3]]$linetype),
               cat_built$plot$scales$get_scales("linetype")$map("x"))
  subset = flexplot(y ~ x + b | a, data=d, method="lm", ghost.line="gray",
                    ghost.reference=list(a="a", b=c("x", "y")))
  expect_separate_ghost_curves(subset, d[d$a == "a" & d$b %in% c("x", "y"), ], "b")

  midpoint = median(small$z)
  numeric = flexplot(y ~ x + z | a, data=small, method="lm", ghost.line="gray",
                     ghost.reference=list(a="a", z=midpoint))
  numeric_built = ggplot2::ggplot_build(numeric)
  breaks = quantile(small$z, seq(0, 1, length.out=4))
  bin = cut(midpoint, breaks, labels=levels(numeric$data$z_binned), include.lowest=TRUE)
  expect_equal(unique(numeric_built$data[[3]]$linetype),
               numeric_built$plot$scales$get_scales("linetype")$map(bin))
  expect_separate_ghost_curves(numeric,
                               numeric$data[numeric$data$a == "a" & numeric$data$z_binned == bin, ],
                               "z_binned")
})

test_that("high-cardinality categorical numeric plots retain legacy color-only lines", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small[rep(seq_len(nrow(small)), times=7), ]
  d$b = factor(rep(letters[1:7], each=nrow(small)))
  p = flexplot(y ~ x + b | a, data=d, method="lm", ghost.line="gray",
               ghost.reference=list(a="a"))
  built = ggplot2::ggplot_build(p)
  expect_null(p$mapping$linetype)
  expect_true(all(built$data[[2]]$linetype == 1))
  expect_true(all(built$data[[3]]$linetype == 1))
  expect_equal(length(unique(built$data[[3]]$group)), 7)
})

