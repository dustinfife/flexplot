prediction_ghost_index = function(p) {
  for (i in seq_along(p$layers)) {
    if (identical(p$layers[[i]]$aes_params$colour, "gray")) return(i)
  }
  stop("No prediction ghost layer")
}

prediction_ghost_main_index = function(p) {
  for (i in seq_along(p$layers)) {
    if (is.data.frame(p$layers[[i]]$data) &&
        all(c("prediction", "model") %in% names(p$layers[[i]]$data))) return(i)
  }
  stop("No ordinary prediction layer")
}

prediction_ghost_expected = function(model, data, condition, reference, method, label) {
  data$prediction = unname(stats::predict(model, newdata=data))
  if (method == "observed") {
    edges = unique(stats::quantile(data$x, seq(0, 1, length.out=10)))
    bins = cut(data$x, edges, include.lowest=TRUE, labels=FALSE)
    centers = (head(edges, -1) + tail(edges, -1)) / 2
    data$x = centers[bins]
  }
  selected = data[as.character(data[[condition]]) == reference, ]
  expected = stats::aggregate(selected$prediction, list(x=selected$x), mean)
  names(expected)[2] = "prediction"
  expected$model = label
  expected
}

expect_prediction_ghost = function(p, expected, panels, reference) {
  index = prediction_ghost_index(p)
  layer = p$layers[[index]]
  expect_s3_class(layer$geom, "GeomLine")
  expect_false(layer$inherit.aes)
  expect_false(layer$show.legend)
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  ghost = built$data[[index]]
  layout = built$layout$layout
  is_reference = rep(TRUE, nrow(layout))
  for (variable in names(reference)) {
    is_reference = is_reference & as.character(layout[[variable]]) == reference[[variable]]
  }
  expect_equal(sum(is_reference), 1)
  expect_equal(nrow(ghost), nrow(expected) * (panels - 1))
  expect_equal(length(unique(ghost$PANEL)), panels - 1)
  expect_setequal(as.character(unique(ghost$PANEL)), as.character(layout$PANEL[!is_reference]))
  expect_false(any(ghost$PANEL %in% layout$PANEL[is_reference]))
  ordinary = built$data[[prediction_ghost_main_index(p)]]
  expect_setequal(unique(ordinary$PANEL), layout$PANEL)
  expect_true(all(ordinary$colour != "gray"))
  for (panel in layout$PANEL) {
    expect_equal(length(unique(ordinary$group[ordinary$PANEL == panel])), length(unique(expected$model)))
  }
  expect_true(all(ghost$colour == "gray"))
  expect_true(all(is.na(ghost$alpha)))
  expect_true(all(ghost$linewidth == .5))
  scale = built$plot$scales$get_scales("linetype")
  ordinary_reference = ordinary[ordinary$PANEL %in% layout$PANEL[is_reference], ]
  for (model in unique(expected$model)) {
    reference = expected[expected$model == model, ]
    curve = ordinary_reference[ordinary_reference$linetype == scale$map(model), ]
    rows = match(round(curve$x, 10), round(reference$x, 10))
    expect_false(anyNA(rows))
    expect_setequal(round(curve$x, 10), round(reference$x, 10))
    expect_equal(curve$y, reference$prediction[rows])
    color = built$plot$scales$get_scales("colour")$map(model)
    expect_equal(curve$colour, rep(color, nrow(curve)))
  }
  for (panel in unique(ghost$PANEL)) {
    block = ghost[ghost$PANEL == panel, ]
    expect_equal(length(unique(block$group)), length(unique(expected$model)))
    for (model in unique(expected$model)) {
      reference = expected[expected$model == model, ]
      curve = block[block$linetype == scale$map(model), ]
      expect_equal(length(unique(curve$group)), 1)
      expect_equal(nrow(curve), nrow(reference))
      rows = match(round(curve$x, 10), round(reference$x, 10))
      expect_false(anyNA(rows))
      expect_equal(curve$y, reference$prediction[rows])
      expect_equal(as.numeric(curve$x), sort(as.numeric(curve$x)))
    }
  }
  invisible(built)
}

test_that("prediction ghosts preserve ordinary reference curves and match legacy elsewhere", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = lm(y ~ x + a + z, data=small)
  m2 = lm(y ~ x * a + z, data=small)
  for (method in c("observed", "reference")) {
    for (two in c(FALSE, TRUE)) {
      options(flexplot.object_renderer=TRUE)
      models = if (two) list(m1, m2) else list(m1)
      arguments = list(formula=y ~ x | a, data=small, model1=m1,
                        prediction_method=method, jitter=FALSE)
      if (two) arguments$model2 = m2
      # Use identical public arguments across the baseline and both renderers.
      base = suppressMessages(do.call(compare_fits, arguments))
      arguments$ghost.line = "gray"
      arguments$ghost.reference = list(a="a")
      p = suppressMessages(do.call(compare_fits, arguments))
      main = p$layers[[prediction_ghost_main_index(p)]]$data
      labels = unique(main$model)
      expected = list()
      for (i in seq_along(models)) {
        source = if (method == "observed") small else main[main$model == labels[i], ]
        expected[[i]] = prediction_ghost_expected(models[[i]], source, "a", "a", method, labels[i])
      }
      expected = do.call(rbind, expected)
      built = expect_prediction_ghost(p, expected, 2, list(a="a"))
      expect_no_warning(ggplot2::ggplotGrob(p))
      original = ggplot2::ggplot_build(base)
      expect_equal(built$layout$layout, original$layout$layout)
      expect_length(p$layers, length(base$layers) + 1)
      for (i in seq_along(base$layers)) expect_equal(built$data[[i]], original$data[[i]])
      options(flexplot.object_renderer=FALSE)
      legacy = suppressMessages(do.call(compare_fits, arguments))
      old = ggplot2::ggplot_build(legacy)$data[[prediction_ghost_index(legacy)]]
      columns = c("x", "y", "PANEL", "colour", "linewidth", "alpha")
      actual = built$data[[prediction_ghost_index(p)]]
      # Legacy draws a redundant ghost over the reference prediction. Compare
      # its other panels; native ghosts deliberately exclude that reference.
      reference_panel = built$layout$layout$PANEL[built$layout$layout$a == "a"]
      old = old[!old$PANEL %in% reference_panel, ]
      rownames(old) = NULL
      rownames(actual) = NULL
      expect_equal(actual[columns], old[columns])
      if (two) expect_equal(actual$linetype, old$linetype)
    }
  }
})

test_that("supplied prediction ghosts collapse duplicate x rows without duplicating ordinary curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$x = round(d$x, 1)
  m = lm(y ~ x * a, data=d)
  predictions = data.frame(x=d$x, a=d$a, prediction=unname(predict(m, newdata=d)), model="m")
  p = flexplot(y ~ x | a, d, prediction=predictions, suppress_smooth=TRUE, jitter=FALSE,
               ghost.line="gray", ghost.reference=list(a="a"))
  source = p$layers[[prediction_ghost_main_index(p)]]$data
  expected = prediction_ghost_expected(m, source, "a", "a", "reference", unique(source$model))
  expect_true(anyDuplicated(source[c("x", "a")]) > 0)
  built = expect_prediction_ghost(p, expected, 2, list(a="a"))
  expect_equal(nrow(built$data[[prediction_ghost_main_index(p)]]), nrow(source))
  expect_equal(nrow(p$layers[[prediction_ghost_index(p)]]$data), nrow(expected))
})

test_that("numeric conditioning selects the same prediction bin without refitting", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = lm(y ~ x + z, data=small)
  m2 = lm(y ~ x * z, data=small)
  edges = stats::quantile(small$z, seq(0, 1, length.out=4))
  selected = cut(median(small$z), edges, include.lowest=TRUE, labels=FALSE)
  for (method in c("observed", "reference")) {
    p = compare_fits(y ~ x | z, small, m1, m2, prediction_method=method,
                     ghost.line="gray", ghost.reference=list(z=median(small$z)), jitter=FALSE)
    main = p$layers[[prediction_ghost_main_index(p)]]$data
    labels = unique(main$model)
    models = list(m1, m2)
    expected = list()
    for (i in seq_along(models)) {
      source = if (method == "observed") small else main[main$model == labels[i], ]
      source$bin = cut(source$z, edges, include.lowest=TRUE, labels=FALSE)
      expected[[i]] = prediction_ghost_expected(models[[i]], source, "bin", selected, method, labels[i])
    }
    expected = do.call(rbind, expected)
    reference_bin = levels(p$data$z_binned)[selected]
    built = expect_prediction_ghost(p, expected, 3, list(z_binned=reference_bin))
    expect_equal(as.character(built$layout$layout$z_binned), levels(p$data$z_binned))
    options(flexplot.object_renderer=FALSE)
    legacy = compare_fits(y ~ x | z, small, m1, m2, prediction_method=method,
                          ghost.line="gray", ghost.reference=list(z=median(small$z)), jitter=FALSE)
    old = ggplot2::ggplot_build(legacy)$data[[prediction_ghost_index(legacy)]]
    reference_panel = built$layout$layout$PANEL[built$layout$layout$z_binned == reference_bin]
    old = old[!old$PANEL %in% reference_panel, ]
    actual = built$data[[prediction_ghost_index(p)]]
    rownames(old) = NULL
    rownames(actual) = NULL
    expect_equal(actual[c("x", "y", "linetype", "PANEL")],
                   old[c("x", "y", "linetype", "PANEL")])
    options(flexplot.object_renderer=TRUE)
  }
})

test_that("prediction ghosts preserve reordered and multiple conditioning facets", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  m = lm(y ~ x + a + b, data=d)
  for (method in c("observed", "reference")) {
    base = compare_fits(y ~ x | a + b, d, m, prediction_method=method, jitter=FALSE)
    p = compare_fits(y ~ x | a + b, d, m, prediction_method=method, jitter=FALSE,
                     ghost.line="gray", ghost.reference=list(a="a", b="x"))
    source = p$layers[[prediction_ghost_main_index(p)]]$data
    source = source[source$a == "a" & source$b == "x", ]
    expected = stats::aggregate(source$prediction, list(x=source$x, model=source$model), mean)
    names(expected)[3] = "prediction"
    built = expect_prediction_ghost(p, expected, 6, list(a="a", b="x"))
    expect_equal(built$layout$layout, ggplot2::ggplot_build(base)$layout$layout)
    expect_equal(unique(as.character(built$layout$layout$a)), levels(d$a))
    expect_equal(unique(as.character(built$layout$layout$b)), levels(d$b))
  }
})

test_that("sampling changes observations but neither prediction nor ghost curves", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  m1 = lm(y ~ x + a, data=small)
  m2 = lm(y ~ x * a, data=small)
  for (method in c("observed", "reference")) {
    full = compare_fits(y ~ x | a, small, m1, m2, prediction_method=method, jitter=FALSE,
                        ghost.line="gray", ghost.reference=list(a="a"))
    original = ggplot2::ggplot_build(full)
    for (size in c(0, 1, 8)) {
      set.seed(123)
      p = compare_fits(y ~ x | a, small, m1, m2, prediction_method=method, jitter=FALSE,
                       ghost.line="gray", ghost.reference=list(a="a"), sample=size)
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      expect_equal(nrow(built$data[[1]]), size)
      expect_equal(built$data[[prediction_ghost_main_index(p)]],
                     original$data[[prediction_ghost_main_index(full)]])
      expect_equal(built$data[[prediction_ghost_index(p)]],
                     original$data[[prediction_ghost_index(full)]])
      expect_equal(built$layout$layout, original$layout$layout)
      expect_equal(built$layout$panel_params[[1]]$y.range, original$layout$panel_params[[1]]$y.range)
    }
  }
})

test_that("prediction ghost coordinates retain destination facets with colliding column names", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (condition in c("y", ".ghost_group", ".ghost_x")) {
    d = small
    d$response = d$y
    d[[condition]] = d$a
    model_formula = stats::reformulate(c("x", condition), response="response")
    formula = stats::as.formula(paste("response ~ x |", condition))
    m = lm(model_formula, data=d)
    reference = stats::setNames(list("a"), condition)
    p = compare.fits(formula, d, m, ghost.line="gray", ghost.reference=reference)
    main = p$layers[[prediction_ghost_main_index(p)]]$data
    selected = main[[condition]] == "a"
    expected = data.frame(x=main$x[selected], prediction=main$prediction[selected],
                           model=as.character(main$model[selected]))
    built = expect_prediction_ghost(p, expected, 2, reference)
    expect_equal(as.character(built$layout$layout[[condition]]), levels(d[[condition]]))
  }
})

test_that("unsupported grouped or pooled prediction ghosts have informative errors", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = lm(y ~ x + a + z, data=small)
  expect_error(compare_fits(y ~ x + a | z, small, m, ghost.line="gray",
                            ghost.reference=list(z=median(small$z))),
                 "Prediction ghost lines with a second-axis grouping variable", fixed=TRUE)
  expect_error(compare_fits(y ~ x + z | a, small, m, ghost.line="gray", ghost.reference=list(a="a")),
                 "Prediction ghost lines with a second-axis grouping variable", fixed=TRUE)
  expect_error(compare_fits(y ~ x | a, small, m, ghost.line="gray", ghost.reference=list(a=c("a", "b"))),
                 "one reference category or bin per conditioning variable", fixed=TRUE)
  expect_error(compare_fits(y ~ x | a, small, m, ghost.line="gray", ghost.reference=list(a="missing")),
                 "No observations match the ghost reference", fixed=TRUE)
})
