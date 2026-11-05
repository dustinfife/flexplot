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

categorical_prediction_ghost_plot = function(formula, data, models, method, ...) {
  model1 = models[[1]]
  model2 = if (length(models) > 1) models[[2]] else NULL
  suppressMessages(compare_fits(formula, data, model1, model2,
                                 prediction_method=method, jitter=FALSE, ...))
}

categorical_prediction_ghost_expected = function(models, data, source, keys, method) {
  labels = unique(as.character(source$model))
  expected = list()
  for (i in seq_along(models)) {
    block = if (method == "observed") data else source[source$model == labels[i], ]
    values = unname(stats::predict(models[[i]], newdata=block, type="response"))
    frame = stats::aggregate(values, block[keys], mean)
    names(frame)[ncol(frame)] = "prediction"
    frame$model = labels[i]
    expected[[i]] = frame
  }
  do.call(rbind, expected)
}

prediction_ghost_is_prediction_line = function(layer) {
  inherits(layer$geom, "GeomLine") && is.data.frame(layer$data) && "prediction" %in% names(layer$data)
}

prediction_ghost_is_point = function(layer) inherits(layer$geom, "GeomPoint")
prediction_ghost_is_summary = function(layer) inherits(layer$stat, "StatSummary")

expect_categorical_prediction_ghost = function(p, expected, reference, magnet=FALSE) {
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_no_warning(ggplot2::ggplotGrob(p))
  index = prediction_ghost_index(p)
  ghost = built$data[[index]]
  expect_s3_class(p$layers[[index]]$geom, "GeomLine")
  expect_false(p$layers[[index]]$inherit.aes)
  expect_false(p$layers[[index]]$show.legend)
  layout = built$layout$layout
  is_reference = rep(TRUE, nrow(layout))
  selected = rep(TRUE, nrow(expected))
  for (variable in names(reference)) {
    is_reference = is_reference & as.character(layout[[variable]]) == reference[[variable]]
    selected = selected & as.character(expected[[variable]]) == reference[[variable]]
  }
  expected = expected[selected, ]
  models = sort(unique(expected$model))
  width = if (magnet) .5 else .4
  source = p$layers[[prediction_ghost_main_index(p)]]$data
  expected$position = match(as.character(expected$a), levels(source$a)) +
    width * ((match(expected$model, models) - .5) / length(models) - .5)
  expect_equal(sum(is_reference), 1)
  expect_setequal(unique(ghost$PANEL), layout$PANEL[!is_reference])
  expect_false(any(ghost$PANEL %in% layout$PANEL[is_reference]))
  expect_equal(nrow(ghost), nrow(expected) * sum(!is_reference))
  expect_true(all(ghost$colour == "gray"))
  expect_true(all(is.na(ghost$alpha)))
  expect_true(all(ghost$linewidth == .5))
  types = built$plot$scales$get_scales("linetype")
  ordinary = built$data[[prediction_ghost_main_index(p)]]
  expect_setequal(unique(ordinary$PANEL), layout$PANEL)
  for (panel in unique(ghost$PANEL)) {
    block = ghost[ghost$PANEL == panel, ]
    expect_equal(length(unique(block$group)), length(models))
    for (model in models) {
      curve = block[block$linetype == types$map(model), ]
      target = expected[expected$model == model, ]
      rows = match(round(curve$x, 10), round(target$position, 10))
      expect_false(anyNA(rows))
      expect_equal(nrow(curve), nrow(target))
      expect_equal(length(unique(curve$group)), 1)
      expect_equal(curve$y, target$prediction[rows])
      expect_true(all(diff(as.numeric(curve$x)) > 0))
      markers = ordinary[ordinary$PANEL %in% layout$PANEL[is_reference], ]
      if (!magnet || length(models) > 1) {
        color = built$plot$scales$get_scales("colour")$map(model)
        markers = markers[markers$colour == color, ]
      }
      expect_equal(sort(as.numeric(markers$x)), sort(target$position))
      expect_equal(markers$y, target$prediction[match(round(markers$x, 10), round(target$position, 10))])
    }
  }
  invisible(built)
}

test_that("categorical and magnet prediction ghosts reuse one or two models and both methods", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (magnet in c(FALSE, TRUE)) {
    models = if (magnet) {
      list(glm(y_bin ~ a + b, small, family=binomial()),
           glm(y_bin ~ a + b + z, small, family=binomial()))
    } else {
      list(lm(y ~ x + a + b + z, small), lm(y ~ x * a + b + z, small))
    }
    formula = if (magnet) y_bin ~ a | b else y ~ a | b
    for (method in c("observed", "reference")) {
      for (number in 1:2) {
        selected_models = models[seq_len(number)]
        base = categorical_prediction_ghost_plot(formula, small, selected_models, method)
        p = categorical_prediction_ghost_plot(formula, small, selected_models, method,
                                              ghost.line="gray", ghost.reference=list(b="x"))
        source = p$layers[[prediction_ghost_main_index(p)]]$data
        expected = categorical_prediction_ghost_expected(selected_models, small, source, c("a", "b"), method)
        built = expect_categorical_prediction_ghost(p, expected, list(b="x"), magnet)
        original = ggplot2::ggplot_build(base)
        expect_length(p$layers, length(base$layers) + 1)
        expect_equal(built$layout$layout, original$layout$layout)
        for (i in seq_along(base$layers)) expect_equal(built$data[[i]], original$data[[i]])
        ordinary_lines = Filter(prediction_ghost_is_prediction_line, p$layers)
        expect_length(ordinary_lines, 1)
        if (number == 2) {
          expect_setequal(unique(built$data[[length(base$layers)]]$linetype), unique(built$data[[prediction_ghost_index(p)]]$linetype))
        }
        if (!magnet) next
        expect_true(all(built$data[[2]]$shape == 1))
        points = Filter(prediction_ghost_is_point, p$layers)
        expect_length(points, 2)
        expect_false(any(vapply(p$layers, prediction_ghost_is_summary, logical(1))))
        for (layer in points) {
          expect_false(is.data.frame(layer$data) && "pred_prob" %in% names(layer$data))
        }
        expect_true(all(built$data[[prediction_ghost_main_index(p)]]$shape %in% c(17, 15)))
      }
    }
  }
})

test_that("categorical prediction ghosts preserve reordered factors under sampling", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  for (magnet in c(FALSE, TRUE)) {
    models = if (magnet) {
      list(glm(y_bin ~ a + b, d, family=binomial()), glm(y_bin ~ a + b + z, d, family=binomial()))
    } else list(lm(y ~ a + b + x, d), lm(y ~ a * x + b, d))
    formula = if (magnet) y_bin ~ a | b else y ~ a | b
    for (method in c("observed", "reference")) {
      full = categorical_prediction_ghost_plot(formula, d, models, method,
                                               ghost.line="gray", ghost.reference=list(b="x"))
      source = full$layers[[prediction_ghost_main_index(full)]]$data
      expected = categorical_prediction_ghost_expected(models, d, source, c("a", "b"), method)
      original = expect_categorical_prediction_ghost(full, expected, list(b="x"), magnet)
      expect_equal(original$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
      expect_equal(as.character(original$layout$layout$b), levels(d$b))
      for (size in c(0, 1, 8)) {
        set.seed(123)
        p = categorical_prediction_ghost_plot(formula, d, models, method,
                                              ghost.line="gray", ghost.reference=list(b="x"), sample=size)
        built = expect_categorical_prediction_ghost(p, expected, list(b="x"), magnet)
        raw = if (magnet) 2 else 1
        expect_equal(nrow(built$data[[raw]]), size)
        expect_equal(built$data[[prediction_ghost_index(p)]], original$data[[prediction_ghost_index(full)]])
        expect_equal(built$data[[prediction_ghost_main_index(p)]], original$data[[prediction_ghost_main_index(full)]])
        expect_equal(built$layout$layout, original$layout$layout)
        expect_equal(built$layout$panel_params[[1]]$y.range, original$layout$panel_params[[1]]$y.range)
      }
    }
  }
})

test_that("categorical prediction ghosts select numeric conditioning bins", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  edges = stats::quantile(d$z, seq(0, 1, length.out=4))
  d$bin = cut(d$z, edges, include.lowest=TRUE, labels=FALSE)
  selected = cut(median(d$z), edges, include.lowest=TRUE, labels=FALSE)
  for (magnet in c(FALSE, TRUE)) {
    models = if (magnet) {
      list(glm(y_bin ~ a + z, d, family=binomial()), glm(y_bin ~ a + z + x, d, family=binomial()))
    } else list(lm(y ~ a + z, d), lm(y ~ a * z, d))
    formula = if (magnet) y_bin ~ a | z else y ~ a | z
    for (method in c("observed", "reference")) {
      p = categorical_prediction_ghost_plot(formula, d, models, method,
                                            ghost.line="gray", ghost.reference=list(z=median(d$z)))
      source = p$layers[[prediction_ghost_main_index(p)]]$data
      expected = categorical_prediction_ghost_expected(models, d, source,
                                                        if (method == "observed") c("a", "bin") else c("a", "z_binned"), method)
      if (method == "observed") {
        expected$z_binned = levels(source$z_binned)[expected$bin]
      }
      expect_categorical_prediction_ghost(p, expected,
                                           list(z_binned=levels(source$z_binned)[selected]), magnet)
    }
  }
})

test_that("categorical prediction ghosts retain empty category slots and multiple facets", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = ordered(rep(c("first", "second", "third"), length.out=nrow(d)), levels=c("third", "first", "second"))
  d = d[!(d$b == "x" & d$a == "first"), ]
  models = list(lm(y ~ a + b + x, d), lm(y ~ a * x + b, d))
  for (method in c("observed", "reference")) {
    p = categorical_prediction_ghost_plot(y ~ a | b, d, models, method,
                                          ghost.line="gray", ghost.reference=list(b="x"))
    source = p$layers[[prediction_ghost_main_index(p)]]$data
    expected = categorical_prediction_ghost_expected(models, d, source, c("a", "b"), method)
    built = expect_categorical_prediction_ghost(p, expected, list(b="x"))
    expect_equal(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
    ghost = built$data[[prediction_ghost_index(p)]]
    expect_true(all(abs(ghost$x - 2) > .5))
  }
  models = list(lm(y ~ a + b + y_bin, small))
  p = categorical_prediction_ghost_plot(y ~ a | b + y_bin, small, models, "observed",
                                        ghost.line="gray", ghost.reference=list(b="x", y_bin=1))
  source = p$layers[[prediction_ghost_main_index(p)]]$data
  expected = categorical_prediction_ghost_expected(models, small, source, c("a", "b", "y_bin"), "observed")
  expect_categorical_prediction_ghost(p, expected, list(b="x", y_bin="1"))
})

test_that("unsupported categorical prediction ghost configurations are explicit", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = lm(y ~ a + b + z, small)
  mb = glm(y_bin ~ a + b + z, small, family=binomial())
  expect_error(compare_fits(y ~ a + b | z, small, m, ghost.line="gray"),
                 "Prediction ghost lines with a second-axis grouping variable", fixed=TRUE)
  expect_error(compare_fits(y_bin ~ a + b | z, small, mb, ghost.line="gray"),
                 "Prediction ghost lines with a second-axis grouping variable", fixed=TRUE)
  expect_error(compare_fits(y ~ a, small, m, ghost.line="gray"),
                 "Prediction ghost lines require a conditioning variable", fixed=TRUE)
  expect_error(compare_fits(y_bin ~ a | b, small, mb, ghost.line="gray", ghost.reference=list(b=c("x", "y"))),
                 "one reference category or bin per conditioning variable", fixed=TRUE)
  d = small[!(small$b == "x" & small$a == "b"), ]
  m = lm(y ~ a + b, d)
  expect_error(compare_fits(y ~ a | b, d, m, ghost.line="gray", ghost.reference=list(b="x")),
                 "at least two x categories for each model", fixed=TRUE)
})
