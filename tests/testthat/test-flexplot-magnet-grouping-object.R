expect_magnet_full_group_counts = function(p, data, group, facets=character()) {
  raw = p$layers[[2]]$data
  counts = p$layers[[4]]$data
  variables = c("a", group, facets)
  max_n = max(counts$n_total)
  expect_equal(sum(counts$n_total), nrow(data))
  expect_equal(nrow(raw), nrow(data))

  for (i in seq_len(nrow(counts))) {
    original_rows = rep(TRUE, nrow(data))
    plotted_rows = rep(TRUE, nrow(raw))
    for (variable in variables) {
      value = as.character(counts[[variable]][i])
      original_rows = original_rows & as.character(data[[variable]]) == value
      plotted_rows = plotted_rows & as.character(raw[[variable]]) == value
    }
    zeros = sum(data$y_bin[original_rows] == 0)
    ones = sum(data$y_bin[original_rows] == 1)
    expect_equal(counts$n_total[i], zeros + ones)
    expect_true(all(raw$n_total[plotted_rows] == zeros + ones))
    expected = c((seq_len(zeros) - .5) / (2 * max_n),
                  1 - (seq_len(ones) - .5) / (2 * max_n))
    expect_equal(sort(raw$y_scaled[plotted_rows]), sort(expected))
  }
}

test_that("native categorical magnet groups use full-data additive logistic fits", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y_bin ~ a + b, data=small, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  fitted = p$layers[[3]]$data
  fit = stats::glm(y_bin ~ a + b, data=small, family=stats::binomial())
  expect_equal(unname(fitted$pred_prob),
               unname(stats::predict(fit, newdata=fitted, type="response")))
  expect_equal(nrow(fitted), nrow(unique(small[c("a", "b")])))
  expect_identical(rlang::as_label(p$layers[[2]]$mapping$colour), "b")
  expect_identical(rlang::as_label(p$layers[[3]]$mapping$group), "b")
  expect_equal(length(unique(built$data[[2]]$colour)), nlevels(small$b))
  expect_equal(length(unique(built$data[[3]]$group)), nlevels(small$b))
  expect_equal(built$data[[3]]$colour,
               built$plot$scales$get_scales("colour")$map(fitted$b))
  expect_equal(built$data[[3]]$y, unname(fitted$pred_prob))
  expect_magnet_full_group_counts(p, small, "b")

  # Group-colored predictions and count labels align horizontally.
  counts = p$layers[[4]]$data
  fit_keys = paste(fitted$a, fitted$b)
  count_keys = paste(counts$a, counts$b)
  expect_equal(as.numeric(built$data[[3]]$x),
               as.numeric(built$data[[4]]$x[match(fit_keys, count_keys)]))
  expect_equal(built$data[[4]]$label, paste0("N=", counts$n_total))
})

test_that("native numeric magnet groups model continuous values at full-bin medians", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  labels = c("low", "middle", "high")
  p = flexplot(y_bin ~ a + z | b, data=d, jitter=FALSE,
               breaks=list(z=c(-.5, .5)), labels=list(z=labels))
  built = ggplot2::ggplot_build(p)
  d$z_binned = cut(d$z, breaks=c(min(d$z), -.5, .5, max(d$z)),
                    labels=labels, include.lowest=TRUE)
  fitted = p$layers[[3]]$data
  medians = tapply(d$z, d$z_binned, median)
  expected_data = fitted
  expected_data$z = as.numeric(medians[as.character(fitted$z_binned)])
  expect_equal(fitted$z, expected_data$z)
  fit = stats::glm(y_bin ~ a + z + b, data=d, family=stats::binomial())
  expect_equal(unname(fitted$pred_prob),
               unname(stats::predict(fit, newdata=expected_data, type="response")))
  expect_identical(levels(p$layers[[2]]$data$a), levels(d$a))
  expect_equal(levels(fitted$z_binned), labels)
  expect_identical(rlang::as_label(p$layers[[2]]$mapping$colour), "z_binned")
  expect_equal(nrow(fitted), nrow(unique(d[c("a", "z_binned", "b")])))
  expect_equal(nrow(built$layout$layout), nlevels(d$b))
  expect_magnet_full_group_counts(p, d, "z_binned", "b")
  sampled = flexplot(y_bin ~ a + z | b, data=d, sample=8, jitter=FALSE,
                     breaks=list(z=c(-.5, .5)), labels=list(z=labels))
  expect_equal(nrow(ggplot2::ggplot_build(sampled)$data[[2]]), 8)
  expect_equal(sampled$layers[[3]]$data, p$layers[[3]]$data)
  expect_equal(sampled$layers[[4]]$data, p$layers[[4]]$data)

  returned = flexplot(y_bin ~ a + z | b, data=d, return_data=TRUE,
                      breaks=list(z=c(-.5, .5)), labels=list(z=labels))
  expect_equal(returned$n_total, p$layers[[2]]$data$n_total)
  expect_equal(returned$z_binned, p$layers[[2]]$data$z_binned)

  for (i in seq_len(nrow(built$layout$layout))) {
    panel = built$layout$layout[i, ]
    probabilities = built$data[[3]][built$data[[3]]$PANEL == panel$PANEL, ]
    expected = fitted[fitted$b == panel$b, ]
    expect_equal(sort(probabilities$y), sort(unname(expected$pred_prob)))
    expect_equal(length(unique(probabilities$group)), length(unique(expected$z_binned)))
  }
})

test_that("sampling leaves categorical and numeric magnet groups unchanged", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  d$row_id = seq_len(nrow(d))
  for (group in c("b", "z")) {
    formula = stats::reformulate(c("a", group), response="y_bin")
    full = flexplot(formula, data=d, jitter=FALSE)
    p = flexplot(formula, data=d, sample=8, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    raw = p$layers[[2]]$data
    all_raw = full$layers[[2]]$data
    expect_equal(nrow(built$data[[2]]), 8)
    expect_equal(raw$y_scaled, all_raw$y_scaled[match(raw$row_id, all_raw$row_id)])
    expect_identical(levels(raw$a), levels(d$a))
    expect_equal(p$layers[[3]]$data, full$layers[[3]]$data)
    expect_equal(p$layers[[4]]$data, full$layers[[4]]$data)
    expect_equal(sum(p$layers[[4]]$data$n_total), nrow(d))
    expect_equal(built$data[[3]]$y, ggplot2::ggplot_build(full)$data[[3]]$y)
  }
})

test_that("native grouped magnet visuals retain legacy colors and categorical stacks", {
  withr::local_options(flexplot.object_renderer=TRUE)

  native = flexplot(y_bin ~ a + b, data=small, jitter=FALSE)
  native_built = ggplot2::ggplot_build(native)
  native_numeric = flexplot(y_bin ~ a + z, data=small, jitter=FALSE)
  options(flexplot.object_renderer=FALSE)
  legacy = flexplot(y_bin ~ a + b, data=small)
  legacy_built = ggplot2::ggplot_build(legacy)
  expect_equal(native_built$data[[2]]$y, legacy_built$data[[2]]$y)
  expect_equal(native_built$data[[2]]$colour, legacy_built$data[[2]]$colour)
  expect_true(all(legacy_built$data[[3]]$colour == "blue"))
  expect_equal(native_built$data[[3]]$colour,
               native_built$plot$scales$get_scales("colour")$map(native$layers[[3]]$data$b))
  # Legacy pools second-axis categories; native supplies a logistic marker per group.
  means = aggregate(y_bin ~ a, data=small, FUN=mean)
  expect_equal(sort(legacy_built$data[[3]]$y), sort(means$y_bin))
  expect_equal(nrow(legacy_built$data[[3]]), nlevels(small$a))
  expect_equal(nrow(native_built$data[[3]]), nrow(unique(small[c("a", "b")])))

  legacy_numeric = flexplot(y_bin ~ a + z, data=small)
  native_numeric_built = ggplot2::ggplot_build(native_numeric)
  legacy_numeric_built = ggplot2::ggplot_build(legacy_numeric)
  expect_equal(length(unique(native_numeric_built$data[[2]]$colour)),
               nlevels(native_numeric$layers[[2]]$data$z_binned))
  expect_equal(native_numeric$layers[[2]]$data$z_binned, legacy_numeric$data$z_binned)
  # Legacy currently uses continuous colors and per-value stacks for numeric axis 2.
  # Native intentionally uses bin colors/stacks and continuous-model bin medians.
  expect_gt(length(unique(legacy_numeric_built$data[[2]]$colour)),
             length(unique(native_numeric_built$data[[2]]$colour)))
  expect_equal(native_numeric_built$data[[3]]$colour,
               native_numeric_built$plot$scales$get_scales("colour")$map(
                 native_numeric$layers[[3]]$data$z_binned
               ))
  expect_equal(nrow(legacy_numeric_built$data[[3]]), nlevels(small$a))
})

expect_magnet_numeric_panel_data = function(p, original, variables, facet_variables) {
  built = ggplot2::ggplot_build(p)
  fitted = p$layers[[3]]$data
  counts = p$layers[[4]]$data
  expected = fitted
  for (variable in variables) {
    medians = tapply(original[[variable]], original[[paste0(variable, "_binned")]], median)
    expected[[variable]] = as.numeric(medians[as.character(fitted[[paste0(variable, "_binned")]])])
    expect_equal(fitted[[variable]], expected[[variable]])
  }
  model_variables = c("a", if ("b" %in% names(fitted)) "b", variables)
  formula = stats::reformulate(model_variables, response="y_bin")
  model = stats::glm(formula, data=original, family=stats::binomial())
  expect_equal(unname(fitted$pred_prob),
               unname(stats::predict(model, newdata=expected, type="response")))
  expect_equal(nrow(built$data[[3]]), nrow(fitted))
  expect_equal(nrow(built$data[[4]]), nrow(counts))

  for (i in seq_len(nrow(built$layout$layout))) {
    panel = built$layout$layout[i, ]
    original_rows = rep(TRUE, nrow(original))
    fitted_rows = rep(TRUE, nrow(fitted))
    count_rows = rep(TRUE, nrow(counts))
    for (variable in facet_variables) {
      original_rows = original_rows & as.character(original[[variable]]) == as.character(panel[[variable]])
      fitted_rows = fitted_rows & as.character(fitted[[variable]]) == as.character(panel[[variable]])
      count_rows = count_rows & as.character(counts[[variable]]) == as.character(panel[[variable]])
    }
    markers = built$data[[3]][built$data[[3]]$PANEL == panel$PANEL, ]
    labels = built$data[[4]][built$data[[4]]$PANEL == panel$PANEL, ]
    raw = built$data[[2]][built$data[[2]]$PANEL == panel$PANEL, ]
    expect_equal(nrow(raw), sum(original_rows))
    expect_equal(nrow(markers), sum(fitted_rows))
    expect_equal(sort(markers$y), sort(unname(fitted$pred_prob[fitted_rows])))
    expect_equal(nrow(labels), sum(count_rows))
    expected_labels = if (any(count_rows)) paste0("N=", counts$n_total[count_rows]) else character()
    expect_equal(sort(labels$label), sort(expected_labels))
    expect_equal(sum(counts$n_total[count_rows]), sum(original_rows))
  }
}

test_that("numeric-conditioned magnet markers and counts stay in their display panels", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y_bin ~ a | z, data=small, jitter=FALSE)
  d = small
  d$z_binned = cut(d$z, quantile(d$z, seq(0, 1, length.out=4)),
                    labels=levels(p$layers[[2]]$data$z_binned), include.lowest=TRUE)
  expect_true("z_binned" %in% names(p$layers[[3]]$data))
  expect_true("z_binned" %in% names(p$layers[[4]]$data))
  expect_magnet_full_group_counts(p, d, character(), "z_binned")
  expect_magnet_numeric_panel_data(p, d, "z", "z_binned")
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[3]]), 6)
  expect_true(all(ggplot2::ggplot_build(p)$data[[3]]$colour == "blue"))
})

test_that("numeric-conditioned magnets preserve second-axis groups, orders, and custom bins", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  for (group in c("b", "x")) {
    formula = stats::as.formula(paste("y_bin ~ a +", group, "| z"))
    breaks = list(z=c(-.5, .5))
    labels = list(z=c("low", "middle", "high"))
    if (group == "x") {
      breaks = c(list(x=c(-.5, .5)), breaks)
      labels = c(list(x=c("below", "near", "above")), labels)
    }
    p = flexplot(formula, data=d, jitter=FALSE, breaks=breaks, labels=labels)
    expected = d
    expected$z_binned = cut(d$z, c(min(d$z), -.5, .5, max(d$z)),
                             labels=c("low", "middle", "high"), include.lowest=TRUE)
    variables = "z"
    display_group = group
    if (group == "x") {
      display_group = "x_binned"
      variables = c("x", "z")
      expected$x_binned = cut(d$x, c(min(d$x), -.5, .5, max(d$x)),
                               labels=c("below", "near", "above"), include.lowest=TRUE)
    }
    fitted = p$layers[[3]]$data
    counts = p$layers[[4]]$data
    expect_identical(levels(p$layers[[2]]$data$a), levels(d$a))
    expect_identical(levels(fitted$z_binned), c("low", "middle", "high"))
    expect_magnet_full_group_counts(p, expected, display_group, "z_binned")
    expect_magnet_numeric_panel_data(p, expected, variables, "z_binned")
    built = ggplot2::ggplot_build(p)
    colors = built$plot$scales$get_scales("colour")$map(fitted[[display_group]])
    layout = built$layout$layout
    panels = layout$PANEL[match(as.character(fitted$z_binned), as.character(layout$z_binned))]
    fitted_keys = paste(panels, as.numeric(fitted$a), colors)
    markers = built$data[[3]]
    labels = built$data[[4]]
    drawn_keys = paste(markers$PANEL, round(markers$x), markers$colour)
    count_keys = paste(labels$PANEL, round(labels$x), labels$colour)
    expect_setequal(drawn_keys, fitted_keys)
    expect_equal(markers$y, unname(fitted$pred_prob[match(drawn_keys, fitted_keys)]))
    expect_equal(as.numeric(built$data[[3]]$x),
                 as.numeric(built$data[[4]]$x[match(drawn_keys, count_keys)]))
    raw = built$data[[2]]
    raw_keys = paste(raw$PANEL, round(raw$x), raw$colour)
    expect_equal(as.numeric(raw$x), as.numeric(labels$x[match(raw_keys, count_keys)]))
  }
})

test_that("mixed and two-numeric conditioning retain facet identifiers", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (formula in list(y_bin ~ a | b + z, y_bin ~ a | z + x)) {
    d = small
    d$b = ordered(d$b, levels=rev(levels(d$b)))
    breaks = list(z=c(-.5, .5))
    labels = list(z=c("low", "middle", "high"))
    if ("x" %in% all.vars(formula)) {
      breaks$x = c(-.5, .5)
      labels$x = c("below", "near", "above")
    }
    p = flexplot(formula, data=d, jitter=FALSE, breaks=breaks, labels=labels)
    d$z_binned = cut(d$z, c(min(d$z), -.5, .5, max(d$z)),
                      labels=c("low", "middle", "high"), include.lowest=TRUE)
    variables = "z"
    facets = c("b", "z_binned")
    if ("x" %in% all.vars(formula)) {
      d$x_binned = cut(d$x, c(min(d$x), -.5, .5, max(d$x)),
                        labels=c("below", "near", "above"), include.lowest=TRUE)
      variables = c("z", "x")
      facets = c("z_binned", "x_binned")
    }
    expect_magnet_full_group_counts(p, d, character(), facets)
    expect_magnet_numeric_panel_data(p, d, variables, facets)
    if ("b" %in% all.vars(formula)) {
      expect_true(is.ordered(p$layers[[3]]$data$b))
      expect_identical(levels(p$layers[[3]]$data$b), levels(d$b))
    }
  }
})

test_that("numeric-conditioned magnet sampling and visibility leave full-data fits unchanged", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  d = small
  d$row_id = seq_len(nrow(d))
  for (formula in list(y_bin ~ a | z, y_bin ~ a + b | z, y_bin ~ a + x | z)) {
    full = flexplot(formula, data=d, jitter=FALSE)
    aligned = flexplot(formula, data=d, sample=8, jitter=FALSE)
    aligned_built = ggplot2::ggplot_build(aligned)
    points = aligned_built$data[[2]]
    labels = aligned_built$data[[4]]
    point_keys = paste(points$PANEL, round(points$x), points$colour)
    count_keys = paste(labels$PANEL, round(labels$x), labels$colour)
    expect_equal(as.numeric(points$x), as.numeric(labels$x[match(point_keys, count_keys)]))
    for (show_raw in c(TRUE, FALSE)) {
      p = flexplot(formula, data=d, sample=8, raw.data=show_raw, alpha=.13,
                   jitter=c(.08, .02))
      built = ggplot2::ggplot_build(p)
      raw = p$layers[[2]]$data
      all_raw = full$layers[[2]]$data
      expect_equal(nrow(built$data[[2]]), 8)
      expect_true(all(built$data[[2]]$alpha == if (show_raw) .13 else 0))
      dy = sort(built$data[[2]]$y) - sort(raw$y_scaled)
      expect_true(all(abs(dy) <= .02))
      expect_true(any(abs(dy) > 0))
      expect_equal(raw$y_scaled, all_raw$y_scaled[match(raw$row_id, all_raw$row_id)])
      expect_equal(p$layers[[3]]$data, full$layers[[3]]$data)
      expect_equal(p$layers[[4]]$data, full$layers[[4]]$data)
      expect_equal(sum(p$layers[[4]]$data$n_total), nrow(d))
    }
    empty = flexplot(formula, data=d, sample=0, raw.data=FALSE, jitter=FALSE)
    expect_equal(nrow(ggplot2::ggplot_build(empty)$data[[2]]), 0)
    expect_equal(empty$layers[[3]]$data, full$layers[[3]]$data)
    expect_equal(empty$layers[[4]]$data, full$layers[[4]]$data)
  }
})

expect_magnet_model_overlay = function(p, models, display_group=NULL, facets=character()) {
  built = ggplot2::ggplot_build(p)
  prediction = p$layers[[5]]$data
  overlay = built$data[[5]]
  fitted = built$data[[3]]
  raw = built$data[[2]]
  layout = built$layout$layout
  panels = rep(as.character(layout$PANEL[1]), nrow(prediction))
  if (length(facets) > 0) {
    panel_keys = do.call(paste, layout[facets])
    panels = as.character(layout$PANEL[match(do.call(paste, prediction[facets]), panel_keys)])
  }
  expect_false(anyNA(panels))
  shape_scale = built$plot$scales$get_scales("shape")
  shapes = shape_scale$map(prediction$model)
  colors = rep("black", nrow(prediction))
  if (!is.null(display_group)) {
    color_scale = built$plot$scales$get_scales("colour")
    colors = color_scale$map(prediction[[display_group]])
    expect_identical(color_scale$get_limits(), levels(droplevels(prediction[[display_group]])))
    expect_false(any(as.character(prediction$model) %in% color_scale$get_limits()))
    expect_equal(built$plot$labels$colour, display_group)
    expect_equal(sort(raw$colour),
                 sort(color_scale$map(p$layers[[2]]$data[[display_group]])))
    expect_equal(sort(fitted$colour),
                 sort(color_scale$map(p$layers[[3]]$data[[display_group]])))
  }
  expected_keys = paste(panels, as.numeric(prediction$a), colors, shapes)
  drawn_keys = paste(overlay$PANEL, round(overlay$x), overlay$colour, overlay$shape)
  rows = match(drawn_keys, expected_keys)
  expect_false(anyNA(rows))
  expect_equal(nrow(overlay), nrow(prediction))
  expect_setequal(drawn_keys, expected_keys)
  expect_equal(overlay$y, prediction$prediction[rows])

  for (name in names(models)) {
    model_rows = as.character(prediction$model) == name
    expect_true(any(model_rows))
    expected = stats::predict(models[[name]], newdata=prediction[model_rows, ], type="response")
    expect_equal(prediction$prediction[model_rows], unname(expected))
  }
  # Match raw data, internal fits, and overlays by panel, category, and group color.
  fitted_keys = paste(fitted$PANEL, round(fitted$x), fitted$colour)
  overlay_keys = paste(overlay$PANEL, round(overlay$x), overlay$colour)
  raw_keys = paste(raw$PANEL, round(raw$x), raw$colour)
  if (is.null(display_group)) {
    fitted_keys = paste(fitted$PANEL, round(fitted$x))
    overlay_keys = paste(overlay$PANEL, round(overlay$x))
    raw_keys = paste(raw$PANEL, round(raw$x))
    expect_true(all(fitted$colour == "blue"))
  }
  expect_false(anyNA(match(overlay_keys, fitted_keys)))
  expect_equal(as.numeric(overlay$x), as.numeric(fitted$x[match(overlay_keys, fitted_keys)]))
  expect_equal(as.numeric(raw$x), as.numeric(fitted$x[match(raw_keys, fitted_keys)]))
  expect_equal(length(unique(overlay$shape)), length(models))
  expect_false(any(overlay$shape %in% c(1, 19)))
  expect_equal(shape_scale$name, "Model")
  expect_setequal(shape_scale$get_limits(), names(models))
  expect_false(p$layers[[5]]$show.legend[["colour"]])
  expect_true(p$layers[[5]]$show.legend[["shape"]])
  invisible(built)
}

test_that("grouped magnet model overlays retain group colors and separate model shapes", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  m = stats::glm(y_bin ~ a + b, data=d, family=stats::binomial(),
                 weights=seq_len(nrow(d)))
  m2 = stats::glm(y_bin ~ a, data=d, family=stats::binomial())
  base = flexplot(y_bin ~ a + b, data=d, jitter=FALSE)
  for (two_models in c(FALSE, TRUE)) {
    second = if (two_models) m2 else NULL
    p = compare.fits(y_bin ~ a + b, data=d, model1=m, model2=second, jitter=FALSE)
    models = list(m)
    if (two_models) models = c(models, list(m2))
    names(models) = levels(p$layers[[5]]$data$model)
    # Names sort alphabetically: m precedes second.
    expect_magnet_model_overlay(p, models, "b")
    expect_equal(p$layers[[2]]$data, base$layers[[2]]$data)
    expect_equal(p$layers[[3]]$data, base$layers[[3]]$data)
    expect_equal(p$layers[[4]]$data, base$layers[[4]]$data)
    expect_identical(levels(p$layers[[5]]$data$a), levels(d$a))
    expect_identical(levels(p$layers[[5]]$data$b), levels(d$b))
    legends = cowplot::get_plot_component(p, "guide-box", return_all=TRUE)
    n_guides = 0L
    for (legend in legends) {
      if (!is.null(legend$layout)) n_guides = n_guides + sum(legend$layout$name == "guides")
    }
    expect_equal(n_guides, 2)
  }
})

test_that("magnet model overlays preserve numeric bins and categorical or numeric panels", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  for (formula in list(y_bin ~ a + b | z, y_bin ~ a + x | b,
                       y_bin ~ a + x | z)) {
    m = stats::glm(stats::reformulate(all.vars(formula)[-1], response="y_bin"),
                   data=small, family=stats::binomial(), weights=seq_len(nrow(small)))
    p = compare.fits(formula, data=small, model1=m, jitter=FALSE)
    group = if ("x" %in% all.vars(formula)) "x_binned" else "b"
    facets = if ("z" %in% all.vars(formula)) "z_binned" else "b"
    models = stats::setNames(list(m), levels(p$layers[[5]]$data$model))
    expect_magnet_model_overlay(p, models, group, facets)
    sampled = compare.fits(formula, data=small, model1=m, jitter=FALSE, sample=8)
    expect_magnet_model_overlay(sampled, models, group, facets)
    expect_equal(sampled$layers[[5]]$data, p$layers[[5]]$data)
    expect_equal(sampled$layers[[3]]$data, p$layers[[3]]$data)
    expect_equal(sampled$layers[[4]]$data, p$layers[[4]]$data)
    m2 = stats::glm(y_bin ~ a, data=small, family=stats::binomial())
    two = compare.fits(formula, data=small, model1=m, model2=m2, jitter=FALSE)
    models = stats::setNames(list(m, m2), levels(two$layers[[5]]$data$model))
    expect_magnet_model_overlay(two, models, group, facets)
  }
})

test_that("ungrouped magnet model overlays retain supplied probabilities and panel membership", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (formula in list(y_bin ~ a, y_bin ~ a | b, y_bin ~ a | z)) {
    m = stats::glm(stats::reformulate(all.vars(formula)[-1], response="y_bin"),
                   data=small, family=stats::binomial(), weights=seq_len(nrow(small)))
    p = compare.fits(formula, data=small, model1=m, jitter=FALSE)
    facets = if ("z" %in% all.vars(formula)) "z_binned" else
      if ("b" %in% all.vars(formula)) "b" else character()
    models = stats::setNames(list(m), levels(p$layers[[5]]$data$model))
    expect_magnet_model_overlay(p, models, facets=facets)
  }
})

expect_magnet_stable_slots = function(p, group) {
  built = ggplot2::ggplot_build(p)
  group_levels = levels(droplevels(p$layers[[4]]$data[[group]]))
  colors = built$plot$scales$get_scales("colour")
  expect_identical(colors$get_limits(), group_levels)
  expect_identical(built$layout$panel_scales_x[[1]]$get_limits(),
                   levels(droplevels(p$layers[[4]]$data$a)))
  layout = built$layout$layout
  facets = intersect(names(layout), names(p$layers[[4]]$data))

  for (layer in 2:length(p$layers)) {
    source = p$layers[[layer]]$data
    drawn = built$data[[layer]]
    expect_equal(nrow(drawn), nrow(source))
    if (nrow(source) == 0) next
    index = match(as.character(source[[group]]), group_levels)
    offsets = .5 * ((index - .5) / length(group_levels) - .5)
    expected_x = as.numeric(source$a) + offsets
    panel_rows = match(do.call(paste, source[facets]), do.call(paste, layout[facets]))
    expected_keys = paste(layout$PANEL[panel_rows], as.numeric(source$a),
                           colors$map(source[[group]]))
    drawn_keys = paste(drawn$PANEL, round(drawn$x), drawn$colour)
    expect_equal(sort(drawn_keys), sort(expected_keys))
    rows = match(drawn_keys, expected_keys)
    expect_false(anyNA(rows))
    expect_equal(as.numeric(drawn$x), expected_x[rows])
    expect_identical(rlang::as_label(p$layers[[layer]]$mapping$group), group)
    expect_equal(length(unique(drawn$group)), length(unique(source[[group]])))
    for (id in unique(drawn$group)) {
      expect_equal(length(unique(drawn$colour[drawn$group == id])), 1)
    }
  }
  invisible(built)
}

test_that("sampled conditioned magnet layers retain full-data group slots", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  for (ordered_group in c(FALSE, TRUE)) {
    d = small
    d$row_id = seq_len(nrow(d))
    d$a = factor(d$a, levels=c("b", "a"), ordered=ordered_group)
    d$b = factor(d$b, levels=c("z", "x", "y"), ordered=ordered_group)
    d$panel = factor(ifelse(d$z < 0, "low", "high"), levels=c("high", "low"))
    for (group in c("b", "x")) {
      formula = stats::as.formula(paste("y_bin ~ a +", group, "| panel"))
      full = flexplot(formula, data=d, jitter=FALSE)
      # Fix the sample independently of plot construction and ggplot RNG use.
      set.seed(123)
      sampled_ids = sample(seq_len(nrow(d)), 8)
      set.seed(123)
      p = flexplot(formula, data=d, sample=8, jitter=FALSE)
      display_group = if (group == "x") "x_binned" else "b"
      built = expect_magnet_stable_slots(p, display_group)
      expect_magnet_stable_slots(full, display_group)
      raw = p$layers[[2]]$data
      all_raw = full$layers[[2]]$data
      expect_identical(raw$row_id, sampled_ids)
      expect_equal(raw$y_scaled, all_raw$y_scaled[match(raw$row_id, all_raw$row_id)])
      expect_identical(levels(raw$a), levels(d$a))
      expect_identical(levels(raw[[display_group]]), levels(all_raw[[display_group]]))
      present = table(raw$panel, raw[[display_group]])
      expect_true(any(present == 0))
      expect_equal(p$layers[[3]]$data, full$layers[[3]]$data)
      expect_equal(p$layers[[4]]$data, full$layers[[4]]$data)
      expect_equal(sum(p$layers[[4]]$data$n_total), nrow(d))
      expect_equal(sort(built$data[[2]]$y), sort(raw$y_scaled))

      model_formula = stats::reformulate(c("a", group, "panel"), response="y_bin")
      m = stats::glm(model_formula, data=d, family=stats::binomial())
      set.seed(123)
      overlay = compare.fits(formula, data=d, model1=m, sample=8, jitter=FALSE)
      expect_magnet_stable_slots(overlay, display_group)
      prediction = overlay$layers[[5]]$data
      expect_equal(prediction$prediction,
                   unname(stats::predict(m, newdata=prediction, type="response")))
      expect_identical(overlay$layers[[2]]$data$row_id, sampled_ids)
      expect_equal(overlay$layers[[3]]$data, full$layers[[3]]$data)
      expect_equal(overlay$layers[[4]]$data, full$layers[[4]]$data)
      complete_overlay = compare.fits(formula, data=d, model1=m, jitter=FALSE)
      expect_equal(overlay$layers[[5]]$data, complete_overlay$layers[[5]]$data)
    }
  }
})

test_that("categorical panels retain numeric magnet slots for sparse categories", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  # This also reproduces the original sample=8, seed=123 offset failure.
  m = stats::glm(y_bin ~ a + x + b, data=small, family=stats::binomial())
  p = compare.fits(y_bin ~ a + x | b, data=small, model1=m,
                   sample=8, jitter=FALSE)
  built = expect_magnet_stable_slots(p, "x_binned")
  raw = p$layers[[2]]$data
  expect_true(any(table(raw$b, raw$x_binned) == 0))
  full = compare.fits(y_bin ~ a + x | b, data=small, model1=m, jitter=FALSE)
  fitted = full$layers[[3]]$data
  expect_true(any(table(fitted$b, fitted$a, fitted$x_binned) == 0))
  expect_magnet_stable_slots(full, "x_binned")
  for (layer in 3:5) expect_equal(p$layers[[layer]]$data, full$layers[[layer]]$data)
  expect_equal(nrow(built$data[[2]]), 8)
})

test_that("empty and single-point magnet samples preserve full-data slots and colors", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  d = small
  d$b = factor(d$b, levels=c("z", "x", "y"))
  d$panel = factor(ifelse(d$z < 0, "low", "high"))
  for (formula in list(y_bin ~ a + b | panel, y_bin ~ a + x | b)) {
    group = if ("panel" %in% all.vars(formula)) "b" else "x_binned"
    full = flexplot(formula, data=d, jitter=FALSE)
    full_built = ggplot2::ggplot_build(full)
    for (sample_size in c(0, 1)) {
      set.seed(123)
      p = flexplot(formula, data=d, jitter=FALSE, sample=sample_size)
      built = expect_magnet_stable_slots(p, group)
      expect_equal(nrow(built$data[[2]]), sample_size)
      for (layer in 3:4) {
        expect_equal(p$layers[[layer]]$data, full$layers[[layer]]$data)
        expect_equal(built$data[[layer]], full_built$data[[layer]])
      }
    }
  }
})
