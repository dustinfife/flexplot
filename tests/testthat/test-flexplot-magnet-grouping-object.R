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
