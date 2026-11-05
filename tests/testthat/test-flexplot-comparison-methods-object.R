comparison_unexpected_renderer = function(...) stop("Unexpected native rendering")

comparison_x_bins = function(data, variable="x") {
  edges = unique(stats::quantile(data[[variable]], seq(0, 1, length.out=10)))
  list(group=cut(data[[variable]], edges, include.lowest=TRUE, labels=FALSE),
       centers=(head(edges, -1) + tail(edges, -1)) / 2)
}

comparison_plot_predictions = function(p) {
  for (layer in p$layers) {
    if (is.data.frame(layer$data) && all(c("prediction", "model") %in% names(layer$data))) {
      return(layer$data)
    }
  }
  stop("No model prediction layer found")
}

test_that("observed categorical predictions average actual correlated observations", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$w = 2 * d$x + d$z
  m = stats::lm(y ~ a + w, data=d)
  observed = compare_fits(y ~ a, d, m, return.preds=TRUE)
  expected = tapply(stats::predict(m, newdata=d), d$a, mean)
  expect_equal(observed$prediction, as.numeric(expected[as.character(observed$a)]))
  expect_setequal(names(observed), c("a", "prediction", "model"))
  reference = compare_fits(y ~ a, d, m, return.preds=TRUE, prediction_method="reference")
  expect_equal(reference$w, rep(mean(d$w), nrow(reference)))
  expect_equal(reference$prediction, unname(stats::predict(m, newdata=reference)))
  expect_true(any(abs(observed$prediction - reference$prediction[match(observed$a, reference$a)]) > .001))
})

test_that("continuous observed predictions use actual responses within common x bins", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ x * z + I(x^2), data=small)
  actual = compare_fits(y ~ x, small, m, return.preds=TRUE)
  bins = comparison_x_bins(small)
  expected = tapply(stats::predict(m, newdata=small), bins$group, mean)
  expect_equal(actual$x, unname(bins$centers[as.integer(names(expected))]))
  expect_equal(actual$prediction, as.numeric(expected))
  expect_equal(nrow(actual), length(unique(bins$group)))
  reference = compare_fits(y ~ x, small, m, return.preds=TRUE, prediction_method="reference")
  expect_equal(reference$prediction, unname(stats::predict(m, newdata=reference)))
  expect_equal(reference$z, rep(mean(small$z), nrow(reference)))
})

test_that("binomial predictions average probabilities rather than inverse averaged links", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::glm(y_bin ~ a + b, data=small, family=stats::binomial())
  actual = compare_fits(y_bin ~ a, small, m, return.preds=TRUE)
  expected = tapply(stats::predict(m, newdata=small, type="response"), small$a, mean)
  links = tapply(stats::predict(m, newdata=small, type="link"), small$a, mean)
  expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]))
  expect_true(any(abs(actual$prediction - stats::plogis(links[as.character(actual$a)])) > .001))
  actual_links = compare_fits(y_bin ~ a, small, m, return.preds=TRUE, pred.type="link")
  expect_equal(actual_links$prediction, as.numeric(links[as.character(actual_links$a)]))
})

test_that("reference profiles use means and modes for both models without extra markers", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small[c(which(small$b == "x"), which(small$b != "x")), ]
  m1 = stats::lm(y ~ a + z, data=d)
  m2 = stats::lm(y ~ a + b, data=d)
  actual = compare_fits(y ~ a, d, m1, m2, return.preds=TRUE, prediction_method="reference")
  mode = names(which.max(table(d$b)))
  expect_false(as.character(d$b[1]) == mode)
  expect_true(all(as.character(actual$b) == mode))
  expect_equal(actual$z, rep(mean(d$z), nrow(actual)))
  expect_equal(nrow(actual), 2 * nlevels(d$a))
  for (name in unique(actual$model)) {
    fit = if (name == "m1 (lm)") m1 else m2
    rows = actual$model == name
    expect_equal(actual$prediction[rows], unname(stats::predict(fit, newdata=actual[rows, ])))
  }
})

test_that("two models use the intersection of estimation samples and missing-value eligibility", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$z[4] = NA
  d$b[7] = NA
  m1 = stats::lm(y ~ x + z, data=d)
  m2 = stats::lm(y ~ x + b, data=d)
  eligible = d[-c(4, 7), ]
  actual = compare_fits(y ~ x, d, m1, m2, return.preds=TRUE)
  bins = comparison_x_bins(eligible)
  for (name in unique(actual$model)) {
    fit = if (name == "m1 (lm)") m1 else m2
    expected = tapply(stats::predict(fit, newdata=eligible), bins$group, mean)
    rows = actual$model == name
    expect_equal(actual$prediction[rows], as.numeric(expected))
  }
  p = compare_fits(y ~ x, d, m1, m2, jitter=FALSE)
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), nrow(eligible))
})

test_that("subset estimation rows stay aligned after input data are reordered", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::lm(y ~ x + z, data=small, subset=1:20)
  m2 = stats::lm(y ~ x + b, data=small, subset=10:27)
  a = compare_fits(y ~ x, small, m1, m2, return.preds=TRUE)
  reordered = small[rev(seq_len(nrow(small))), ]
  b = compare_fits(y ~ x, reordered, m1, m2, return.preds=TRUE)
  expect_equal(a, b)
  eligible = small[10:20, ]
  bins = comparison_x_bins(eligible)
  expected = tapply(stats::predict(m1, newdata=eligible), bins$group, mean)
  expect_equal(a$prediction[a$model == "m1 (lm)"], as.numeric(expected))

  rownames(reordered) = rownames(small)
  expect_error(compare_fits(y ~ x, reordered, m1, m2, return.preds=TRUE), "Cannot align model observations")
  disjoint = stats::lm(y ~ x, data=small, subset=21:27)
  expect_error(compare_fits(y ~ x, small, m1, disjoint, return.preds=TRUE), "no common eligible")
})

test_that("display groups and conditioning use shared breaks without fabricated combinations", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = ordered(d$a, levels=rev(levels(d$a)))
  m = stats::lm(y ~ a + b + z, data=d)
  breaks = list(z=c(-.5, .5))
  labels = list(z=c("low", "middle", "high"))
  actual = compare_fits(y ~ a + b | z, d, m, return.preds=TRUE, breaks=breaks, labels=labels)
  independent = d
  independent$z_binned = cut(d$z, c(min(d$z), -.5, .5, max(d$z)),
                              include.lowest=TRUE, labels=labels$z)
  independent$prediction = stats::predict(m, newdata=d)
  expected = stats::aggregate(prediction ~ a + b + z_binned, independent, mean)
  keys = paste(actual$a, actual$b, actual$z_binned)
  expected_keys = paste(expected$a, expected$b, expected$z_binned)
  expect_setequal(keys, expected_keys)
  expect_equal(actual$prediction, expected$prediction[match(keys, expected_keys)])
  expect_identical(levels(actual$a), levels(d$a))
  expect_identical(levels(actual$z_binned), labels$z)
})

test_that("unused levels and empty displayed combinations do not create predictions", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small[!(small$a == "b" & small$b == "y"), ]
  d$a = factor(d$a, levels=c("b", "a", "unused"), ordered=TRUE)
  m = stats::lm(y ~ a + b + z, data=d)
  actual = compare_fits(y ~ a + b, d, m, return.preds=TRUE)
  expect_false(any(as.character(actual$a) == "unused"))
  expect_false(any(actual$a == "b" & actual$b == "y"))
  expect_equal(nrow(actual), nrow(unique(d[c("a", "b")])))
  expected = stats::aggregate(stats::predict(m, newdata=d), d[c("a", "b")], mean)
  rows = match(paste(actual$a, actual$b), paste(expected$a, expected$b))
  expect_equal(actual$prediction, expected$x[rows])
})

test_that("both exported comparison names and positional calls are identical", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ a + z, data=small)
  expect_identical(compare_fits, compare.fits)
  expect_identical(formals(compare_fits), formals(compare.fits))
  expect_identical(compare_fits(y ~ a, small, m, return.preds=TRUE),
                   compare.fits(y ~ a, small, m, return.preds=TRUE))
  positional = compare_fits(y ~ a, small, m, NULL, TRUE, FALSE, FALSE, "response", 50, 3, "reference")
  expect_identical(positional, compare_fits(y ~ a, small, m, return.preds=TRUE,
                                           prediction_method="reference"))
  expect_error(compare_fits(y ~ a, small, m, prediction_method="partial"),
                'prediction_method must be "observed" or "reference"', fixed=TRUE)
  expect_equal(ggplot2::ggplot_build(compare_fits(y ~ a, small, m, jitter=FALSE))$data,
               ggplot2::ggplot_build(compare.fits(y ~ a, small, m, jitter=FALSE))$data)
})

test_that("prediction data and plotted values ignore the obsolete renderer option", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ x + z, data=small)
  for (method in c("observed", "reference")) {
    options(flexplot.object_renderer=TRUE)
    native_values = compare_fits(y ~ x, small, m, return.preds=TRUE, prediction_method=method)
    native = compare_fits(y ~ x, small, m, prediction_method=method, jitter=FALSE)
    options(flexplot.object_renderer=FALSE)
    legacy_values = compare_fits(y ~ x, small, m, return.preds=TRUE, prediction_method=method)
    legacy = compare_fits(y ~ x, small, m, prediction_method=method, jitter=FALSE)
    expect_identical(native_values, legacy_values)
    expect_equal(comparison_plot_predictions(native)$prediction,
                 comparison_plot_predictions(legacy)$prediction)
  }
  options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(flexplot_render_object=comparison_unexpected_renderer, .package="flexplot")
  expect_error(compare_fits(y ~ x, small, m, plot.string=TRUE),
                 "String-based plotting has been retired", fixed=TRUE)
})

test_that("observed group standard errors use covariance of the averaged model gradient", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ a + z, data=small)
  p = compare_fits(y ~ a, small, m, return.preds=TRUE, report.se=TRUE)
  matrix = stats::model.matrix(m)
  for (i in seq_len(nrow(p))) {
    gradient = colMeans(matrix[small$a == p$a[i], , drop=FALSE])
    expected = sqrt(drop(gradient %*% stats::vcov(m) %*% gradient))
    expect_equal(p$se[i], expected)
  }
  reference = compare_fits(y ~ a, small, m, return.preds=TRUE,
                           prediction_method="reference", report.se=TRUE)
  expect_equal(reference$se, unname(stats::predict(m, newdata=reference, se.fit=TRUE)$se.fit))
  interval = stats::predict(m, newdata=reference, interval="confidence")
  expect_equal(reference$prediction.fit, unname(interval[, "fit"]))
  expect_equal(reference$prediction.lwr, unname(interval[, "lwr"]))
  expect_equal(reference$prediction.upr, unname(interval[, "upr"]))
})

test_that("grouped magnet comparison summaries ignore the obsolete renderer option", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::glm(y_bin ~ a + b + z, small, family=stats::binomial())
  m2 = stats::glm(y_bin ~ a + z, small, family=stats::binomial())
  for (method in c("observed", "reference")) {
    options(flexplot.object_renderer=TRUE)
    native = compare_fits(y_bin ~ a + b | z, small, m1, m2,
                          prediction_method=method, jitter=FALSE)
    options(flexplot.object_renderer=FALSE)
    legacy = compare_fits(y_bin ~ a + b | z, small, m1, m2,
                          prediction_method=method, jitter=FALSE)
    actual = comparison_plot_predictions(legacy)
    expected = comparison_plot_predictions(native)
    rows = match(paste(actual$a, actual$b, actual$z_binned, actual$model),
                  paste(expected$a, expected$b, expected$z_binned, expected$model))
    expect_false(anyNA(rows))
    expect_equal(actual$prediction, expected$prediction[rows])
    expect_no_warning({ built = ggplot2::ggplot_build(legacy) })
    expect_equal(length(unique(built$layout$layout$COL)), 2)
    expect_equal(length(unique(built$layout$layout$ROW)), 3)
  }
  options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(flexplot_render_object=comparison_unexpected_renderer, .package="flexplot")
  expect_error(compare_fits(y_bin ~ a + b, small, m1, plot.string=TRUE),
                 "String-based plotting has been retired", fixed=TRUE)
})

test_that("observed magnet averages preserve groups and model-column conditioning facets", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::glm(y_bin ~ a + b + z, small, family=stats::binomial())
  m2 = stats::glm(y_bin ~ a + z, small, family=stats::binomial())
  p = compare_fits(y_bin ~ a + b | z, small, m1, m2, jitter=FALSE)
  source = p$layers[[2]]$data
  predicted = p$layers[[4]]$data
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_no_warning(ggplot2::ggplotGrob(p))
  expect_equal(length(unique(built$layout$layout$COL)), 2)
  expect_equal(length(unique(built$layout$layout$ROW)), 3)
  expect_equal(nrow(built$data[[2]]), 2 * nrow(small))
  expect_true(all(built$data[[2]]$shape == 1))
  expect_true(all(built$data[[4]]$shape == 17))
  for (name in unique(predicted$model)) {
    fit = if (name == "m1 (glm)") m1 else m2
    independent = source
    independent$prediction = stats::predict(fit, newdata=source, type="response")
    expected = stats::aggregate(prediction ~ a + b + z_binned, independent, mean)
    block = predicted[predicted$model == name, ]
    rows = match(paste(block$a, block$b, block$z_binned), paste(expected$a, expected$b, expected$z_binned))
    expect_equal(block$prediction, expected$prediction[rows])
  }
})

test_that("factor-binomial outcomes retain unrounded response-scale averages", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::glm(y_ord ~ x, data=small_logistic, family=stats::binomial())
  actual = compare_fits(y_ord ~ x, small_logistic, m, return.preds=TRUE)
  bins = comparison_x_bins(small_logistic)
  expected = tapply(stats::predict(m, newdata=small_logistic, type="response"), bins$group, mean)
  expect_equal(actual$prediction, as.numeric(expected))
  expect_true(all(actual$prediction >= 0 & actual$prediction <= 1))
})

test_that("mixed-model predictions respect the selected observations and prediction streams", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(42)
  d = lme4::sleepstudy
  m1 = lme4::lmer(Reaction ~ Days + (Days | Subject), data=d)
  m2 = lme4::lmer(Reaction ~ Days + (1 | Subject), data=d)
  set.seed(42)
  actual = compare_fits(Reaction ~ Days | Subject, d, m1, return.preds=TRUE,
                        re=TRUE, clusters=2)
  expect_equal(length(unique(actual$Subject)), 2)
  expect_setequal(actual$model, c("fixed effects", "random effects"))
  selected = d[d$Subject %in% actual$Subject, ]
  bins = comparison_x_bins(selected, "Days")
  for (type in unique(actual$model)) {
    re_form = if (type == "fixed effects") NA else NULL
    value = stats::predict(m1, newdata=selected, re.form=re_form)
    expected = stats::aggregate(value, list(bin=bins$group, Subject=selected$Subject), mean)
    block = actual[actual$model == type, ]
    rows = match(paste(block$Days, block$Subject), paste(bins$centers[expected$bin], expected$Subject))
    expect_false(anyNA(rows))
    expect_equal(block$prediction, expected$x[rows])
  }
  set.seed(42)
  pair = compare_fits(Reaction ~ Days | Subject, d, m1, m2, re=TRUE,
                      return.preds=TRUE, clusters=2)
  expect_setequal(pair$model, c("m1 (lmerMod)", "m2 (lmerMod)"))
  expect_setequal(pair$Subject, actual$Subject)
  for (name in unique(pair$model)) {
    fit = if (name == "m1 (lmerMod)") m1 else m2
    expected = stats::aggregate(stats::predict(fit, newdata=selected, re.form=NULL),
                                list(bin=bins$group, Subject=selected$Subject), mean)
    block = pair[pair$model == name, ]
    rows = match(paste(block$Days, block$Subject), paste(bins$centers[expected$bin], expected$Subject))
    expect_equal(block$prediction, expected$x[rows])
  }
  expect_error(compare_fits(Reaction ~ Days, d, m1, return.preds=TRUE, report.se=TRUE), "report.se=TRUE")
})

test_that("glmer predictions honor newdata and cluster selection", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(12)
  d = lme4::sleepstudy
  effect = rep(seq(-2, 2, length.out=18), each=10)
  d$binary = stats::rbinom(nrow(d), 1, stats::plogis((d$Days - 4) / 4 + effect))
  m = lme4::glmer(binary ~ Days + (1 | Subject), data=d, family=stats::binomial())
  actual = compare_fits(binary ~ Days | Subject, d, m, return.preds=TRUE, clusters=2)
  expect_equal(length(unique(actual$Subject)), 2)
  selected = d[d$Subject %in% actual$Subject, ]
  bins = comparison_x_bins(selected, "Days")
  expected = stats::aggregate(stats::predict(m, newdata=selected, type="response", re.form=NA),
                              list(bin=bins$group, Subject=selected$Subject), mean)
  rows = match(paste(actual$Days, actual$Subject), paste(bins$centers[expected$bin], expected$Subject))
  expect_equal(actual$prediction, expected$x[rows])
})

test_that("regression trees and forests use predictions for the actual supplied rows", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(10)
  models = list(
    tree=rpart::rpart(y ~ a + x + z, data=small),
    forest=party::cforest(y ~ a + x + z, data=small,
                          controls=party::cforest_unbiased(ntree=15, mtry=2))
  )
  if (requireNamespace("randomForest", quietly=TRUE)) {
    models$randomForest = randomForest::randomForest(y ~ a + x + z, data=small, ntree=20)
  }
  reordered = small[rev(seq_len(nrow(small))), ]
  for (fit in models) {
    actual = compare_fits(y ~ a, reordered, fit, return.preds=TRUE)
    expected = tapply(stats::predict(fit, newdata=reordered), reordered$a, mean)
    expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]))
  }
})

test_that("binary trees and forests average probabilities for the second response level", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(10)
  d = small
  d$binary = factor(d$y_bin, levels=c(0, 1), labels=c("no", "yes"))
  tree = rpart::rpart(binary ~ a + x + z, data=d)
  actual = compare_fits(binary ~ a, d, tree, return.preds=TRUE)
  expected = tapply(stats::predict(tree, newdata=d, type="prob")[, "yes"], d$a, mean)
  expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]))
  forest = party::cforest(binary ~ a + x + z, data=d,
                           controls=party::cforest_unbiased(ntree=15, mtry=2))
  actual = compare_fits(binary ~ a, d, forest, return.preds=TRUE)
  probability = stats::predict(forest, newdata=d, type="prob")
  probability = if (is.list(probability)) do.call(rbind, probability)[, "binary.yes"] else probability[, "yes"]
  expected = tapply(probability, d$a, mean)
  expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]))
})

test_that("numeric ordinal prediction scores retain the existing polr convention", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$ordinal = cut(d$y, stats::quantile(d$y, c(0, 1/3, 2/3, 1)),
                   labels=c("1", "2", "3"), include.lowest=TRUE, ordered_result=TRUE)
  m = MASS::polr(ordinal ~ a + z, data=d, Hess=TRUE)
  actual = compare_fits(ordinal ~ a, d, m, return.preds=TRUE)
  value = as.numeric(as.character(stats::predict(m, newdata=d, type="class")))
  expected = tapply(value, d$a, mean)
  expect_equal(actual$prediction, as.numeric(expected[as.character(actual$a)]))
  expect_no_error(ggplot2::ggplot_build(compare_fits(ordinal ~ a, d, m)))
})

test_that("model transformations use original plotting coordinates and ambiguous plot transforms fail", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ I(x^2) + z, data=small)
  expect_error(compare_fits(y ~ I(x^2), small, m, return.preds=TRUE), "original variable names")
  expect_error(compare_fits(z ~ x, small, m, return.preds=TRUE), "same outcome")
  expect_no_error(compare_fits(y ~ x, small, m, return.preds=TRUE))
})

test_that("mixed return.preds uses no renderer and shares cluster selection across options", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(7)
  d = lme4::sleepstudy
  m = lme4::lmer(Reaction ~ Days + (1 | Subject), data=d)
  testthat::local_mocked_bindings(flexplot=comparison_unexpected_renderer, .package="flexplot")
  set.seed(7)
  native = compare_fits(Reaction ~ Days | Subject, d, m, return.preds=TRUE, clusters=2)
  options(flexplot.object_renderer=FALSE)
  set.seed(7)
  legacy = compare_fits(Reaction ~ Days | Subject, d, m, return.preds=TRUE, clusters=2)
  expect_identical(native, legacy)
})

test_that("intercept-only comparisons retain estimates and distribution plots", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m = stats::lm(y ~ 1, data=small)
  for (method in c("observed", "reference")) {
    actual = compare_fits(y ~ 1, small, m, return.preds=TRUE,
                          report.se=TRUE, prediction_method=method)
    expect_equal(nrow(actual), 1)
    expect_equal(actual$prediction, mean(small$y))
    expect_equal(actual$se, unname(summary(m)$coefficients[1, 2]))
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      expect_no_error(ggplot2::ggplot_build(compare_fits(y ~ 1, small, m,
                                                        prediction_method=method)))
    }
  }
  expect_equal(estimates(m)$Mean, mean(small$y))
})
