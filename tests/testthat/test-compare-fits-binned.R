unexpected_binned_model_refit = function(...) stop("Unexpected model refit")

test_that("matched grouping preserves nondefault binomial links and supported second axes", {
  d = small
  d$weight = rep(c(1, 2, 3), length.out=nrow(d))
  d$log_exposure = .1 * sin(seq_len(nrow(d)))
  model = glm(y_bin ~ x + a + b + offset(log_exposure), d,
                family=binomial(link="probit"), weights=weight)
  p = compare.fits(y_bin ~ x + b | a, d, model, prediction_method="observed",
                     observation.type="binned")
  observed = p$layers[[3]]$data
  bins = cut(d$x, unique(quantile(d$x, seq(0, 1, length.out=10))), include.lowest=TRUE, labels=FALSE)
  edges = unique(quantile(d$x, seq(0, 1, length.out=10)))
  mid = (head(edges, -1) + tail(edges, -1)) / 2
  expected = aggregate(cbind(observed=d$y_bin, predicted=predict(model, d, type="response")),
                         list(mid=mid[bins], a=d$a, b=d$b), mean)
  rows = match(paste(observed$x, observed$a, observed$b), paste(expected$mid, expected$a, expected$b))
  expect_equal(observed$proportion, expected$observed[rows])
  expect_equal(sum(observed$count), nrow(d))
  prediction = compare_fits(y_bin ~ x + b | a, d, model, prediction_method="observed", return.preds=TRUE)
  rows = match(paste(prediction$x, prediction$a, prediction$b), paste(expected$mid, expected$a, expected$b))
  expect_equal(prediction$prediction, expected$predicted[rows], tolerance=1e-10)
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_equal(built$data[[3]]$colour, built$plot$scales$get_scales("colour")$map(observed$b))
  expect_no_message(ggplot2::ggplotGrob(p))
})

test_that("matched binomial observations and model means use the same eligible rows", {
  d = small
  d$z[4] = NA
  full = glm(y_bin ~ x + a + z, d, family=binomial(), subset=1:24)
  reduced = glm(y_bin ~ x, d, family=binomial(), subset=3:27)
  eligible = d[setdiff(3:24, 4), ]
  edges = unique(quantile(eligible$x, seq(0, 1, length.out=10)))
  bins = cut(eligible$x, edges, include.lowest=TRUE, labels=FALSE)
  midpoints = (head(edges, -1) + tail(edges, -1)) / 2
  expected = aggregate(eligible$y_bin, list(x=midpoints[bins], a=eligible$a), mean)
  names(expected)[3] = "proportion"
  counts = aggregate(eligible$y_bin, list(x=midpoints[bins], a=eligible$a), length)[[3]]
  p = compare_fits(y_bin ~ x | a, d, full, reduced, prediction_method="observed",
                     observation.type="binned")
  built = ggplot2::ggplot_build(p)
  observed = p$layers[[3]]$data
  rows = match(paste(observed$x, observed$a), paste(expected$x, expected$a))
  expect_false(anyNA(rows))
  expect_equal(observed$proportion, expected$proportion[rows])
  expect_equal(observed$count, counts[rows])
  expect_equal(sum(observed$count), nrow(eligible))
  expect_true(any(observed$count == 1))
  expect_true(all(built$data[[1]]$alpha == 0))
  expect_true(all(built$data[[3]]$shape == 21))
  expect_true(all(built$data[[3]]$fill == "white"))
  expect_false("model" %in% names(observed))
  expect_s3_class(p$layers[[4]]$geom, "GeomPoint")
  panels = built$layout$layout
  expect_equal(as.character(built$data[[3]]$PANEL),
                 as.character(panels$PANEL[match(observed$a, panels$a)]))
  prediction = compare_fits(y_bin ~ x | a, d, full, reduced,
                              prediction_method="observed", return.preds=TRUE)
  for (name in unique(prediction$model)) {
    fit = if (name == "full (glm)") full else reduced
    values = aggregate(predict(fit, eligible, type="response"),
                         list(x=midpoints[bins], a=eligible$a), mean)
    curve = prediction[prediction$model == name, ]
    rows = match(paste(curve$x, curve$a), paste(values$x, values$a))
    expect_equal(curve$prediction, values[[3]][rows], tolerance=1e-10)
  }
  colors = built$plot$scales$get_scales("colour")
  expect_setequal(colors$get_limits(), unique(prediction$model))
  markers = built$data[[4]]
  expected_panel = panels$PANEL[match(prediction$a, panels$a)]
  rows = match(paste(markers$x, markers$PANEL, markers$colour),
                 paste(prediction$x, expected_panel, colors$map(prediction$model)))
  expect_false(anyNA(rows))
  expect_equal(markers$y, prediction$prediction[rows])
  expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
  expect_true(any(vapply(grob$grobs[grepl("^guide-box", grob$layout$name)],
                        inherits, logical(1), "gtable")))
})

test_that("matched proportions retain numeric conditioning keys and sampling invariance", {
  d = small
  full = glm(y_bin ~ x + a + z, d, family=binomial())
  reduced = glm(y_bin ~ x, d, family=binomial())
  arguments = list(formula=y_bin ~ x | a + z, data=d, model1=full, model2=reduced,
                   prediction_method="observed", observation.type="binned",
                   breaks=list(z=c(-.5, .5)), labels=list(z=c("low", "middle", "high")))
  p = do.call(compare_fits, arguments)
  sampled = withr::with_seed(27, do.call(compare_fits, c(arguments, list(sample=1))))
  observed = p$layers[[3]]$data
  bins = cut(d$x, unique(quantile(d$x, seq(0, 1, length.out=10))), include.lowest=TRUE, labels=FALSE)
  z_bins = cut(d$z, c(min(d$z), -.5, .5, max(d$z)), include.lowest=TRUE,
                labels=c("low", "middle", "high"))
  edges = unique(quantile(d$x, seq(0, 1, length.out=10)))
  coordinates = (head(edges, -1) + tail(edges, -1)) / 2
  expected = aggregate(d$y_bin, list(mid=coordinates[bins], a=d$a, z=z_bins), mean)
  rows = match(paste(observed$x, observed$a, observed$z_binned), paste(expected$mid, expected$a, expected$z))
  expect_false(anyNA(rows))
  expect_equal(observed$proportion, expected$x[rows])
  expect_equal(sum(observed$count), nrow(d))
  observation_layer = which(vapply(sampled$layers, function(layer) {
    is.data.frame(layer$data) && "proportion" %in% names(layer$data)
  }, logical(1)))
  model_layer = which(vapply(sampled$layers, function(layer) {
    inherits(layer$geom, "GeomPoint") && is.data.frame(layer$data) && "prediction" %in% names(layer$data)
  }, logical(1)))
  expect_equal(sampled$layers[[observation_layer]]$data, observed)
  expect_equal(sampled$layers[[model_layer]]$data, p$layers[[4]]$data)
  expect_equal(nrow(sampled$layers[[1]]$data), 1)
  built = ggplot2::ggplot_build(p)
  panels = built$layout$layout
  rows = match(paste(observed$a, observed$z_binned), paste(panels$a, panels$z_binned))
  expect_equal(as.character(built$data[[3]]$PANEL), as.character(panels$PANEL[rows]))
  expect_no_message(ggplot2::ggplotGrob(p))
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("binary factor success coding is global even for extreme and singleton bins", {
  d = small
  d$y_bin = factor(d$y_bin, levels=c(1, 0), labels=c("success", "failure"))
  full = glm(y_bin ~ x + a + z, d, family=binomial())
  reduced = glm(y_bin ~ x, d, family=binomial())
  p = compare_fits(y_bin ~ x | a, d, full, reduced, prediction_method="observed",
                     observation.type="binned")
  observed = p$layers[[3]]$data
  coded = as.numeric(d$y_bin == "failure") # GLM predicts the second factor level.
  edges = unique(quantile(d$x, seq(0, 1, length.out=10)))
  bins = cut(d$x, edges, include.lowest=TRUE, labels=FALSE)
  mid = (head(edges, -1) + tail(edges, -1)) / 2
  expected = aggregate(coded, list(mid=mid[bins], a=d$a), mean)
  rows = match(paste(observed$x, observed$a), paste(expected$mid, expected$a))
  expect_equal(observed$proportion, expected$x[rows])
  expect_true(any(observed$proportion == 0))
  expect_true(any(observed$proportion == 1))
  expect_true(any(observed$count == 1))
  expect_true(all(is.finite(observed$proportion)))
  reversed = d
  reversed$y_bin = factor(reversed$y_bin, levels=rev(levels(d$y_bin)))
  other = glm(y_bin ~ x, reversed, family=binomial())
  expect_error(compare_fits(y_bin ~ x, d, full, other, prediction_method="observed",
                              observation.type="binned"), "different success coding")
})

test_that("matched probabilities remain visible when the common sample has one outcome", {
  full = glm(y_bin ~ x + a, small, family=binomial())
  for (value in c(0, 1)) {
    reduced = glm(y_bin ~ x, small, family=binomial(), subset=y_bin == value)
    expect_no_warning({
      p = compare_fits(y_bin ~ x, small, full, reduced, prediction_method="observed",
                         observation.type="binned")
    })
    expect_equal(p$coordinates$limits$y, c(0, 1))
    observed = p$layers[[3]]$data
    expect_true(all(observed$proportion == value))
    expect_equal(sum(observed$count), sum(small$y_bin == value))
    expect_true(all(ggplot2::ggplot_build(p)$data[[4]]$y >= 0 &
                    ggplot2::ggplot_build(p)$data[[4]]$y <= 1))
  }
})

test_that("observation display controls do not change returned predictions or other methods", {
  full = glm(y_bin ~ x + a + z, small, family=binomial())
  reduced = glm(y_bin ~ x, small, family=binomial())
  for (method in c("marginal", "reference", "observed")) {
    expected = compare_fits(y_bin ~ x | a, small, full, reduced,
                              prediction_method=method, return.preds=TRUE)
    for (type in c("raw", "binned", "none")) {
      expect_identical(compare_fits(y_bin ~ x | a, small, full, reduced,
                                     prediction_method=method, return.preds=TRUE,
                                     observation.type=type), expected)
    }
  }
  raw = compare_fits(y_bin ~ x | a, small, full, reduced)
  expect_gt(raw$layers[[1]]$position$height, 0)
  hidden = compare_fits(y_bin ~ x | a, small, full, reduced, observation.type="none")
  expect_true(all(ggplot2::ggplot_build(hidden)$data[[1]]$alpha == 0))
  expect_equal(hidden$layers[[2]]$data, raw$layers[[2]]$data)
  for (type in c("raw", "binned", "none")) {
    p = compare_fits(y_bin ~ x | a, small, full, reduced, prediction_method="observed",
                       observation.type=type, raw.data=FALSE)
    expect_true(all(ggplot2::ggplot_build(p)$data[[1]]$alpha == 0))
    expect_false(any(vapply(p$layers, function(layer) is.data.frame(layer$data) &&
                             "proportion" %in% names(layer$data), logical(1))))
  }
  expect_error(compare_fits(y_bin ~ x, small, full, reduced, observation.type="binned"),
                 "requires prediction_method='observed'")
  expect_error(compare_fits(y_bin ~ x, small, full, reduced, prediction_method="observed",
                              observation.type="binned", pred.type="link"), "pred.type='response'")
  expect_error(compare_fits(y_bin ~ x + a | z, small, full, reduced,
                              prediction_method="observed", observation.type="binned"), "second-axis variable")
  expect_identical(compare.fits, compare_fits)
  intercept = glm(y_bin ~ 1, small, family=binomial())
  p = compare_fits(y_bin ~ 1, small, intercept, observation.type="none")
  expect_true(all(ggplot2::ggplot_build(p)$data[[1]]$alpha == 0))
})

test_that("matched views reuse uncertainty predictions without refitting and keep coordinate names", {
  d = small
  names(d)[names(d) == "x"] = "proportion"
  names(d)[names(d) == "a"] = "count"
  full = glm(y_bin ~ proportion + count + z, d, family=binomial())
  reduced = glm(y_bin ~ proportion, d, family=binomial())
  expected = compare_fits(y_bin ~ proportion | count, d, full, reduced,
                            prediction_method="observed", report.se=TRUE, return.preds=TRUE)
  local_mocked_bindings(glm.fit=unexpected_binned_model_refit, .package="stats")
  expect_identical(compare_fits(y_bin ~ proportion | count, d, full, reduced,
                                  prediction_method="observed", report.se=TRUE,
                                  observation.type="binned", return.preds=TRUE), expected)
  p = compare_fits(y_bin ~ proportion | count, d, full, reduced,
                     prediction_method="observed", report.se=TRUE, observation.type="binned")
  observed = p$layers[[3]]$data
  expect_identical(attr(observed, "observed_fields"), c("proportion.1", "count.1"))
  expect_equal(sum(observed$count.1), nrow(d))
  expect_true(is.factor(observed$count))
  expect_no_warning(ggplot2::ggplotGrob(p))
  expect_error(compare_fits(y_bin ~ count, d, full, reduced,
                              prediction_method="observed", observation.type="binned"), "numeric x")
  gaussian = lm(y ~ proportion, d)
  expect_error(compare_fits(y ~ proportion, d, gaussian, prediction_method="observed",
                              observation.type="binned"), "binomial GLMs")
})
