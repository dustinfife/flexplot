test_that("supported binomial comparisons plot response predictions and retain explicit link scale", {
  d = tablesaw.injury
  full = glm(injury ~ safety * gender + attention, d, family=binomial())
  reduced = glm(injury ~ safety, d, family=binomial())
  formula = injury ~ safety | gender + attention
  for (method in c("marginal", "reference")) {
    for (scale in c("response", "link")) {
      prediction = compare_fits(formula, d, full, reduced, prediction_method=method,
                                  pred.type=scale, return.preds=TRUE, num_points=4)
      expect_no_warning({
        p = compare_fits(formula, d, full, reduced, prediction_method=method,
                           pred.type=scale, num_points=4, jitter=FALSE)
      })
      built = ggplot2::ggplot_build(p)
      # All model predictors are displayed, so each profile has one prediction
      # even when replicated over the entire reference population.
      for (name in unique(prediction$model)) {
        fit = if (name == "full (glm)") full else reduced
        rows = prediction$model == name
        expected = unname(predict(fit, prediction[rows, ], type=scale))
        expect_equal(prediction$prediction[rows], expected, tolerance=1e-10)
      }
      expect_equal(sort(built$data[[2]]$y), sort(prediction$prediction))
      if (scale == "response") {
        expect_true(all(prediction$prediction >= 0 & prediction$prediction <= 1))
        expect_equal(p$coordinates$limits$y, c(0, 1))
      } else {
        expect_true(any(prediction$prediction < 0 | prediction$prediction > 1))
        panels = built$layout$panel_params
        expect_lte(min(vapply(panels, function(panel) panel$y.range[1], numeric(1))),
                     min(prediction$prediction))
        expect_gte(max(vapply(panels, function(panel) panel$y.range[2], numeric(1))),
                     max(prediction$prediction))
      }
    }
  }
  default = compare_fits(formula, d, full, reduced, return.preds=TRUE, num_points=4)
  explicit = compare_fits(formula, d, full, reduced, return.preds=TRUE, num_points=4, pred.type="response")
  expect_identical(default, explicit)
})

test_that("observed binomial comparisons average actual probabilities within displayed bins", {
  d = tablesaw.injury
  full = glm(injury ~ safety + gender + attention, d, family=binomial())
  reduced = glm(injury ~ safety, d, family=binomial())
  edges = unique(quantile(d$safety, seq(0, 1, length.out=10)))
  bins = cut(d$safety, edges, include.lowest=TRUE, labels=FALSE)
  centers = (head(edges, -1) + tail(edges, -1)) / 2
  actual = compare_fits(injury ~ safety | gender, d, full, reduced,
                          prediction_method="observed", return.preds=TRUE)
  for (name in unique(actual$model)) {
    fit = if (name == "full (glm)") full else reduced
    expected = aggregate(predict(fit, d, type="response"), list(bin=bins, gender=d$gender), mean)
    curve = actual[actual$model == name, ]
    rows = match(paste(curve$safety, curve$gender), paste(centers[expected$bin], expected$gender))
    expect_false(anyNA(rows))
    expect_equal(curve$prediction, expected$x[rows], tolerance=1e-10)
  }
  p = compare_fits(injury ~ safety | gender, d, full, reduced,
                     prediction_method="observed", jitter=FALSE)
  expect_equal(sort(ggplot2::ggplot_build(p)$data[[2]]$y), sort(actual$prediction))
})

test_that("two-model second-axis errors provide a dynamic conditioning suggestion", {
  d = tablesaw.injury
  full = glm(injury ~ safety + gender + attention, d, family=binomial())
  reduced = glm(injury ~ safety, d, family=binomial())
  error = tryCatch(compare_fits(injury ~ safety + gender | attention, d, full, reduced), error=identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "in this case, gender", fixed=TRUE)
  text = strsplit(conditionMessage(error), "\n\n", fixed=TRUE)[[1]][2]
  suggestion = parse(text=text)[[1]]
  expect_identical(all.vars(suggestion$formula), c("injury", "safety", "gender", "attention"))
  expect_identical(suggestion$data, quote(d))
  expect_identical(suggestion$model1, quote(full))
  expect_identical(suggestion$model2, quote(reduced))

  toy = small
  names(toy)[names(toy) == "x"] = "first predictor"
  names(toy)[names(toy) == "b"] = "second group"
  full = lm(y ~ `first predictor` + `second group` + a, toy)
  reduced = lm(y ~ `first predictor`, toy)
  error = tryCatch(compare_fits(y ~ `first predictor` + `second group` | a,
                                  data=toy, model1=full, model2=reduced), error=identity)
  expect_match(conditionMessage(error), "in this case, second group", fixed=TRUE)
  suggestion = parse(text=strsplit(conditionMessage(error), "\n\n", fixed=TRUE)[[1]][2])[[1]]
  expect_identical(all.vars(suggestion$formula), c("y", "first predictor", "second group", "a"))
  expect_identical(suggestion$data, quote(toy))
})

test_that("positional and named binomial models use identical argument matching", {
  d = small
  full = glm(y_bin ~ x + a + z, d, family=binomial())
  reduced = glm(y_bin ~ x, d, family=binomial())
  for (method in c("marginal", "observed", "reference")) {
    positional = compare_fits(y_bin ~ x | a, d, full, reduced,
                               prediction_method=method, num_points=4, return.preds=TRUE)
    named = compare_fits(y_bin ~ x | a, data=d, model1=full, model2=reduced,
                          prediction_method=method, num_points=4, return.preds=TRUE)
    expect_identical(positional, named)
    mixed = compare_fits(formula=y_bin ~ x | a, data=d, full, reduced,
                          prediction_method=method, num_points=4, return.preds=TRUE)
    expect_identical(mixed, named)
    first = compare_fits(y_bin ~ x | a, d, full, reduced, prediction_method=method,
                          num_points=4, jitter=FALSE)
    second = compare_fits(y_bin ~ x | a, data=d, model1=full, model2=reduced,
                           prediction_method=method, num_points=4, jitter=FALSE)
    expect_equal(ggplot2::ggplot_build(first)$data, ggplot2::ggplot_build(second)$data)
    expect_null(attr(first$layers[[2]]$data, "flexplot_comparison_call"))
  }
})
