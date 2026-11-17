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

test_that("binomial comparisons preserve the fitted event for ordered and unordered factors", {
  d = avengers
  full = glm(died ~ minutes.fighting * willpower,d,family=binomial())
  reduced = glm(died ~ minutes.fighting + willpower,d,family=binomial())
  encoding = levels(model.response(model.frame(full)))
  for (method in c("marginal","observed","reference")) {
    expected = compare_fits(died~willpower|minutes.fighting,d,full,reduced,
      prediction_method=method,return.preds=TRUE,num_points=5)
    for (ordered in c(FALSE,TRUE)) {
      supplied = d
      supplied$died = factor(supplied$died,levels=encoding,ordered=ordered)
      actual = compare_fits(died~willpower|minutes.fighting,supplied,full,reduced,
        prediction_method=method,return.preds=TRUE,num_points=5)
      expect_equal(actual$prediction,expected$prediction,tolerance=1e-12)
      p = compare_fits(died~willpower|minutes.fighting,supplied,full,reduced,
        prediction_method=method,num_points=5,jitter=FALSE)
      expect_identical(p$scales$get_scales("y")$labels,encoding)
      expect_equal(sort(ggplot2::ggplot_build(p)$data[[1]]$y),
        sort(as.numeric(factor(d$died,levels=encoding))-1))
    }
  }
})

test_that("binomial comparisons reject reversed data or model encodings before prediction", {
  d = avengers
  full = glm(died~minutes.fighting*willpower,d,family=binomial())
  reduced = glm(died~minutes.fighting+willpower,d,family=binomial())
  reversed = d
  reversed$died = factor(d$died,levels=rev(levels(d$died)),ordered=TRUE)
  opposite = glm(died~minutes.fighting+willpower,reversed,family=binomial())
  for (method in c("marginal","observed","reference")) for (scale in c("response","link")) {
    expect_error(compare_fits(died~willpower|minutes.fighting,reversed,full,reduced,
      prediction_method=method,pred.type=scale,return.preds=TRUE),
      "Binomial outcome coding.*died.*model1.*Restore the fitted outcome level order")
    expect_error(compare_fits(died~willpower|minutes.fighting,d,full,opposite,
      prediction_method=method,pred.type=scale,return.preds=TRUE),
      "different success coding: model1.*model2.*same outcome level order")
  }
  expect_error(compare.fits(died~willpower|minutes.fighting,reversed,full,reduced),"outcome coding")
  expect_error(compare_fits(died~willpower|minutes.fighting,reversed,full,reduced,
    prediction_method="observed",observation.type="binned"),"outcome coding")
  local_mocked_bindings(compare_fits_marginal=function(...) stop("prediction generation reached"))
  expect_error(compare_fits(died~willpower|minutes.fighting,reversed,full,reduced),"outcome coding")
})

test_that("refitting both binomial models for the reversed event yields complementary probabilities", {
  d = avengers
  full = glm(died~minutes.fighting*willpower,d,family=binomial())
  reduced = glm(died~minutes.fighting+willpower,d,family=binomial())
  reversed = d
  reversed$died = factor(d$died,levels=rev(levels(d$died)),ordered=TRUE)
  reversed_full = glm(died~minutes.fighting*willpower,reversed,family=binomial())
  reversed_reduced = glm(died~minutes.fighting+willpower,reversed,family=binomial())
  for (method in c("marginal","observed","reference")) {
    original = compare_fits(died~willpower|minutes.fighting,d,full,reduced,
      prediction_method=method,return.preds=TRUE,num_points=5)
    actual = compare_fits(died~willpower|minutes.fighting,reversed,reversed_full,reversed_reduced,
      prediction_method=method,return.preds=TRUE,num_points=5)
    expect_equal(actual[c("willpower","minutes.fighting")],original[c("willpower","minutes.fighting")])
    expect_equal(actual$prediction,1-original$prediction,tolerance=1e-8)
    if (method != "observed") {
      for (label in unique(actual$model)) {
        model = if (label == "reversed_full (glm)") reversed_full else reversed_reduced
        rows = actual$model == label
        expect_equal(actual$prediction[rows],unname(predict(model,actual[rows,],type="response")),tolerance=1e-10)
      }
    }
    p = compare_fits(died~willpower|minutes.fighting,reversed,reversed_full,reversed_reduced,
      prediction_method=method,num_points=5,jitter=FALSE)
    expect_identical(p$scales$get_scales("y")$labels,levels(reversed$died))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("numeric binomial outcomes and valid predictor-factor changes remain supported", {
  d = small
  d$y_bin = as.numeric(as.character(d$y_bin))
  full = glm(y_bin~x+a,d,family=binomial())
  reduced = glm(y_bin~x,d,family=binomial())
  supplied = d
  supplied$a = factor(d$a,levels=rev(levels(d$a)))
  for (method in c("marginal","observed","reference")) {
    actual = compare_fits(y_bin~x|a,supplied,full,reduced,
      prediction_method=method,return.preds=TRUE,num_points=5)
    expect_true(all(actual$prediction>=0 & actual$prediction<=1))
    if (method != "observed") for (label in unique(actual$model)) {
      model = if (label == "full (glm)") full else reduced
      rows = actual$model == label
      expect_equal(actual$prediction[rows],unname(predict(model,actual[rows,],type="response")),tolerance=1e-10)
    }
    expect_no_warning(ggplot2::ggplot_build(compare_fits(y_bin~x|a,supplied,full,reduced,
      prediction_method=method,jitter=FALSE,num_points=5)))
  }
})

test_that("unused outcome declarations do not change an otherwise matching binomial event", {
  d = avengers
  full = glm(died~minutes.fighting+willpower,d,family=binomial())
  supplied = d
  supplied$died = factor(d$died,levels=c("unused",levels(d$died),"also unused"))
  p = compare_fits(died~willpower|minutes.fighting,supplied,full,jitter=FALSE,num_points=5)
  expect_identical(p$scales$get_scales("y")$labels,levels(model.response(model.frame(full))))
  expect_no_warning(ggplot2::ggplot_build(p))
})
