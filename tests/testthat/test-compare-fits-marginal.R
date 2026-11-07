comparison_standardized_predictions = function(model, reference, coordinates, displayed, ...) {
  vapply(seq_len(nrow(coordinates)), function(i) {
    profile = reference
    for (variable in displayed) profile[[variable]][] = coordinates[[variable]][i]
    mean(stats::predict(model, newdata=profile, ...))
  }, numeric(1))
}

comparison_population_fixture = function() {
  d = small[rep(seq_len(nrow(small)), length.out=650), ]
  rownames(d) = NULL
  d$w = seq_len(nrow(d))/100 + sin(seq_len(nrow(d)))
  d$weight = rep(c(1, 2, 5), length.out=nrow(d))
  d$log_exposure = .3*cos(seq_len(nrow(d))/17)
  d$y = d$y + .8*d$w + .3*d$x*d$w
  d
}

test_that("marginal is the default and returns named plotting coordinates", {
  withr::local_seed(123)
  m = lm(y ~ x + a + z, small)
  set.seed(123)
  actual = compare_fits(y ~ x | a, small, m, return.preds=TRUE, num_points=7)
  set.seed(123)
  explicit = compare_fits(y ~ x | a, small, m, return.preds=TRUE,
                           num_points=7, prediction_method="marginal")
  expect_identical(actual, explicit)
  set.seed(123)
  expect_identical(actual, compare.fits(y ~ x | a, small, m, return.preds=TRUE, num_points=7))
  expect_identical(names(actual), c("x", "a", "model", "prediction"))
  expect_equal(nrow(actual), 7 * nlevels(small$a))
  expect_equal(sort(unique(actual$x)), seq(min(small$x), max(small$x), length.out=7))
  expect_identical(levels(actual$a), levels(small$a))
  expect_equal(actual$prediction,
                 comparison_standardized_predictions(m, small, actual, c("x", "a")), tolerance=1e-10)
  for (category in levels(small$a)) {
    curve = actual[actual$a == category, ]
    expect_equal(diff(curve$prediction)/diff(curve$x), rep(unname(coef(m)["x"]), 6))
  }
})

test_that("marginal curves preserve displayed and undisplayed nonlinear interactions", {
  d = small
  d$w = d$x^2 + d$z
  m = lm(y ~ x*a + z + x:z + a:z + I(z^2) + w + x:w, d)
  actual = compare_fits(y ~ x | a, d, m, return.preds=TRUE, num_points=8)
  expected = comparison_standardized_predictions(m, d, actual, c("x", "a"))
  expect_equal(actual$prediction, expected, tolerance=1e-10)
  beta = coef(m)
  for (category in levels(d$a)) {
    curve = actual[actual$a == category, ]
    slope = beta["x"] + beta["x:z"]*mean(d$z) + beta["x:w"]*mean(d$w) +
      if (category == "b") beta["x:ab"] else 0
    expect_equal(diff(curve$prediction)/diff(curve$x), rep(unname(slope), 7))
  }
  reference = actual
  reference$z = mean(d$z)
  reference$w = mean(d$w)
  expect_gt(max(abs(expected - predict(m, newdata=reference))), 1e-4)
})

test_that("continuous moderator coordinates use full-data bin means", {
  m = lm(y ~ x*z + x:I(z^2) + a, small)
  actual = compare_fits(y ~ x | z, small, m, return.preds=TRUE, num_points=6)
  edges = quantile(small$z, seq(0, 1, length.out=4))
  bins = cut(small$z, edges, include.lowest=TRUE)
  # The existing flexplot reference grid rounds its bin means to three decimals.
  means = round(as.numeric(tapply(small$z, bins, mean)), 3)
  expect_setequal(unique(actual$z), means)
  expect_equal(nrow(actual), 6 * length(means))
  expect_equal(actual$prediction,
                 comparison_standardized_predictions(m, small, actual, c("x", "z")), tolerance=1e-10)
  slopes = tapply(seq_len(nrow(actual)), actual$z, function(rows) {
    curve = actual[rows, ]
    diff(range(curve$prediction))/diff(range(curve$x))
  })
  expect_gt(diff(range(slopes)), .001)
})

test_that("both models share the entire eligible reference population without RNG", withr::with_seed(900, {
  d = small[rep(seq_len(nrow(small)), length.out=650), ]
  rownames(d) = NULL
  d$w = seq_len(nrow(d))/100 + rnorm(nrow(d))
  d$y = d$y + .8*d$w + .3*d$x*d$w
  m1 = lm(y ~ x + w + I(w^2), d)
  m2 = lm(y ~ x*w + I(w^2), d)
  set.seed(42)
  seed = .Random.seed
  actual = compare_fits(y ~ x, d, m1, m2, return.preds=TRUE, num_points=5)
  expect_identical(.Random.seed, seed)
  for (name in unique(actual$model)) {
    fit = if (name == "m1 (lm)") m1 else m2
    curve = actual[actual$model == name, ]
    expected = comparison_standardized_predictions(fit, d, curve, "x")
    expect_equal(curve$prediction, expected, tolerance=1e-10)
  }
  set.seed(43)
  seed = .Random.seed
  different = compare_fits(y ~ x, d, m1, m2, return.preds=TRUE, num_points=5)
  expect_identical(actual, different)
  expect_identical(.Random.seed, seed)
}))

test_that("marginal predictions use only common eligible estimation observations", {
  d = small
  d$z[4] = NA
  d$b[7] = NA
  m1 = lm(y ~ x*z, d, subset=1:24)
  m2 = lm(y ~ x + b + I(z^2), d, subset=3:27)
  eligible = d[setdiff(3:24, c(4, 7)), ]
  actual = compare_fits(y ~ x, d, m1, m2, return.preds=TRUE, num_points=5)
  expect_equal(range(actual$x), range(eligible$x))
  for (name in unique(actual$model)) {
    fit = if (name == "m1 (lm)") m1 else m2
    curve = actual[actual$model == name, ]
    expect_equal(curve$prediction, comparison_standardized_predictions(fit, eligible, curve, "x"))
  }
})

test_that("large marginal comparisons retain every common eligible row", withr::with_seed(54, {
  d = comparison_population_fixture()
  d$w[c(50, 600)] = NA
  m1 = lm(y ~ x*w + a, d, subset=1:620)
  m2 = lm(y ~ x + I(w^2) + b, d, subset=31:650)
  eligible = d[setdiff(31:620, c(50, 600)), ]
  actual = compare_fits(y ~ x, d, m1, m2, return.preds=TRUE, num_points=5)
  expect_gt(nrow(eligible), 300)
  for (name in unique(actual$model)) {
    fitted = if (name == "m1 (lm)") m1 else m2
    curve = actual[actual$model == name, ]
    expect_equal(curve$prediction,
                   comparison_standardized_predictions(fitted, eligible, curve, "x"),
                   tolerance=1e-10)
  }
}))

test_that("full-population marginalization preserves fit weights offsets and scales", withr::with_seed(56, {
  d = comparison_population_fixture()
  models = list(
    lm(y ~ a*z + I(w^2) + offset(log_exposure), d, weights=weight),
    glm(y_pois ~ a*z + w + offset(log_exposure), d, weights=weight, family=poisson()),
    glm(y_bin ~ a*z + w + offset(log_exposure), d, weights=weight,
          family=binomial(link="probit"))
  )
  for (fitted in models) {
    response = all.vars(formula(fitted))[1]
    plotting = stats::reformulate("a", response=response)
    coefficients = coef(fitted)
    weights = stats::weights(fitted)
    scales = if (inherits(fitted, "glm")) c("response", "link") else "response"
    for (scale in scales) {
      seed = .Random.seed
      actual = compare_fits(plotting, d, fitted, return.preds=TRUE,
                              pred.type=scale, report.se=TRUE)
      expect_identical(.Random.seed, seed)
      expected = comparison_standardized_predictions(fitted, d, actual, "a", type=scale)
      expect_equal(actual$prediction, expected, tolerance=1e-10)
      for (i in seq_len(nrow(actual))) {
        profile = d
        profile$a[] = actual$a[i]
        design = model.matrix(delete.response(terms(fitted)), profile,
                                contrasts.arg=fitted$contrasts, xlev=fitted$xlevels)
        if (inherits(fitted, "glm") && scale == "response") {
          design = design*fitted$family$mu.eta(predict(fitted, profile, type="link"))
        }
        gradient = colMeans(design)
        expected_se = sqrt(drop(gradient %*% vcov(fitted) %*% gradient))
        expect_equal(actual$se[i], expected_se, tolerance=1e-10)
      }
    }
    expect_identical(coef(fitted), coefficients)
    expect_identical(stats::weights(fitted), weights)
  }
}))

test_that("large tree and forest predictions average over the full population", withr::with_seed(57, {
  d = comparison_population_fixture()
  models = list(rpart::rpart(y ~ a + x + w, d),
                  party::cforest(y ~ a + x + w, d,
                    controls=party::cforest_unbiased(ntree=12, mtry=2)))
  if (requireNamespace("randomForest", quietly=TRUE)) {
    models[[length(models)+1]] = randomForest::randomForest(y ~ a + x + w, d, ntree=12)
  }
  for (fitted in models) {
    seed = .Random.seed
    actual = compare_fits(y ~ a, d, fitted, return.preds=TRUE)
    expect_identical(.Random.seed, seed)
    expected = if (inherits(fitted, "RandomForest")) {
      comparison_standardized_predictions(fitted, d, actual, "a", OOB=FALSE)
    } else {
      comparison_standardized_predictions(fitted, d, actual, "a")
    }
    expect_equal(actual$prediction, expected, tolerance=1e-10)
  }
}))

test_that("GLM marginalization averages response predictions before applying any link", {
  m = glm(y_bin ~ a + b + a:b + z, small, family=binomial())
  actual = compare_fits(y_bin ~ a, small, m, return.preds=TRUE)
  expected = comparison_standardized_predictions(m, small, actual, "a", type="response")
  average_link = comparison_standardized_predictions(m, small, actual, "a", type="link")
  expect_equal(actual$prediction, expected)
  expect_gt(max(abs(expected - plogis(average_link))), .001)
  links = compare_fits(y_bin ~ a, small, m, return.preds=TRUE, pred.type="link")
  expect_equal(links$prediction, average_link)
  expect_true(all(actual$prediction >= 0 & actual$prediction <= 1))
})

test_that("marginal standard errors use the covariance of the averaged gradient", {
  for (fit in list(lm(y ~ a*z + I(z^2), small),
                   glm(y_bin ~ a + b + z, small, family=binomial()))) {
    formula = if (inherits(fit, "glm")) y_bin ~ a else y ~ a
    actual = compare_fits(formula, small, fit, return.preds=TRUE, report.se=TRUE)
    for (i in seq_len(nrow(actual))) {
      profile = small
      profile$a[] = actual$a[i]
      design = model.matrix(delete.response(terms(fit)), profile)
      if (inherits(fit, "glm")) design = design * fit$family$mu.eta(predict(fit, profile, type="link"))
      gradient = colMeans(design)
      se = sqrt(drop(gradient %*% vcov(fit) %*% gradient))
      expect_equal(actual$se[i], se)
      critical = if (inherits(fit, "glm")) qnorm(.975) else qt(.975, df.residual(fit))
      expect_equal(actual$prediction.lwr[i], actual$prediction[i] - critical*se)
      expect_equal(actual$prediction.upr[i], actual$prediction[i] + critical*se)
    }
  }
})

test_that("marginal mixed-model curves retain cluster-specific prediction streams", {
  withr::local_seed(42)
  d = lme4::sleepstudy
  m1 = lme4::lmer(Reaction ~ Days + (Days | Subject), d)
  m2 = lme4::lmer(Reaction ~ Days + (1 | Subject), d)
  actual = compare_fits(Reaction ~ Days | Subject, d, m1, return.preds=TRUE,
                         clusters=2, num_points=5, re=TRUE)
  expect_equal(length(unique(actual$Subject)), 2)
  expect_setequal(actual$model, c("fixed effects", "random effects"))
  for (type in unique(actual$model)) {
    curve = actual[actual$model == type, ]
    expect_equal(curve$prediction, unname(predict(m1, newdata=curve,
                        re.form=if (type == "fixed effects") NA else NULL)))
  }
  pair = compare_fits(Reaction ~ Days | Subject, d, m1, m2, return.preds=TRUE,
                       clusters=2, num_points=5, re=TRUE)
  expect_setequal(pair$model, c("m1 (lmerMod)", "m2 (lmerMod)"))
  for (name in unique(pair$model)) {
    fit = if (name == "m1 (lmerMod)") m1 else m2
    curve = pair[pair$model == name, ]
    expect_equal(curve$prediction, unname(predict(fit, newdata=curve, re.form=NULL)))
  }
})

test_that("mixed marginalization uses every prepared observation without RNG", withr::with_seed(58, {
  d = small_mixed[rep(seq_len(nrow(small_mixed)), length.out=900), ]
  rownames(d) = NULL
  d$w = seq_len(nrow(d))/100 + sin(seq_len(nrow(d)))
  d$y = d$y + .1*d$w + .15*d$x*d$w
  fitted = lme4::lmer(y ~ x*w + a + (1 | id), d)
  plotting = y ~ x | id
  # Cluster selection is a separate, existing step. Hold that selection fixed
  # while independently checking the entire resulting reference population.
  prepared = withr::with_seed(42,
    prepare_data_for_compare_fits(d, fitted, plotting, clusters=3))
  actual = withr::with_seed(42,
    compare_fits(plotting, d, fitted, return.preds=TRUE,
                   clusters=3, num_points=3, re=TRUE))
  expect_gt(nrow(prepared), 300)
  expect_setequal(as.character(actual$id), as.character(prepared$id))
  for (stream in unique(actual$model)) {
    curve = actual[actual$model == stream, ]
    expected = comparison_standardized_predictions(fitted, prepared, curve, c("x", "id"),
                 re.form=if (stream == "fixed effects") NA else NULL)
    expect_equal(curve$prediction, expected, tolerance=1e-10)
  }
  seed = .Random.seed
  internal = compare_fits_marginal(list(fitted), droplevels(prepared), plotting,
                    re=TRUE, pred.type="response", model_labels="fitted", single_model=TRUE,
                    report.se=FALSE, num_points=3)
  expect_identical(.Random.seed, seed)
  expect_equal(internal, actual)
}))

test_that("marginal tree and forest curves predict standardized profiles", {
  withr::local_seed(10)
  models = list(tree=rpart::rpart(y ~ a + x + z, small),
                forest=party::cforest(y ~ a + x + z, small,
                         controls=party::cforest_unbiased(ntree=15, mtry=2)))
  if (requireNamespace("randomForest", quietly=TRUE)) {
    models$randomForest = randomForest::randomForest(y ~ a + x + z, small, ntree=20)
  }
  for (fit in models) {
    actual = compare_fits(y ~ a, small, fit, return.preds=TRUE)
    expect_equal(actual$prediction, comparison_standardized_predictions(fit, small, actual, "a"))
    expect_error(compare_fits(y ~ a, small, fit, return.preds=TRUE, report.se=TRUE), "report.se=TRUE")
  }
})

test_that("marginal binary trees and forests retain second-level probabilities", {
  withr::local_seed(10)
  d = small
  d$binary = factor(d$y_bin, levels=c(0, 1), labels=c("no", "yes"))
  tree = rpart::rpart(binary ~ a + x + z, d)
  forest = party::cforest(binary ~ a + x + z, d,
                           controls=party::cforest_unbiased(ntree=15, mtry=2))
  for (fit in list(tree, forest)) {
    actual = compare_fits(binary ~ a, d, fit, return.preds=TRUE)
    expected = numeric(nrow(actual))
    for (i in seq_len(nrow(actual))) {
      profile = d
      profile$a[] = actual$a[i]
      probabilities = predict(fit, newdata=profile, type="prob")
      if (is.list(probabilities)) probabilities = do.call(rbind, probabilities)
      column = if ("yes" %in% colnames(probabilities)) "yes" else "binary.yes"
      expected[i] = mean(probabilities[, column])
    }
    expect_equal(actual$prediction, expected)
  }
})

test_that("marginal GLMMs use response-scale predictions at displayed moderators", {
  withr::local_seed(12)
  d = lme4::sleepstudy
  effect = rep(seq(-2, 2, length.out=18), each=10)
  d$binary = rbinom(nrow(d), 1, plogis((d$Days - 4)/4 + effect))
  m = lme4::glmer(binary ~ Days + (1 | Subject), d, family=binomial())
  actual = compare_fits(binary ~ Days | Subject, d, m, return.preds=TRUE,
                         num_points=5, clusters=2)
  expect_equal(length(unique(actual$Subject)), 2)
  expect_equal(actual$prediction, unname(predict(m, actual, re.form=NA, type="response")))
})

test_that("marginal robust and numeric ordinal predictions retain their model conventions", {
  d = small
  d$ordinal = cut(d$y, quantile(d$y, c(0, 1/3, 2/3, 1)),
                   labels=c("1", "2", "3"), include.lowest=TRUE, ordered_result=TRUE)
  robust = MASS::rlm(y ~ a + z, d)
  actual = compare_fits(y ~ a, d, robust, return.preds=TRUE)
  expect_equal(actual$prediction, comparison_standardized_predictions(robust, d, actual, "a"))
  ordinal = MASS::polr(ordinal ~ a + z, d, Hess=TRUE)
  actual = compare_fits(ordinal ~ a, d, ordinal, return.preds=TRUE)
  expected = numeric(nrow(actual))
  for (i in seq_len(nrow(actual))) {
    profile = d
    profile$a[] = actual$a[i]
    expected[i] = mean(as.numeric(as.character(predict(ordinal, profile, type="class"))))
  }
  expect_equal(actual$prediction, expected)
})

test_that("marginal grids reject invalid sizes without changing historical methods", {
  m = lm(y ~ x + z, small)
  for (size in list(1, 2.5, NA_real_, Inf)) {
    expect_error(compare_fits(y ~ x, small, m, num_points=size, return.preds=TRUE), "num_points")
  }
  for (method in c("observed", "reference")) {
    actual = compare_fits(y ~ x, small, m, prediction_method=method, return.preds=TRUE)
    expect_true(all(is.finite(actual$prediction)))
  }
})

test_that("default intercept-only comparisons retain estimates", {
  m = lm(y ~ 1, small)
  actual = compare_fits(y ~ 1, small, m, return.preds=TRUE, report.se=TRUE)
  expect_identical(names(actual), c("model", "prediction", "se", "prediction.fit",
                                    "prediction.lwr", "prediction.upr"))
  expect_equal(nrow(actual), 1)
  expect_equal(actual$prediction, mean(small$y))
  expect_equal(actual$se, unname(summary(m)$coefficients[1, 2]))
  expect_no_error(ggplot2::ggplot_build(compare_fits(y ~ 1, small, m)))
  expect_equal(estimates(m)$Mean, mean(small$y))
  expect_no_error(ggplot2::ggplot_build(visualize(m)))
  m2 = lm(y ~ 1, small)
  for (method in c("marginal", "observed", "reference")) {
    pair = compare_fits(y ~ 1, small, m, m2, return.preds=TRUE,
                         report.se=TRUE, prediction_method=method)
    expect_equal(nrow(pair), 2)
    expect_setequal(pair$model, c("m (lm)", "m2 (lm)"))
    expect_equal(pair$prediction, rep(mean(small$y), 2))
    expect_equal(pair$se, rep(unname(summary(m)$coefficients[1, 2]), 2))
  }
  binary = glm(y_bin ~ 1, small, family=binomial())
  probability = compare_fits(y_bin ~ 1, small, binary, return.preds=TRUE, report.se=TRUE)
  expect_equal(probability$prediction, mean(small$y_bin))
  expect_equal(probability$se, unname(predict(binary, newdata=small[1, ],
                                              type="response", se.fit=TRUE)$se.fit))
})

test_that("marginal custom conditioning labels retain representative coordinates", {
  m = lm(y ~ a*z, small)
  actual = compare_fits(y ~ a | z, small, m, return.preds=TRUE,
                         breaks=list(z=c(-.5, .5)), labels=list(z=c("low", "middle", "high")))
  bins = cut(small$z, c(min(small$z), -.5, .5, max(small$z)), include.lowest=TRUE)
  expect_setequal(unique(actual$z), round(as.numeric(tapply(small$z, bins, mean)), 3))
  expect_equal(actual$prediction, comparison_standardized_predictions(m, small, actual, c("a", "z")))
})

test_that("custom display labels remain separate from model labels for every method", {
  m1 = lm(y ~ a + z, small)
  m2 = lm(y ~ a*z, small)
  labels = list(z=c("low", "middle", "high"))
  breaks = list(z=c(-.5, .5))
  independent = small
  independent$z_binned = cut(small$z, c(min(small$z), -.5, .5, max(small$z)),
                              include.lowest=TRUE, labels=labels$z)
  for (method in c("marginal", "observed", "reference")) {
    actual = compare_fits(y ~ a | z, small, m1, m2, return.preds=TRUE,
                           labels=labels, breaks=breaks, prediction_method=method)
    expect_setequal(actual$model, c("m1 (lm)", "m2 (lm)"))
    for (name in unique(actual$model)) {
      fit = if (name == "m1 (lm)") m1 else m2
      curve = actual[actual$model == name, ]
      if (method == "observed") {
        independent$prediction = unname(predict(fit, newdata=small))
        expected = aggregate(prediction ~ a + z_binned, independent, mean)
        rows = match(paste(curve$a, curve$z_binned), paste(expected$a, expected$z_binned))
        expect_false(anyNA(rows))
        expect_equal(curve$prediction, expected$prediction[rows])
        next
      }
      expect_equal(curve$prediction, comparison_standardized_predictions(fit, small, curve, c("a", "z")))
    }
    p = compare_fits(y ~ a | z, small, m1, m2, labels=labels,
                      breaks=breaks, prediction_method=method, jitter=FALSE)
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_identical(as.character(built$layout$layout$z_binned), labels$z)
  }
})

test_that("integer-trained forests retain fractional prediction coordinates", {
  withr::local_seed(44)
  d = expand.grid(x=0:11, z=1:10)
  d$g = factor(rep(c("b", "a"), length.out=nrow(d)), levels=c("b", "a"))
  d$y = as.integer(10*d$x + 3*d$z + 5*(d$g == "a"))
  control = party::cforest_unbiased(ntree=15, mtry=2)
  set.seed(44)
  integer_model = party::cforest(y ~ x + z + g, d, controls=control)
  numeric_data = d
  numeric_data$x = as.double(d$x)
  numeric_data$z = as.double(d$z)
  set.seed(44)
  numeric_model = party::cforest(y ~ x + z + g, numeric_data, controls=control)
  # Identical fitted trees let ordinary predict() on double inputs provide an
  # independent oracle without any flexplot class adaptation or coordinate casts.
  expect_identical(integer_model@ensemble, numeric_model@ensemble)
  expect_identical(integer_model@weights, numeric_model@weights)
  actual = compare_fits(y ~ x | z, d, integer_model, return.preds=TRUE, num_points=17)
  expect_equal(sort(unique(actual$x)), seq(min(d$x), max(d$x), length.out=17))
  expect_true(any(actual$x != trunc(actual$x)))
  expect_true(any(actual$z != trunc(actual$z)))
  expected = comparison_standardized_predictions(numeric_model, numeric_data, actual, c("x", "z"), OOB=FALSE)
  expect_equal(actual$prediction, expected, tolerance=1e-10)
  truncated = actual
  truncated$x = trunc(truncated$x)
  truncated$z = trunc(truncated$z)
  wrong = comparison_standardized_predictions(numeric_model, numeric_data, truncated, c("x", "z"), OOB=FALSE)
  expect_gt(max(abs(expected - wrong)), .01)
  expect_identical(integer_model@ensemble, numeric_model@ensemble)
  trained = attr(integer_model, "data")@get("input")
  for (variable in c("x", "z", "g")) expect_identical(trained[[variable]], d[[variable]])
  for (method in c("observed", "reference")) {
    values = compare_fits(y ~ x | z, d, integer_model, return.preds=TRUE, prediction_method=method)
    if (method == "reference") {
      oracle = values
      oracle$x = as.double(oracle$x)
      oracle$z = as.double(oracle$z)
      expect_equal(values$prediction, unname(as.numeric(predict(numeric_model, newdata=oracle, OOB=FALSE))))
      next
    }
    edges = unique(quantile(d$x, seq(0, 1, length.out=10)))
    x_bins = cut(d$x, edges, include.lowest=TRUE, labels=FALSE)
    centers = (head(edges, -1) + tail(edges, -1))/2
    by_z = cut(d$z, quantile(d$z, seq(0, 1, length.out=4)), include.lowest=TRUE, labels=FALSE)
    z_values = as.numeric(tapply(d$z, by_z, mean))
    predictions = as.numeric(predict(integer_model, newdata=d, OOB=FALSE))
    expected = aggregate(predictions, list(x=centers[x_bins], z=z_values[by_z]), mean)
    names(expected)[3] = "prediction"
    rows = match(paste(round(values$x, 10), values$z), paste(round(expected$x, 10), expected$z))
    expect_false(anyNA(rows))
    expect_equal(values$prediction, expected$prediction[rows])
  }
})

test_that("marginal additive and interaction plots preserve their validated curves", {
  withr::local_seed(123)
  additive = lm(y ~ x + a + z, small)
  interaction = lm(y ~ x*a + I(z^2), small)
  p = compare_fits(y ~ x | a, small, additive, interaction, jitter=FALSE, num_points=10)
  vdiffr::expect_doppelganger("marginal additive and interaction curves", p)
})
