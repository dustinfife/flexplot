lm_matrix_oracle = function(model, reference, coordinates, displayed) {
  vapply(seq_len(nrow(coordinates)), function(i) {
    profile = reference
    for (variable in displayed) profile[[variable]] = rep(coordinates[[variable]][i], nrow(profile))
    mean(predict(model, newdata=profile, type="response"))
  }, numeric(1))
}

test_that("lm matrix marginalization matches full-population prediction averages", {
  d = small
  d$weight = seq_len(nrow(d)) / nrow(d) + 1
  d$exposure = exp(d$z)
  models = list(
    lm(y ~ x, d),
    lm(y ~ x + z + b, d),
    lm(y ~ x*z + x*b, d),
    lm(y ~ (x + I(x^2) + I(x^3))*z + I(z^2), d),
    lm(y ~ x*b + z, d, contrasts=list(b="contr.sum")),
    lm(y ~ x*z + b, d, weights=weight),
    lm(y ~ x*z + offset(log(exposure)), d),
    lm(y ~ x*z + offset(x/3), d, offset=log(exposure))
  )
  for (model in models) {
    actual = compare_fits(y ~ x, d, model, return.preds=TRUE, num_points=6)
    expect_equal(actual$prediction, lm_matrix_oracle(model, d, actual, "x"), tolerance=1e-10)
    expect_type(compare_fits_lm_average(model, d, "response"), "double")
  }
  grouped = compare_fits(y ~ x | b, d, models[[5]], return.preds=TRUE, num_points=6)
  expect_equal(grouped$prediction,
               lm_matrix_oracle(models[[5]], d, grouped, c("x", "b")), tolerance=1e-10)
  expect_identical(levels(grouped$b), levels(d$b))
})

test_that("optimized lm comparisons share eligible rows and preserve streams", {
  d = small
  first = lm(y ~ x*z, d, subset=1:24)
  second = lm(y ~ x + I(x^2) + z + b, d, subset=3:27)
  eligible = d[3:24, ]
  actual = compare_fits(y ~ x | b, d, first, second, return.preds=TRUE, num_points=5)
  for (label in unique(actual$model)) {
    model = if (label == "first (lm)") first else second
    curve = actual[actual$model == label, ]
    expect_equal(curve$prediction,
                 lm_matrix_oracle(model, eligible, curve, c("x", "b")), tolerance=1e-10)
  }
  expect_equal(length(unique(actual$model)), 2)
  testthat::local_mocked_bindings(compare_fits_lm_average=function(...) NULL)
  oracle = compare_fits(y ~ x | b, d, first, second, return.preds=TRUE, num_points=5)
  expect_equal(actual, oracle, tolerance=1e-10)
})

test_that("lm matrix optimization bypasses row predictions only for supported fits", {
  model = lm(y ~ x*z, small)
  expected = compare_fits(y ~ x, small, model, return.preds=TRUE, num_points=5)
  testthat::local_mocked_bindings(compare_fits_prediction_frame=function(...) stop("row predictions used"))
  expect_equal(compare_fits(y ~ x, small, model, return.preds=TRUE, num_points=5), expected)
  expect_error(compare_fits(y ~ x, small, model, return.preds=TRUE,
                            prediction_method="observed"), "row predictions used")
  expect_error(compare_fits(y ~ x, small, model, return.preds=TRUE,
                            prediction_method="reference"), "row predictions used")
  expect_error(compare_fits(y ~ x, small, model, return.preds=TRUE, report.se=TRUE),
               "row predictions used")
})

test_that("unsupported lm specifications retain full-prediction fallback", {
  d = small
  d$duplicate = d$x
  deficient = lm(y ~ x + duplicate, d)
  expect_null(compare_fits_lm_average(deficient, d, "response"))
  subclass = lm(y ~ x, d)
  class(subclass) = c("custom_lm", "lm")
  expect_null(compare_fits_lm_average(subclass, d, "response"))
  expect_null(compare_fits_lm_average(glm(y_bin ~ x, d, family=binomial()), d, "response"))
  model = lm(y ~ x, d)
  expect_null(compare_fits_lm_average(model, d, "terms"))
  invalid = d
  invalid$x[1] = Inf
  expect_null(compare_fits_lm_average(model, invalid, "response"))
  intercept = lm(y ~ 1, d)
  expect_equal(compare_fits(y ~ 1, d, intercept, return.preds=TRUE)$prediction,
               mean(predict(intercept, newdata=d)), tolerance=1e-10)
})

# Orthogonal polynomial prediction terms are exercised below eligibility checks:
# existing row verification rejects their matrix-valued training columns.
test_that("orthogonal polynomial prediction terms retain their fitted basis", {
  model = lm(y ~ poly(x, 3)*z, small)
  actual = compare_fits_marginal(list(model), small, y ~ x, FALSE, "response",
                                 "polynomial", TRUE, FALSE, num_points=6)
  expect_equal(actual$prediction, lm_matrix_oracle(model, small, actual, "x"), tolerance=1e-10)
})

test_that("ordered factors and uncertainty retain prediction semantics", {
  d = small
  d$b = ordered(d$b, levels=rev(levels(d$b)))
  model = lm(y ~ x*b + z, d)
  actual = compare_fits(y ~ x | b, d, model, return.preds=TRUE, num_points=4)
  expect_equal(actual$prediction, lm_matrix_oracle(model, d, actual, c("x", "b")), tolerance=1e-10)
  expect_identical(levels(actual$b), levels(d$b))
  uncertainty = compare_fits(y ~ x | b, d, model, return.preds=TRUE, num_points=4, report.se=TRUE)
  expect_equal(actual$prediction, uncertainty$prediction, tolerance=1e-10)
  testthat::local_mocked_bindings(compare_fits_lm_average=function(...) NULL)
  expect_equal(uncertainty,
               compare_fits(y ~ x | b, d, model, return.preds=TRUE, num_points=4, report.se=TRUE),
               tolerance=1e-10)
})
