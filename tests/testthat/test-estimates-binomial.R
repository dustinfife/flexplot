# Replicate the small predictor fixture with successes and failures at every
# covariate profile, avoiding separation in the interaction regressions.
binomial_estimates_data = small[rep(seq_len(nrow(small)), each=12), ]
binomial_estimates_probability = with(small, plogis(.2 + .5*x - .3*z +
  .4*(a == "b") + .3*x*(a == "b") + .2*(b == "y") - .2*(b == "z")))
binomial_estimates_data$y_bin = as.integer(rep(seq_len(12), nrow(small)) <=
  rep(pmax(1, pmin(11, floor(12*binomial_estimates_probability))), each=12))

test_that("conditional logistic summaries agree with independent roots and derivatives", {
  for (formula in list(y_bin ~ x, y_bin ~ x + z, y_bin ~ x*z,
                       y_bin ~ x*a + z, y_bin ~ x*b + z)) {
    model = glm(formula, data=binomial_estimates_data, family=binomial)
    variables = all.vars(formula)[-1]
    reference = binomial_estimates_data[1, , drop=FALSE]
    for (variable in variables) {
      reference[[variable]] = if (is.numeric(binomial_estimates_data[[variable]])) mean(binomial_estimates_data[[variable]]) else
        factor(levels(binomial_estimates_data[[variable]])[1], levels=levels(binomial_estimates_data[[variable]]))
    }
    for (location in c(.5, .7)) {
      summary = logistic_coefficients(model, location)
      for (variable in variables[vapply(binomial_estimates_data[variables], is.numeric, logical(1))]) {
        # The root is computed on the link scale to avoid numerical saturation.
        root = uniroot(function(value) {
          reference[[variable]] = value
          predict(model, reference, type="link") - qlogis(location)
        }, c(-100, 100), tol=1e-12)$root
        profile = reference[rep(1, 3), , drop=FALSE]
        profile[[variable]] = root + c(-1e-5, 0, 1e-5)
        probabilities = predict(model, profile, type="response")
        row = match(variable, summary$variable)
        expect_equal(summary$intercept_threshold[row], root, tolerance=1e-9)
        expect_equal(summary$instantaneous_slope[row],
                     unname((probabilities[3]-probabilities[1])/2e-5), tolerance=1e-8)
        expect_equal(unname(probabilities[2]), location, tolerance=1e-9)
      }
    }
  }
})

test_that("fitted treatment reference levels and nondefault contrasts are respected", {
  d = binomial_estimates_data
  contrasts(d$b) = contr.treatment(levels(d$b), base=3)
  model = glm(y_bin ~ x*b + z, d, family=binomial)
  summary = logistic_coefficients(model)
  profile = d[1, , drop=FALSE]
  profile$b = factor("z", levels=levels(d$b))
  profile$z = mean(d$z)
  profile$x = summary$intercept_threshold[summary$variable == "x"]
  expect_equal(unname(predict(model, profile, type="response")), .5, tolerance=1e-9)
  profile$x = profile$x + 1e-5
  expect_equal(unname((predict(model, profile, type="response")-.5)/1e-5),
               summary$instantaneous_slope[summary$variable == "x"], tolerance=1e-5)
  for (coding in list(contr.sum(3), contr.helmert(3))) {
    contrasts(d$b) = coding
    model = glm(y_bin ~ x*b, d, family=binomial)
    result = estimates(model)
    profile$b = factor("x", levels=levels(d$b))
    profile$x = attr(result, "irt")$intercept_threshold[1]
    expect_equal(unname(predict(model, profile, type="response")), .5, tolerance=1e-8)
    expect_equal(attr(result, "row.names"), names(coef(model)))
    expect_equal(result$raw.coefficients, unname(round(coef(model), 3)))
    marginal = attr(result, "marginal_probabilities")
    for (i in seq_len(nrow(marginal))) {
      data = d
      data$b = factor(marginal$Level[i], levels=levels(d$b))
      expect_equal(marginal$Probability[i], mean(predict(model, data, type="response")), tolerance=1e-12)
    }
  }
})

test_that("marginal probabilities average original-model predictions on estimation rows", {
  for (formula in list(y_bin ~ a, y_bin ~ b, y_bin ~ x*a + z*b)) {
    d = binomial_estimates_data
    d$z[1] = NA_real_
    model = glm(formula, d, family=binomial)
    result = estimates(model)
    marginal = attr(result, "marginal_probabilities")
    eligible = d[match(rownames(model.frame(model)), rownames(d)), ]
    for (i in seq_len(nrow(marginal))) {
      profile = eligible
      variable = marginal$Variable[i]
      profile[[variable]] = factor(marginal$Level[i], levels=levels(d[[variable]]))
      expect_equal(marginal$Probability[i], mean(predict(model, profile, type="response")),
                   tolerance=1e-12)
    }
    expect_equal(marginal$Level, unlist(model$xlevels, use.names=FALSE))
  }
})

test_that("IRT rows align with continuous main effects without changing coefficient rows", {
  model = glm(y_bin ~ x*b + z*a, binomial_estimates_data, family=binomial)
  result = estimates(model)
  expect_identical(class(result), "glm_estimates")
  expect_equal(attr(result, "row.names"), names(coef(model)))
  expect_named(result, c("raw.coefficients", "OR", "inverse.OR", "standardized.OR",
                         "inverse.standardized.OR", "instantaneous_slope", "intercept_threshold",
                         "Prediction Difference (+/- 1 SD)"))
  expect_equal(result$OR, unname(round(exp(coef(model)), 3)))
  expect_equal(result$inverse.OR, unname(round(exp(-coef(model)), 3)))
  expected = coef(model) * apply(model.matrix(model), 2, sd)
  expect_equal(result$standardized.OR, unname(round(exp(expected), 3)))
  expect_equal(result$inverse.standardized.OR, unname(round(exp(-expected), 3)))
  summary = attr(result, "irt")
  for (variable in c("x", "z")) {
    expect_equal(result$instantaneous_slope[attr(result, "row.names") == variable],
                   round(summary$instantaneous_slope[summary$variable == variable], 3))
  }
  expect_type(result$instantaneous_slope, "double")
  expect_type(result$intercept_threshold, "double")
  expect_true(all(is.na(result$instantaneous_slope[!attr(result, "row.names") %in% c("x", "z")])))
})

test_that("four printed sections retain continuous rows with missing IRT quantities", {
  model = glm(y_bin ~ x + I(x^2) + b, binomial_estimates_data, family=binomial)
  result = estimates(model)
  output = capture.output(print(result))
  sections = match(c("Raw Estimates:", "IRT Estimates:", "Standardized Estimates:",
                      "Marginal Predicted Probabilities:"), output)
  expect_false(anyNA(sections))
  expect_true(all(diff(sections) > 0))
  irt = output[seq(sections[2]+1, sections[3]-1)]
  expect_true(any(grepl("x.*NA.*NA", irt)))
  expect_false(any(grepl("^1 b|^2 b|^3 b", irt)))
  expect_true(any(grepl("IRT estimates are reported only for continuous predictors", irt, fixed=TRUE)))
  note = "Note: IRT estimates are conditional on other continuous predictors being held at their means and categorical predictors at their reference levels. For predictors involved in interactions, estimates may differ at other moderator values."
  expect_true(note %in% irt)
  expect_null(attr(estimates(glm(y_bin ~ x, binomial_estimates_data, family=binomial)), "marginal_probabilities"))
  expect_false(any(grepl("Note: IRT", capture.output(print(estimates(glm(y_bin ~ x, binomial_estimates_data, family=binomial)))), fixed=TRUE)))
})

test_that("unique transformed thresholds use original predictor units", {
  d = binomial_estimates_data
  d$positive = exp(d$x)
  for (expression in c("log(positive)", "sqrt(positive)")) {
    model = glm(as.formula(paste("y_bin ~", expression, "* a + z")), d, family=binomial)
    for (location in c(.5, .7)) {
      summary = logistic_coefficients(model, location)
      threshold = summary$intercept_threshold[1]
      if (is.na(threshold)) next # sqrt may not reach the target on its domain.
      profile = d[rep(1, 3), ]
      profile$z = mean(d$z)
      profile$positive = threshold + c(-1e-6, 0, 1e-6)
      probabilities = predict(model, profile, type="response")
      expect_equal(unname(probabilities[2]), location, tolerance=1e-8)
      expect_equal(summary$instantaneous_slope[1], unname(diff(probabilities[c(1,3)])/2e-6), tolerance=1e-7)
      expect_equal(attr(estimates(model), "row.names"), names(coef(model)))
    }
  }
  nonlinear = glm(y_bin ~ x + I(x^2), d, family=binomial)
  expect_true(all(is.na(logistic_coefficients(nonlinear)$intercept_threshold)))
  expect_type(logistic_coefficients(nonlinear)$intercept_threshold, "double")
})

test_that("constant conditional curves and invalid target probabilities are handled", {
  model = glm(y_bin ~ x + z, binomial_estimates_data, family=binomial)
  model$coefficients["x"] = 0
  result = logistic_coefficients(model)
  expect_equal(result$instantaneous_slope[result$variable == "x"], 0)
  expect_true(is.na(result$intercept_threshold[result$variable == "x"]))
  for (location in list(0, 1, NA_real_, Inf, c(.2, .5))) {
    expect_error(logistic_coefficients(model, location), "strictly between")
  }
  intercept = estimates(glm(y_bin ~ 1, binomial_estimates_data, family=binomial))
  expect_length(intercept$raw.coefficients, 1)
  expect_true(is.na(intercept$intercept_threshold))
  expect_output(print(intercept), "IRT Estimates:")
})

test_that("prediction differences preserve the existing plus/minus SD definition", {
  model = glm(y_bin ~ x + z + a, binomial_estimates_data, family=binomial)
  result = estimates(model)
  for (variable in c("x", "z")) {
    profile = binomial_estimates_data[c(1,1), ]
    profile$x = mean(binomial_estimates_data$x)
    profile$z = mean(binomial_estimates_data$z)
    profile[[variable]] = mean(binomial_estimates_data[[variable]]) + c(1,-1)*sd(binomial_estimates_data[[variable]])
    expected = abs(round(diff(predict(model, profile, type="response")), 2))
    expect_equal(as.numeric(result[["Prediction Difference (+/- 1 SD)"]][attr(result, "row.names")==variable]), unname(expected))
  }
})

test_that("ordered factors retain fitted levels and categorical summaries", {
  d = binomial_estimates_data
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  model = glm(y_bin ~ x*b, d, family=binomial)
  result = estimates(model)
  expect_equal(attr(result, "row.names"), names(coef(model)))
  expect_identical(as.character(attr(result, "conditioning")$b), "z")
  expect_true(is.ordered(attr(result, "conditioning")$b))
  marginal = attr(result, "marginal_probabilities")
  expect_equal(marginal$Level, c("z", "x", "y"))
  for (i in seq_len(nrow(marginal))) {
    profile = d
    profile$b = ordered(marginal$Level[i], levels=levels(d$b))
    expect_equal(marginal$Probability[i], mean(predict(model, profile, type="response")), tolerance=1e-12)
  }
})
