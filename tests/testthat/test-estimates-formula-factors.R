# Keep both outcomes at each mtcars profile so interaction fits are estimable.
formula_factor_data = mtcars[rep(seq_len(nrow(mtcars)), each=16), ]
formula_factor_probability = with(mtcars, plogis(.3 - .6*(wt-3) +
  .2*(cyl == 6) - .3*(cyl == 8) + .25*(wt-3)*(cyl == 6)))
formula_factor_data$am = as.integer(rep(seq_len(16), nrow(mtcars)) <=
  rep(pmax(1, pmin(15, floor(16*formula_factor_probability))), each=16))
formula_factor_data$x = formula_factor_data$cyl
formula_factor_data$exposure = exp(formula_factor_data$hp / 500)

test_that("the reported mtcars factor expression matches an explicit factor column", {
  model = glm(am ~ wt + factor(cyl), mtcars, family=binomial)
  data = mtcars
  data$cyl = factor(data$cyl)
  explicit = glm(am ~ wt + cyl, data, family=binomial)
  result = estimates(model)
  reference = estimates(explicit)
  expect_identical(class(result), "glm_estimates")
  expect_equal(attr(result, "row.names"), names(coef(model)))
  expect_identical(names(result), names(reference))
  for (column in names(result)) expect_equal(result[[column]], reference[[column]])
  expect_equal(attr(result, "irt"), attr(reference, "irt"))
  expect_equal(attr(result, "marginal_probabilities"), attr(reference, "marginal_probabilities"))
  expect_equal(attr(result, "marginal_probabilities")$Level, c("4", "6", "8"))
  expect_equal(attr(result, "continuous"), "wt")
  expect_equal(attr(result, "conditioning")$cyl, 4)
  expect_true(all(is.na(result$instantaneous_slope[grepl("factor", names(coef(model)), fixed=TRUE)])))
  expect_type(result$instantaneous_slope, "double")
  expect_type(result$intercept_threshold, "double")
  output = capture.output(print(result))
  sections = match(c("Raw Estimates:", "IRT Estimates:", "Standardized Estimates:",
                      "Marginal Predicted Probabilities:"), output)
  expect_false(anyNA(sections))
  expect_true(all(diff(sections) > 0))
  irt = output[seq(sections[2]+1L, sections[3]-1L)]
  expect_true(any(grepl("wt", irt, fixed=TRUE)))
  expect_false(any(grepl("cyl", irt, fixed=TRUE)))
})

test_that("factor expression interactions have correct conditional roots and derivatives", {
  for (formula in list(am ~ wt + factor(x), am ~ wt*factor(x))) {
    for (coding in list(contr.treatment(3), contr.treatment(3, base=2), contr.sum(3))) {
      data = formula_factor_data
      model = glm(formula, data, family=binomial,
                    contrasts=list("factor(x)"=coding))
      baseline = if (any(rowSums(abs(coding)) == 0)) which(rowSums(abs(coding)) == 0) else 1L
      value = c(4, 6, 8)[baseline]
      for (location in c(.5, .7)) {
        summary = logistic_coefficients(model, location)
        root = uniroot(function(wt) {
          predict(model, data.frame(wt=wt, x=value), type="link") - qlogis(location)
        }, c(-100, 100), tol=1e-12)$root
        probabilities = predict(model, data.frame(wt=root + c(-1e-5, 0, 1e-5), x=value), type="response")
        expect_equal(summary$intercept_threshold[summary$variable == "wt"], root, tolerance=1e-9)
        expect_equal(summary$instantaneous_slope[summary$variable == "wt"],
                       unname((probabilities[3]-probabilities[1])/2e-5), tolerance=1e-8)
        expect_equal(unname(probabilities[2]), location, tolerance=1e-9)
        expect_identical(summary$instantaneous_slope[summary$variable == "x"], NA_real_)
      }
      result = estimates(model)
      expect_equal(attr(result, "conditioning")$x, value)
      marginal = attr(result, "marginal_probabilities")
      for (i in seq_len(nrow(marginal))) {
        profile = data
        profile$x = as.numeric(marginal$Level[i])
        expect_equal(marginal$Probability[i], mean(predict(model, profile, type="response")), tolerance=1e-12)
      }
      explicit_data = data
      explicit_data$x = factor(data$x)
      explicit_formula = if (length(attr(terms(model), "term.labels")) == 2L) am ~ wt + x else am ~ wt*x
      explicit = glm(explicit_formula, explicit_data, family=binomial, contrasts=list(x=coding))
      expect_equal(attr(result, "irt"), attr(estimates(explicit), "irt"), tolerance=1e-9)
      expect_equal(marginal, attr(estimates(explicit), "marginal_probabilities"), tolerance=1e-12)
    }
  }
})

test_that("formula factor levels and labels retain valid original prediction inputs", {
  for (formula in list(am ~ wt*as.factor(x), am ~ wt*base::factor(x),
                       am ~ wt*ordered(x), am ~ wt*as.ordered(x),
                       am ~ wt*factor(x, levels=c(8,4,6), labels=c("eight","four","six")))) {
    data = formula_factor_data
    model = glm(formula, data, family=binomial)
    result = estimates(model)
    expect_identical(attr(result, "continuous"), "wt")
    marginal = attr(result, "marginal_probabilities")
    expect_equal(marginal$Level, unname(model$xlevels[[1]]))
    fitted = model.frame(model)[[3]]
    for (i in seq_len(nrow(marginal))) {
      profile = data
      profile$x = data$x[match(marginal$Level[i], as.character(fitted))]
      expect_equal(marginal$Probability[i], mean(predict(model, profile, type="response")), tolerance=1e-12)
    }
    ref_value = data$x[match(model$xlevels[[1]][1], as.character(fitted))]
    threshold = attr(result, "irt")$intercept_threshold[1]
    expect_equal(unname(predict(model, data.frame(wt=threshold, x=ref_value), type="response")), .5, tolerance=1e-9)
  }
})

test_that("offsets, transformations and eligible estimation rows survive factor expressions", {
  data = formula_factor_data
  data$wt[1:3] = NA_real_
  data$x[4:5] = NA_real_
  data$positive = exp(data$wt/3)
  for (formula in list(am ~ wt*factor(x) + offset(log(exposure)),
                       am ~ log(positive)*factor(x) + offset(log(exposure)))) {
    model = glm(formula, data, family=binomial)
    result = estimates(model)
    eligible = data[match(rownames(model.frame(model)), rownames(data)), ]
    marginal = attr(result, "marginal_probabilities")
    for (i in seq_len(nrow(marginal))) {
      profile = eligible
      profile$x = as.numeric(marginal$Level[i])
      expect_equal(marginal$Probability[i], mean(predict(model, profile, type="response")), tolerance=1e-12)
    }
    variable = if ("positive" %in% all.vars(formula)) "positive" else "wt"
    summary = logistic_coefficients(model, location=.7)
    threshold = summary$intercept_threshold[summary$variable == variable]
    profile = eligible[rep(1, 3), ]
    profile$exposure = mean(eligible$exposure)
    profile$x = 4
    profile[[variable]] = threshold + c(-1e-5, 0, 1e-5)
    probabilities = predict(model, profile, type="response")
    expect_equal(unname(probabilities[2]), .7, tolerance=1e-9)
    expect_equal(summary$instantaneous_slope[summary$variable == variable],
                   unname((probabilities[3]-probabilities[1])/2e-5), tolerance=1e-8)
  }
})

test_that("ambiguous or noninvertible factor constructions fail informatively", {
  data = formula_factor_data
  for (formula in list(am ~ wt + cut(x, c(0,5,7,9)), am ~ wt + factor(x %% 4))) {
    model = glm(formula, data, family=binomial)
    expect_error(estimates(model), "Unsupported categorical transformation.*explicit factor column")
  }
  model = glm(am ~ wt + factor(x, labels=c("A","B","C")), data, family=binomial)
  expect_error(estimates(model), "Cannot safely construct predictions.*explicit factor column")
  model = glm(am ~ wt + x + factor(x), data, family=binomial)
  expect_error(estimates(model), "multiple fitted representations.*separate explicit predictor columns")
})

test_that("binary numeric source columns remain categorical when wrapped in factor", {
  data = formula_factor_data
  model = glm(am ~ wt*factor(vs), data, family=binomial)
  result = estimates(model)
  data$vs = factor(data$vs)
  explicit = estimates(glm(am ~ wt*vs, data, family=binomial))
  expect_equal(attr(result, "irt"), attr(explicit, "irt"), tolerance=1e-9)
  expect_identical(attr(result, "continuous"), "wt")
  expect_equal(attr(result, "marginal_probabilities"), attr(explicit, "marginal_probabilities"), tolerance=1e-12)
  expect_equal(attr(result, "row.names"), names(coef(model)))
  expect_type(result$instantaneous_slope, "double")
  expect_true(all(is.na(result$instantaneous_slope[grepl("factor", names(coef(model)), fixed=TRUE)])))
})
