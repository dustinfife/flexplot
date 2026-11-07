mixed_population_data = function() {
  d = lme4::sleepstudy
  d$w = as.numeric(d$Subject) + .1*sin(d$Days)
  d$Reaction = d$Reaction + .6*d$w + .12*d$Days*d$w
  d
}

mixed_population_predict = function(model, reference, coordinates, random) {
  vapply(seq_len(nrow(coordinates)), function(i) {
    profile = reference
    profile$Days = coordinates$Days[i]
    profile$Subject = coordinates$Subject[i]
    mean(predict(model, newdata=profile, re.form=if (random) NULL else NA))
  }, numeric(1))
}

mixed_population_forbid_refit = function(...) stop("Unexpected mixed-model refit")

test_that("mixed marginal curves use the full population for intercepts and slopes", withr::with_seed(41, {
  d = mixed_population_data()
  models = list(lme4::lmer(Reaction ~ Days*w + (1 | Subject), d),
                  lme4::lmer(Reaction ~ Days*w + (Days | Subject), d))
  local_mocked_bindings(lmer=mixed_population_forbid_refit,
                         refit=mixed_population_forbid_refit, .package="lme4")
  for (fitted in models) {
    coefficients = lme4::fixef(fitted)
    effects = lme4::ranef(fitted)
    for (random in c(FALSE, TRUE)) {
      actual = withr::with_seed(42,
        compare_fits(Reaction ~ Days | Subject, d, fitted, return.preds=TRUE,
                       num_points=4, re=random))
      expect_equal(length(unique(actual$Subject)), 3)
      expect_setequal(actual$model, if (random) c("fixed effects", "random effects") else "fitted (lmerMod)")
      selected = d[d$Subject %in% actual$Subject, ]
      expect_gt(nrow(d), nrow(selected))
      for (stream in unique(actual$model)) {
        curve = actual[actual$model == stream, ]
        conditional = random && stream == "random effects"
        expected = mixed_population_predict(fitted, d, curve, conditional)
        previous = mixed_population_predict(fitted, selected, curve, conditional)
        expect_equal(curve$prediction, expected, tolerance=1e-10)
        expect_gt(max(abs(expected-previous)), .01)
        if (!conditional) {
          by_cluster = split(curve$prediction, droplevels(curve$Subject))
          for (values in by_cluster) expect_equal(values, by_cluster[[1]])
        }
      }
      if (random) {
        conditional = actual[actual$model == "random effects", ]
        fixed = actual[actual$model == "fixed effects", ]
        rows = match(paste(conditional$Days, conditional$Subject), paste(fixed$Days, fixed$Subject))
        estimate = effects$Subject[as.character(conditional$Subject), , drop=FALSE]
        contribution = estimate[["(Intercept)"]]
        if ("Days" %in% names(estimate)) contribution = contribution + conditional$Days*estimate$Days
        expect_equal(conditional$prediction-fixed$prediction[rows], contribution, tolerance=1e-10)
      }
    }
    expect_identical(lme4::fixef(fitted), coefficients)
    expect_identical(lme4::ranef(fitted), effects)
  }
}))

test_that("mixed model pairs share the full common eligible population", withr::with_seed(42, {
  d = mixed_population_data()
  first = lme4::lmer(Reaction ~ Days*w + (1 | Subject), d)
  second = lme4::lmer(Reaction ~ Days*w + (Days | Subject), d, subset=Days > 0)
  eligible = d[rownames(d) %in% intersect(rownames(model.frame(first)), rownames(model.frame(second))), ]
  expect_equal(nrow(eligible), nrow(d)-nlevels(d$Subject))
  local_mocked_bindings(lmer=mixed_population_forbid_refit,
                         refit=mixed_population_forbid_refit, .package="lme4")
  for (random in c(FALSE, TRUE)) {
    actual = withr::with_seed(42,
      compare_fits(Reaction ~ Days | Subject, d, first, second,
                     return.preds=TRUE, num_points=4, re=random))
    expect_setequal(actual$model, c("first (lmerMod)", "second (lmerMod)"))
    expect_equal(length(unique(actual$Subject)), 3)
    expect_equal(range(actual$Days), range(eligible$Days))
    for (name in unique(actual$model)) {
      fitted = if (name == "first (lmerMod)") first else second
      curve = actual[actual$model == name, ]
      expected = mixed_population_predict(fitted, eligible, curve, random)
      selected = eligible[eligible$Subject %in% actual$Subject, ]
      expect_equal(curve$prediction, expected, tolerance=1e-10)
      expect_gt(max(abs(expected-mixed_population_predict(fitted, selected, curve, random))), .01)
      expect_gt(max(abs(expected-mixed_population_predict(fitted, d, curve, random))), .001)
    }
  }
}))

test_that("historical mixed methods retain selected-cluster predictions and RNG", withr::with_seed(43, {
  d = mixed_population_data()
  fitted = lme4::lmer(Reaction ~ Days*w + (Days | Subject), d)
  plotting = Reaction ~ Days | Subject
  prepared = withr::with_seed(42, {
    selected = prepare_data_for_compare_fits(d, fitted, plotting, clusters=3)
    list(data=droplevels(selected), seed=.Random.seed)
  })
  selected = prepared$data
  for (method in c("observed", "reference")) {
    for (random in c(FALSE, TRUE)) {
      result = withr::with_seed(42, {
        prediction = compare_fits(plotting, d, fitted, return.preds=TRUE,
                                    prediction_method=method, re=random)
        list(prediction=prediction, seed=.Random.seed)
      })
      actual = result$prediction
      expect_identical(result$seed, prepared$seed)
      expect_setequal(as.character(actual$Subject), as.character(selected$Subject))
      for (stream in unique(actual$model)) {
        curve = actual[actual$model == stream, ]
        conditional = random && stream == "random effects"
        if (method == "reference") {
          profile = curve
          profile$w = mean(selected$w)
          expect_equal(curve$prediction,
                         unname(predict(fitted, newdata=profile, re.form=if (conditional) NULL else NA)))
          next
        }
        edges = unique(quantile(selected$Days, seq(0, 1, length.out=10)))
        centers = (head(edges, -1)+tail(edges, -1))/2
        bins = cut(selected$Days, edges, include.lowest=TRUE, labels=FALSE)
        predicted = unname(predict(fitted, newdata=selected, re.form=if (conditional) NULL else NA))
        expected = aggregate(predicted, list(Days=centers[bins], Subject=selected$Subject), mean)
        rows = match(paste(round(curve$Days, 10), curve$Subject),
                       paste(round(expected$Days, 10), expected$Subject))
        expect_false(anyNA(rows))
        expect_equal(curve$prediction, expected$x[rows])
      }
    }
  }
}))

test_that("mixed marginal plots retain the selected observations and facets", withr::with_seed(44, {
  d = mixed_population_data()
  fitted = lme4::lmer(Reaction ~ Days*w + (Days | Subject), d)
  plotting = Reaction ~ Days | Subject
  selected = withr::with_seed(42,
    prepare_data_for_compare_fits(d, fitted, plotting, clusters=3))
  for (random in c(FALSE, TRUE)) {
    prediction = withr::with_seed(42,
      compare_fits(plotting, d, fitted, re=random, return.preds=TRUE, num_points=4))
    outside = any(prediction$prediction < min(d$Reaction) |
                    prediction$prediction > max(d$Reaction))
    # Limits use all eligible observations, while genuinely out-of-range
    # predictions still retain the existing truncation warning.
    if (outside) {
      expect_warning({
        p = withr::with_seed(42,
          compare_fits(plotting, d, fitted, re=random, jitter=FALSE, num_points=4))
      }, "truncating the y-axis")
    } else {
      expect_no_warning({
        p = withr::with_seed(42,
          compare_fits(plotting, d, fitted, re=random, jitter=FALSE, num_points=4))
      })
    }
    built = ggplot2::ggplot_build(p)
    expect_equal(p$coordinates$limits$y, range(d$Reaction))
    expect_equal(nrow(p$data), nrow(selected))
    expect_setequal(as.character(p$data$Subject), as.character(selected$Subject))
    expect_equal(nrow(built$layout$layout), 3)
    expect_setequal(as.character(built$layout$layout$Subject), as.character(selected$Subject))
    expect_equal(sort(built$data[[1]]$y), sort(selected$Reaction))
  }
}))
