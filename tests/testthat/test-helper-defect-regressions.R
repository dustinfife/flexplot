test_that("zero-inflated comparisons use the supplied models", withr::with_seed(614, {
  skip_if_not_installed("pscl")
  d = data.frame(x=seq(-2, 2, length.out=400))
  d$y = ifelse(runif(nrow(d)) < .35, 0,
                rpois(nrow(d), exp(.3 + .5*d$x)))
  full = pscl::zeroinfl(y ~ x | 1, data=d)
  reduced = pscl::zeroinfl(y ~ 1 | 1, data=d)
  # This change repairs scoping, without changing the legacy statistic or df.
  expected = pchisq(logLik(full) - logLik(reduced), df=1, lower.tail=FALSE)
  expect_equal(get_p_value(full, reduced), expected)
  expect_equal(get_p_value(reduced, full),
                 pchisq(logLik(reduced) - logLik(full), df=1, lower.tail=FALSE))
  expect_true(is.na(get_p_value(full, reduced, nested=FALSE)))
}))

test_that("flipped data swaps positional aesthetics and preserves other columns", withr::with_seed(12, {
  d = data.frame(x=1:3, y=4:6, xmin=0:2, ymax=5:7,
                  xend=2:4, yintercept=7:9, colour=c("red","blue","red"))
  expected = d
  names(expected) = c("y", "x", "ymin", "xmax", "yend", "xintercept", "colour")
  expect_identical(flip_data(d, TRUE), expected)
  expect_identical(flip_data(flip_data(d, TRUE), TRUE), d)
  expect_identical(flip_data(d, FALSE), d)
  expect_identical(flip_data(d), d)
  d$flipped_aes = TRUE
  expect_identical(flip_data(d), ggplot2::flip_data(d, TRUE))
}))

test_that("horizontal grouped jitter-dodge preserves transposed coordinates", withr::with_seed(12, {
  d = expand.grid(category=factor(c("A","B")), group=factor(c("one","two")),
                    observation=1:3)
  d$value = seq_len(nrow(d))*.2
  vertical = ggplot2::ggplot(d, ggplot2::aes(category, value, colour=group)) +
    ggplot2::geom_point(position=position_jitterdodged(jitter.width=0,
                            jitter.height=0, seed=12))
  horizontal = ggplot2::ggplot(d, ggplot2::aes(value, category, colour=group)) +
    ggplot2::geom_point(position=position_jitterdodged(jitter.width=0,
                            jitter.height=0, seed=12))
  v = ggplot2::ggplot_build(vertical)$data[[1]]
  h = ggplot2::ggplot_build(horizontal)$data[[1]]
  h = h[match(v$y, h$x), ]
  expect_equal(as.numeric(v$x), as.numeric(h$y))
  expect_equal(v$y, h$x)
  expect_identical(v$colour, h$colour)
  expect_equal(nrow(h), nrow(d))
  expect_no_warning(ggplot2::ggplotGrob(horizontal))
}))

test_that("aligned logistic plots retain predictions with zero, one, or two facets", withr::with_seed(913, {
  d = expand.grid(x=seq(-2, 2, length.out=40),
                    a=factor(c("B","A"), levels=c("B","A")),
                    z=factor(c("high","low"), levels=c("high","low")),
                    q=factor(c("east","west")))
  d$y = rbinom(nrow(d), 1, plogis(-.4 + .6*d$x + .5*(d$a == "B") -
                    .3*(d$z == "high") + .25*(d$q == "east") + .2*d$x*(d$a == "B")))
  formulas = list(y ~ x + a, y ~ x + a | z, y ~ x + a | z + q)
  model_formulas = list(y ~ x*a, y ~ x*a*z, y ~ x*a*z*q)
  for (i in seq_along(formulas)) {
    fitted = glm(model_formulas[[i]], data=d, family=binomial())
    base = flexplot(formulas[[i]], d, method="logistic")
    p = logistic_aligned(base, resolution=12)
    values = p$data
    expect_equal(unname(values$prob), unname(predict(fitted, newdata=values, type="response")))
    at_zero = at_one = values
    at_zero$x = 0
    at_one$x = 1
    eta_zero = predict(fitted, newdata=at_zero, type="link")
    eta_one = predict(fitted, newdata=at_one, type="link")
    threshold = unname(-eta_zero/(eta_one-eta_zero))
    expect_equal(values$threshold, threshold, tolerance=1e-10)
    expect_equal(values$x_shifted, values$x-threshold, tolerance=1e-10)
    built = ggplot2::ggplot_build(p)
    expect_equal(nrow(built$layout$layout), 2^(i-1))
    expect_setequal(unique(built$data[[1]]$linetype), c("solid","dotted"))
    if (i > 1) expect_identical(levels(built$layout$layout$z), levels(d$z))
    if (i == 3) expect_identical(levels(built$layout$layout$q), levels(d$q))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
}))

test_that("mixed model visuals retain observations and actual fixed/random predictions", withr::with_seed(172, {
  d = small_mixed
  # Three clusters keep compare_fits' existing three-cluster reference sample
  # equal to the full fixture, so the independent standardization is exact.
  d = d[d$id %in% unique(d$id)[1:3], ]
  d$a = factor(d$a, levels=rev(levels(d$a)))
  fitted = lme4::lmer(y ~ a + x + (1 | id), data=d)
  for (formula in list(y ~ a, y ~ x)) {
    set.seed(17)
    p = mixed.mod.visual(formula, d, fitted, n=2)
    built = ggplot2::ggplot_build(p)
    categorical = identical(all.vars(formula)[2], "a")
    raw_index = if (categorical) 1 else 3
    fixed_index = if (categorical) 2 else 1
    random_index = if (categorical) 3 else 2
    selected = p$data
    expect_equal(length(unique(selected$id)), 2)
    expect_equal(nrow(selected), sum(d$id %in% as.character(unique(selected$id))))
    expect_equal(sort(built$data[[raw_index]]$y), sort(selected$y))
    expected_random = unname(predict(fitted, newdata=selected, re.form=NULL))
    expect_equal(sort(built$data[[random_index]]$y), sort(expected_random))
    profile = p$layers[[fixed_index]]$data
    expected_fixed = vapply(seq_len(nrow(profile)), function(j) {
      reference = d
      variable = all.vars(formula)[2]
      reference[[variable]] = rep(profile[[variable]][j], nrow(reference))
      mean(predict(fitted, newdata=reference, re.form=NA))
    }, numeric(1))
    expect_equal(profile$y, expected_fixed)
    expect_equal(nrow(built$data[[fixed_index]]), 2*nrow(profile))
    if (categorical) expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
}))

# Force the optional argument so a dangling lazy promise cannot go unnoticed.
defect_checked_get_fitted = function(model, re=FALSE, pred.values=NULL,
                                     pred.type="response", report.se=FALSE) {
  expect_false(report.se)
  if (is.null(pred.values)) pred.values = extract_data_from_fitted_object(model)
  generate_predictions(model, re, pred.values, pred.type, report.se)
}

test_that("mixed-model compatibility predictions inherit the supported SE policy", withr::with_seed(71, {
  d = small_mixed
  first = small_mixed_mod
  second = lme4::lmer(y ~ x + (1 | id), data=d)
  profile = d
  profile$x = profile$x + .25
  local_mocked_bindings(get_fitted=defect_checked_get_fitted)
  for (random in c(FALSE,TRUE)) {
    predictions = generate_predictions(first, random, profile, "response", FALSE)
    actual = post_prediction_process_cf(first, second, predictions,
                        y ~ x | id, re=random, k=profile, pred.type="response")
    expect_setequal(unique(actual$model), c("first","second"))
    expected_first = expected_second = profile
    expected_first$prediction = unname(predict(first, newdata=profile,
                                      re.form=if (random) NULL else NA))
    expected_second$prediction = unname(predict(second, newdata=profile,
                                       re.form=if (random) NULL else NA))
    expected_first$model = "first"
    expected_second$model = "second"
    # The compatibility helper establishes shared x bins after combining models.
    summarized = get_plotted_line(first, rbind(expected_first, expected_second), y ~ x | id)
    expect_equal(nrow(actual), nrow(summarized))
    for (column in names(summarized)) {
      expect_equal(actual[[column]], summarized[[column]])
    }
  }
}))
