visualize_binomial_curve_layer = function(p) {
  which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomSmooth") &&
                 is.data.frame(layer$data) && "prediction" %in% names(layer$data), logical(1)))
}

visualize_unexpected_glm_fit = function(...) stop("Unexpected model refit")

test_that("binomial visualizations preserve the supplied quadratic model and presentation", {
  withr::local_seed(123)
  d = data.frame(x=seq(-3, 3, length.out=500))
  d$y = rbinom(500, 1, plogis(1.5 - 1.8*d$x^2))
  model = glm(y ~ x + I(x^2), data=d, family=binomial())
  coefficients = coef(model)
  local_mocked_bindings(glm.fit=visualize_unexpected_glm_fit, .package="stats")
  for (formula in list(NULL, y ~ x)) {
    p = visualize(model, formula=formula, plot="model")
    index = visualize_binomial_curve_layer(p)
    expect_length(index, 1)
    expect_s3_class(p$layers[[index]]$stat, "StatIdentity")
    built = ggplot2::ggplot_build(p)
    curve = built$data[[index]]
    expected = predict(model, newdata=data.frame(x=curve$x), type="response")
    expect_equal(curve$y, unname(expected), tolerance=1e-12)
    link = predict(model, newdata=data.frame(x=curve$x), type="link", se.fit=TRUE)
    expect_equal(curve$ymin, unname(plogis(link$fit - qnorm(.975)*link$se.fit)), tolerance=1e-10)
    expect_equal(curve$ymax, unname(plogis(link$fit + qnorm(.975)*link$se.fit)), tolerance=1e-10)
    expect_true(all(curve$colour == "#3366FF"))
    expect_true(all(curve$fill == "#999999FF"))
    expect_true(all(curve$alpha == .4))
    expect_equal(nrow(curve), 80)
    overlay = p$layers[[length(p$layers)-1]]$data
    bins = cut(d$x, quantile(d$x, seq(0, 1, length.out=11)), include.lowest=TRUE)
    proportions = tapply(d$y, bins, mean)
    expect_equal(overlay$proportion, as.numeric(proportions[as.character(overlay$bin)]))
    expect_equal(sum(overlay$count), nrow(d))
    expect_equal(built$data[[length(p$layers)]]$yintercept, .5)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
  expect_identical(coef(model), coefficients)
})

test_that("binomial visualizations retain interaction-specific model curves and panels", {
  withr::local_seed(321)
  d = data.frame(x=seq(-2.5, 2.5, length.out=400),
                  a=factor(rep(c("one", "two"), 200), levels=c("two", "one")))
  d$y = rbinom(400, 1, plogis(-.3 + .7*d$x + .8*(d$a == "two") - 1.4*d$x*(d$a == "two")))
  model = glm(y ~ x*a, data=d, family=binomial())
  local_mocked_bindings(glm.fit=visualize_unexpected_glm_fit, .package="stats")
  p = visualize(model, plot="model")
  index = visualize_binomial_curve_layer(p)
  predictions = p$layers[[index]]$data
  expect_equal(predictions$prediction, unname(predict(model, newdata=predictions, type="response")))
  built = ggplot2::ggplot_build(p)
  curve = built$data[[index]]
  layout = built$layout$layout
  expect_identical(as.character(layout$a), levels(d$a))
  for (panel in layout$PANEL) {
    rows = curve$PANEL == panel
    profile = data.frame(x=curve$x[rows], a=layout$a[layout$PANEL == panel])
    expect_equal(curve$y[rows], unname(predict(model, newdata=profile, type="response")))
    expect_equal(length(unique(curve$group[rows])), 1)
  }
  grouped = visualize_binomial_model(model, y ~ x + a, d, 10, "dot")
  index = visualize_binomial_curve_layer(grouped)
  source = grouped$layers[[index]]$data
  built = ggplot2::ggplot_build(grouped)
  scale = built$plot$scales$get_scales("colour")
  expect_equal(length(unique(built$data[[index]]$group)), 2)
  expect_setequal(paste(built$data[[index]]$x, round(built$data[[index]]$y, 10), built$data[[index]]$colour),
                   paste(source$x, round(predict(model, newdata=source, type="response"), 10), scale$map(source$a)))
  expect_no_warning(ggplot2::ggplotGrob(grouped))
})

test_that("binomial model curves and confidence bands use the supplied inverse link", {
  withr::local_seed(123)
  d = data.frame(x=seq(-2, 2, length.out=400))
  d$y = rbinom(400, 1, pnorm(.4 - .5*d$x - .6*d$x^2))
  d$weight = rep(c(1, 4), 200)
  for (link_name in c("probit", "cloglog")) {
    model = glm(y ~ x + I(x^2), data=d, weights=weight, family=binomial(link=link_name))
    p = visualize(model, formula=y ~ x, plot="model")
    index = visualize_binomial_curve_layer(p)
    curve = ggplot2::ggplot_build(p)$data[[index]]
    profile = data.frame(x=curve$x)
    expect_equal(curve$y, unname(predict(model, newdata=profile, type="response")), tolerance=1e-12)
    expected = predict(model, newdata=profile, type="link", se.fit=TRUE)
    expect_equal(curve$ymin, unname(model$family$linkinv(expected$fit-qnorm(.975)*expected$se.fit)), tolerance=1e-10)
    expect_equal(curve$ymax, unname(model$family$linkinv(expected$fit+qnorm(.975)*expected$se.fit)), tolerance=1e-10)
    expect_identical(model$family$link, link_name)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("binomial visualization keeps bar and log-odds controls without refitting", {
  withr::local_seed(123)
  d = data.frame(x=seq(-3, 3, length.out=500))
  d$y = rbinom(500, 1, plogis(1.5 - 1.8*d$x^2))
  model = glm(y ~ x + I(x^2), data=d, family=binomial())
  local_mocked_bindings(glm.fit=visualize_unexpected_glm_fit, .package="stats")
  for (scale in c("probability", "logit")) {
    p = visualize(model, formula=y ~ x, plot="model", overlay_type="bar", scale=scale,
                    n_bins=7, se=FALSE, sample=1)
    index = visualize_binomial_curve_layer(p)
    built = ggplot2::ggplot_build(p)
    curve = built$data[[index]]
    expected = unname(predict(model, newdata=data.frame(x=curve$x), type="response"))
    if (scale == "logit") expected = qlogis(expected)
    expect_equal(curve$y, expected, tolerance=1e-10)
    expect_false(p$layers[[index]]$geom_params$se)
    expect_s3_class(p$layers[[length(p$layers)-1]]$geom, "GeomRect")
    expect_equal(sum(p$layers[[length(p$layers)-1]]$data$count), nrow(d))
    expect_equal(built$data[[length(p$layers)]]$yintercept, if (scale == "logit") 0 else .5)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})
