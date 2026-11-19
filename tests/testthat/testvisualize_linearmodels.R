#context("visualize function on linear models")
set.seed(1212)
d = exercise_data

test_that("visualize function plots", {
  
  #### VISUALIZE FUNCTIONS -- Linear Models
  ## t-test
  mod = lm(weight.loss~rewards, data=d)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/ttest.svg", "ttest",visualize(mod)))
  
  ### regression
  mod = lm(weight.loss~motivation, data=d)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/regression.svg", "regression",visualize(mod)))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/regression-residuals-only.svg", "regression residuals only",visualize(mod, plot="residuals")))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/regression-model.svg", "regression model",visualize(mod, plot="model")))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/regression-model-with-alpha.svg", "regression model with alpha",visualize(mod, plot="model", alpha=.2)))

  ### ancova
  mod = lm(weight.loss~motivation + rewards, data=d)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/ancova.svg", "ancova",visualize(mod)))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/ancova-resids.svg", "ancova resids",visualize(mod, plot="residuals")))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/ancova-model.svg", "ancova model",visualize(mod, plot="model")))
  
  ### factorial anova
  mod = lm(weight.loss~gender + rewards, data=d)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/factorial-anova.svg", "factorial anova",visualize(mod)))

  
  ### multiple regression
  mod = lm(weight.loss~gender + rewards + motivation, data=d)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/multiple-regression.svg", "multiple regression",visualize(mod)))
  
  ### visualize with a formula provided
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/visualize-with-formula.svg", "visualize with formula",visualize(mod, formula = weight.loss~motivation + rewards | gender)))
  load(file=system.file("datasets", "small.rda", package="flexplot"))
  mod = lm(y~a + b + x + z, data=small)
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/four-variables.svg", "four variables",visualize(mod)))
  
  # visualize related t
  m = data.frame(difference = rnorm(100, 2))
  related_t_as_glm = lm(difference~1, data=m)
  expect_flexplot_snapshot("testvisualize_linearmodels/related-t-test.svg", "related t test", visualize(related_t_as_glm))
  
  ### categorical binomial models use their supplied fitted probabilities
  model = glm(a ~ b, data=small, family=binomial())
  p = visualize(model, plot="model")
  prediction_layers = Filter(function(layer) is.data.frame(layer$data) &&
                              "prediction" %in% names(layer$data), p$layers)
  expect_true(length(prediction_layers) > 0)
  for (layer in prediction_layers) {
    expect_equal(layer$data$prediction,
                 unname(predict(model, newdata=layer$data, type="response")))
  }
})

test_that("added.plot function", {
  load(file=system.file("datasets", "small.rda", package="flexplot"))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/avp.svg", "avp",
    added.plot(y~x + b + a, data=small, method="lm")))
  expect_error(added.plot(y~xx+b, method="lm"))
  tib = tibble::as_tibble(small)
  tibble.test = added.plot(y~x+a, data=tib, method="lm")
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/testing-tibbles.svg", "testing tibbles",tibble.test))
  suppressWarnings(expect_flexplot_snapshot("testvisualize_linearmodels/testing-missing-values-on-added-plot.svg", "testing missing values on added.plot",
    added.plot(weight.loss~therapy.type + muscle.gain.missing, data=exercise_data)))
})
