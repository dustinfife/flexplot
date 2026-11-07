#context("visualize function on linear models")
set.seed(1212)
d = exercise_data

test_that("visualize function plots", {
  
  #### VISUALIZE FUNCTIONS -- Linear Models
  ## t-test
  mod = lm(weight.loss~rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("ttest",visualize(mod)))
  
  ### regression
  mod = lm(weight.loss~motivation, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("regression",visualize(mod)))
  suppressWarnings(vdiffr::expect_doppelganger("regression residuals only",visualize(mod, plot="residuals")))
  suppressWarnings(vdiffr::expect_doppelganger("regression model",visualize(mod, plot="model")))
  suppressWarnings(vdiffr::expect_doppelganger("regression model with alpha",visualize(mod, plot="model", alpha=.2)))

  ### ancova
  mod = lm(weight.loss~motivation + rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("ancova",visualize(mod)))
  suppressWarnings(vdiffr::expect_doppelganger("ancova resids",visualize(mod, plot="residuals")))
  suppressWarnings(vdiffr::expect_doppelganger("ancova model",visualize(mod, plot="model")))
  
  ### factorial anova
  mod = lm(weight.loss~gender + rewards, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("factorial anova",visualize(mod)))

  
  ### multiple regression
  mod = lm(weight.loss~gender + rewards + motivation, data=d)
  suppressWarnings(vdiffr::expect_doppelganger("multiple regression",visualize(mod)))
  
  ### visualize with a formula provided
  suppressWarnings(vdiffr::expect_doppelganger("visualize with formula",visualize(mod, formula = weight.loss~motivation + rewards | gender)))
  load(file=system.file("datasets", "small.rda", package="flexplot"))
  mod = lm(y~a + b + x + z, data=small)
  suppressWarnings(vdiffr::expect_doppelganger("four variables",visualize(mod)))
  
  # visualize related t
  m = data.frame(difference = rnorm(100, 2))
  related_t_as_glm = lm(difference~1, data=m)
  vdiffr::expect_doppelganger("related t test", visualize(related_t_as_glm))
  
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
  suppressWarnings(vdiffr::expect_doppelganger("avp",
    added.plot(y~x + b + a, data=small, method="lm")))
  expect_error(added.plot(y~xx+b, method="lm"))
  tib = tibble::as_tibble(small)
  tibble.test = added.plot(y~x+a, data=tib, method="lm")
  suppressWarnings(vdiffr::expect_doppelganger("testing tibbles",tibble.test))
  suppressWarnings(vdiffr::expect_doppelganger("testing missing values on added.plot",
    added.plot(weight.loss~therapy.type + muscle.gain.missing, data=exercise_data)))
})
