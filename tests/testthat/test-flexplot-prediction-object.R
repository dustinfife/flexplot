testthat::test_that("Continuous prediction overlays use lines", {
  
  prediction = data.frame(
    x=small$x,
    prediction=small$y,
    model="Model 1"
  )
  
  layers = flexplot_prediction_layer(
    prediction=prediction,
    axis=c("x", NA),
    data=small
  )
  
  expect_type(layers, "list")
  expect_s3_class(layers[[1]]$geom, "GeomLine")
})


testthat::test_that("Categorical prediction overlays use points and lines", {
  
  prediction = data.frame(
    a=levels(small$a),
    prediction=c(0.5, 1),
    model="Model 1"
  )
  
  layers = flexplot_prediction_layer(
    prediction=prediction,
    axis=c("a", NA),
    data=small
  )
  
  expect_length(layers, 2)
  expect_s3_class(layers[[1]]$geom, "GeomPoint")
  expect_s3_class(layers[[2]]$geom, "GeomLine")
})


testthat::test_that("Magnet prediction overlays use points", {
  withr::local_options(flexplot.object_renderer=TRUE)
  
  prediction = data.frame(
    a=levels(small$a),
    prediction=c(0.25, 0.75),
    model="Model 1"
  )
  
  layer = flexplot_prediction_layer(
    prediction=prediction,
    axis=c("a", NA),
    data=small,
    use_magnet=TRUE
  )
  
  expect_s3_class(layer[[1]]$geom, "GeomPoint")
})

testthat::test_that("Continuous prediction overlays preserve coordinates", {
  
  prediction = data.frame(
    x=small$x,
    prediction=small$y,
    model="Model 1"
  )
  
  p = ggplot2::ggplot(small, ggplot2::aes(x=x, y=y)) +
    flexplot_prediction_layer(
      prediction=prediction,
      axis=c("x", NA),
      data=small
    )
  
  built = ggplot2::ggplot_build(p)
  
  expect_equal(
    sort(built$data[[1]]$y),
    sort(prediction$prediction)
  )
})

testthat::test_that("Logistic predictions remain on probability scale", {
  
  prediction = data.frame(
    x=small_logistic$x,
    prediction=stats::plogis(small_logistic$x),
    model="Logistic"
  )
  
  p = ggplot2::ggplot() +
    flexplot_prediction_layer(
      prediction=prediction,
      axis=c("x", NA),
      data=small_logistic
    )
  
  built = ggplot2::ggplot_build(p)
  
  expect_equal(
    sort(built$data[[1]]$y),
    sort(prediction$prediction)
  )
  
  expect_true(all(built$data[[1]]$y >= 0))
  expect_true(all(built$data[[1]]$y <= 1))
})

# Check coordinates and group membership, rather than accepting a build alone.
expect_grouped_prediction_layer = function(p, index, expected, xvar, groupvar) {
  built = ggplot2::ggplot_build(p)
  actual = built$data[[index]]
  colour = built$plot$scales$get_scales("colour")$map(expected[[groupvar]])
  x = expected[[xvar]]
  if (!is.numeric(p$data[[xvar]])) {
    x = as.numeric(factor(x, levels=levels(p$data[[xvar]])))
    actual$x = round(actual$x)
  }
  coordinates = data.frame(x=x, y=expected$prediction, colour=colour)
  coordinates = coordinates[order(coordinates$colour, coordinates$x), ]
  actual = actual[order(actual$colour, actual$x), ]

  expect_equal(as.numeric(actual$x), coordinates$x)
  expect_equal(actual$y, coordinates$y)
  expect_equal(actual$colour, coordinates$colour)

  if (!inherits(p$layers[[index]]$geom, "GeomLine")) return(invisible(NULL))

  groups = length(unique(expected[[groupvar]]))
  expect_equal(length(unique(actual$group)), groups)
  expect_equal(nrow(unique(actual[c("group", "colour")])), groups)
}

testthat::test_that("flexplot predictions use the base plot's numeric second-axis bins", {
  withr::local_options(flexplot.object_renderer=TRUE)

  prediction = expand.grid(
    x=c(-1, 0, 1),
    z=seq(min(small$z), max(small$z), length.out=9)
  )
  prediction$prediction = 10 * prediction$x + 100 * prediction$z
  prediction$model = "Model 1"
  labels = c("low", "middle", "high")
  p = flexplot(y ~ x + z, data=small, prediction=prediction,
               suppress_smooth=TRUE, breaks=list(z=c(-.5, .5)),
               labels=list(z=labels))

  prediction$z_binned = cut(
    prediction$z, breaks=c(min(small$z), -.5, .5, max(small$z)),
    labels=labels, include.lowest=TRUE
  )
  expected = aggregate(prediction ~ x + z_binned, data=prediction, FUN=mean)
  expect_identical(rlang::as_label(p$mapping$colour), "z_binned")
  expect_identical(rlang::as_label(p$layers[[2]]$mapping$group), "z_binned")
  expect_equal(levels(p$data$z_binned), labels)
  expect_grouped_prediction_layer(p, 2, expected, "x", "z_binned")
})

testthat::test_that("flexplot preserves categorical second-axis prediction curves", {
  withr::local_options(flexplot.object_renderer=TRUE)

  prediction = expand.grid(x=c(-1, 0, 1), b=levels(small$b))
  prediction$prediction = prediction$x + 10 * as.numeric(prediction$b)
  prediction$model = "Model 1"
  p = flexplot(y ~ x + b, data=small, prediction=prediction,
               suppress_smooth=TRUE)

  expect_grouped_prediction_layer(p, 2, prediction, "x", "b")
})

testthat::test_that("categorical-x prediction summaries connect within second-axis groups", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (groupvar in c("b", "z")) {
    prediction = expand.grid(a=levels(small$a), value=unique(small[[groupvar]]))
    names(prediction)[2] = groupvar
    prediction$prediction = as.numeric(prediction$a) +
      10 * as.numeric(prediction[[groupvar]])
    prediction$model = "Model 1"
    formula = stats::reformulate(c("a", groupvar), response="y")
    p = flexplot(formula, data=small, prediction=prediction,
                 suppress_smooth=TRUE)

    resolved = groupvar
    if (groupvar == "z") {
      resolved = "z_binned"
      prediction$z_binned = cut(
        prediction$z, breaks=quantile(small$z, seq(0, 1, length.out=4)),
        labels=levels(p$data$z_binned), include.lowest=TRUE
      )
    }
    expected = aggregate(prediction[c("prediction")],
                         by=prediction[c("a", resolved)], FUN=mean)
    expect_grouped_prediction_layer(p, 2, expected, "a", resolved)
    expect_grouped_prediction_layer(p, 3, expected, "a", resolved)
  }
})

testthat::test_that("compare.fits preserves categorical and numeric second-axis predictions", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (xvar in c("x", "a")) {
    for (groupvar in c("b", "z")) {
      formula = stats::reformulate(c(xvar, groupvar), response="y")
      model = stats::lm(formula, data=small)
      prediction = compare.fits(formula, data=small, model1=model, return.preds=TRUE)
      p = compare.fits(formula, data=small, model1=model)
      prediction$prediction = stats::predict(model, newdata=prediction)

      resolved = groupvar
      if (groupvar == "z") {
        resolved = "z_binned"
        prediction$z_binned = cut(
          prediction$z, breaks=quantile(small$z, seq(0, 1, length.out=4)),
          labels=levels(p$data$z_binned), include.lowest=TRUE
        )
      }
      expected = aggregate(prediction[c("prediction")],
                           by=prediction[c(xvar, resolved)], FUN=mean)
      expect_grouped_prediction_layer(p, 2, expected, xvar, resolved)
      if (xvar == "a") {
        expect_grouped_prediction_layer(p, 3, expected, xvar, resolved)
      }
    }
  }
})
