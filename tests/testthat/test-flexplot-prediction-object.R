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
  
  expect_s3_class(layer$geom, "GeomPoint")
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