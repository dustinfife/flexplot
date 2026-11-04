
testthat::test_that("compare.fits renders continuous predictions", {
  
  m1 = stats::lm(y ~ x, data=small)
  m2 = stats::lm(y ~ x + z, data=small)
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = compare.fits(
    y ~ x,
    data=small,
    model1=m1,
    model2=m2
  )
  
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  
  prediction_layers = vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomLine") &&
      "prediction" %in% names(layer$data),
    logical(1)
  )
  
  expect_true(any(prediction_layers))
  
  pred_data = p$layers[[which(prediction_layers)[1]]]$data
  
  expect_equal(
    length(unique(pred_data$model)),
    2
  )
  
  expect_true(all(is.finite(pred_data$prediction)))
})


testthat::test_that("compare.fits renders categorical predictions", {
  
  m1 = stats::lm(y ~ a, data=small)
  m2 = stats::lm(y ~ a + z, data=small)
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = compare.fits(
    y ~ a,
    data=small,
    model1=m1,
    model2=m2
  )
  
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  
  prediction_points = vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomPoint") &&
      "prediction" %in% names(layer$data),
    logical(1)
  )
  
  expect_true(any(prediction_points))
})


testthat::test_that("Logistic predictions remain between zero and one", {
  
  m1 = stats::glm(
    y_numb ~ x,
    data=small_logistic,
    family=stats::binomial()
  )
  
  m2 = stats::glm(
    y_numb ~ x + y,
    data=small_logistic,
    family=stats::binomial()
  )
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = compare.fits(
    y_numb ~ x,
    data=small_logistic,
    model1=m1,
    model2=m2
  )
  
  expect_no_error(ggplot2::ggplot_build(p))
  
  prediction_layers = Filter(
    function(layer) "prediction" %in% names(layer$data),
    p$layers
  )
  
  expect_gt(length(prediction_layers), 0)
  
  pred = prediction_layers[[1]]$data$prediction
  
  expect_true(all(pred >= 0 & pred <= 1))
})


testthat::test_that("Logistic predictions respect factor level ordering", {
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  for (outcome in c("y_ord", "y_ord_rev")) {
    
    formula = stats::reformulate("x", response=outcome)
    
    m = stats::glm(
      formula,
      data=small_logistic,
      family=stats::binomial()
    )
    
    p = compare.fits(
      formula,
      data=small_logistic,
      model1=m
    )
    
    expect_no_error(ggplot2::ggplot_build(p))
    
    prediction_layers = Filter(
      function(layer) "prediction" %in% names(layer$data),
      p$layers
    )
    
    expect_gt(length(prediction_layers), 0)
    
    pred = prediction_layers[[1]]$data$prediction
    
    expect_true(all(pred >= 0 & pred <= 1))
  }
})


extract_prediction_data = function(p) {
  prediction_layers = Filter(
    function(layer) {
      is.data.frame(layer$data) &&
        all(c("prediction", "model") %in% names(layer$data))
    },
    p$layers
  )
  
  if (length(prediction_layers) == 0) {
    stop("No prediction layers found.")
  }
  
  prediction_layers[[1]]$data
}

testthat::test_that(
  "Object and legacy renderers use identical continuous predictions", {
    
    m1 = stats::lm(y ~ x, data=small)
    m2 = stats::lm(y ~ x + z, data=small)
    
    withr::local_options(flexplot.object_renderer=FALSE)
    
    legacy = compare.fits(
      y ~ x,
      data=small,
      model1=m1,
      model2=m2
    )
    
    options(flexplot.object_renderer=TRUE)
    
    object = compare.fits(
      y ~ x,
      data=small,
      model1=m1,
      model2=m2
    )
    
    legacy_pred = extract_prediction_data(legacy)
    object_pred = extract_prediction_data(object)
    
    columns = c("x", "prediction", "model")
    
    legacy_pred = legacy_pred[do.call(order, legacy_pred[columns]), columns]
    object_pred = object_pred[do.call(order, object_pred[columns]), columns]
    
    rownames(legacy_pred) = NULL
    rownames(object_pred) = NULL
    
    expect_equal(
      object_pred,
      legacy_pred,
      tolerance=1e-8
    )
  }
)
