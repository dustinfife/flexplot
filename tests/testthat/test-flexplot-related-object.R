
testthat::test_that("Related object plots preserve paired differences", {
  
  d = data.frame(
    person=c(1, 1, 2, 2),
    time=factor(c(1, 2, 1, 2)),
    score=c(15, 18, 11, 19)
  )
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  p = flexplot(score ~ time, data=d, related=TRUE)
  
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  
  expect_equal(
    sort(p$data$Difference),
    c(3, 8)
  )
  
  expect_true(
    any(vapply(
      p$layers,
      function(layer) inherits(layer$geom, "GeomHline"),
      logical(1)
    ))
  )
})

testthat::test_that("Related plots support all summary types", {
  
  d = data.frame(
    person=rep(1:6, each=2),
    time=factor(rep(1:2, 6)),
    score=c(10, 12, 14, 17, 11, 15,
            18, 19, 13, 16, 20, 24)
  )
  
  withr::local_options(flexplot.object_renderer=TRUE)
  
  for (spread in c("quartiles", "stdev", "sterr")) {
    
    p = flexplot(
      score ~ time,
      data=d,
      related=TRUE,
      spread=spread
    )
    
    testthat::expect_no_error(
      ggplot2::ggplot_build(p)
    )
    
    summary_layers = Filter(
      function(layer) inherits(layer$stat, "StatSummary"),
      p$layers
    )
    
    expect_length(summary_layers, 2)
  }
})


testthat::test_that(
  "Related summary statistics match the legacy renderer", {
    
    d = data.frame(
      person=rep(1:6, each=2),
      time=factor(rep(1:2, 6)),
      score=c(
        10, 12,
        14, 17,
        11, 15,
        18, 19,
        13, 16,
        20, 24
      )
    )
    
    for (spread in c("quartiles", "stdev", "sterr")) {
      
      withr::local_options(flexplot.object_renderer=FALSE)
      
      legacy = flexplot(
        score ~ time,
        data=d,
        related=TRUE,
        spread=spread
      )
      
      options(flexplot.object_renderer=TRUE)
      
      object = flexplot(
        score ~ time,
        data=d,
        related=TRUE,
        spread=spread
      )
      
      legacy_built = ggplot2::ggplot_build(legacy)
      object_built = ggplot2::ggplot_build(object)
      
      legacy_idx = which(vapply(
        legacy$layers,
        function(layer) inherits(layer$geom, "GeomErrorbar"),
        logical(1)
      ))
      
      object_idx = which(vapply(
        object$layers,
        function(layer) inherits(layer$geom, "GeomErrorbar"),
        logical(1)
      ))
      
      testthat::expect_length(legacy_idx, 1)
      testthat::expect_length(object_idx, 1)
      
      legacy_summary = legacy_built$data[[legacy_idx]]
      object_summary = object_built$data[[object_idx]]
      
      testthat::expect_equal(
        object_summary$ymin,
        legacy_summary$ymin,
        tolerance=1e-8
      )
      
      testthat::expect_equal(
        object_summary$ymax,
        legacy_summary$ymax,
        tolerance=1e-8
      )
    }
  }
)