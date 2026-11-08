
testthat::test_that("Related object plots preserve paired differences", {
  
  d = data.frame(
    person=c(1, 1, 2, 2),
    time=factor(c(1, 2, 1, 2)),
    score=c(15, 18, 11, 19)
  )
  
  
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

test_that("paired summaries use positional differences and full eligible data", {
  # Deliberately reverse the second condition's rows and factor levels. Pairing
  # follows row position within the encountered conditions, not subject IDs.
  first = small[small$a == "a", ]
  second = small[small$a == "b", ]
  n = min(nrow(first), nrow(second))
  d = rbind(first[seq_len(n), ], second[rev(seq_len(n)), ])
  d$a = ordered(d$a, levels=c("b", "a"))
  d$y[c(1, n + 1)] = NA
  eligible = d[!is.na(d$y), ]
  difference = eligible$y[eligible$a == "b"] - eligible$y[eligible$a == "a"]
  for (spread in c("quartiles", "stdev", "sterr")) {
    center = if (spread == "quartiles") median(difference) else mean(difference)
    interval = sd(difference)
    if (spread == "sterr") interval = 1.96 * interval / sqrt(length(difference) - 1)
    bounds = if (spread == "quartiles") quantile(difference, c(.25, .75)) else
      c(center - interval, center + interval)
    for (sample in c(Inf, 2)) {
      p = withr::with_seed(45, flexplot(y ~ a, d, related=TRUE, spread=spread,
                                         sample=sample, jitter=FALSE))
      built = ggplot2::ggplot_build(p)
      expect_equal(p$data$Difference, difference)
      expect_identical(p$labels$x, "Difference (b-a)")
      expect_equal(unname(vapply(p$layers, function(layer) class(layer$geom)[1], character(1))),
                     c("GeomHline", "GeomPoint", "GeomPoint", "GeomErrorbar"))
      expect_equal(built$data[[1]]$yintercept, 0)
      expect_equal(nrow(built$data[[2]]), min(sample, length(difference)))
      expect_true(all(built$data[[2]]$y %in% difference))
      if (is.infinite(sample)) expect_equal(built$data[[2]]$y, difference)
      expect_equal(built$data[[3]]$y, center)
      expect_equal(c(built$data[[4]]$ymin, built$data[[4]]$ymax), unname(bounds))
    }
  }
  expect_error(flexplot(y ~ a, eligible[-1, ], related=TRUE), "group sizes are identical")
})
