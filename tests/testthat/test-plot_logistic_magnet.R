test_that("magnet_plot works with valid binary data", {
  set.seed(123)
  df = data.frame(
    X = rep(c("A", "B", "C"), times = c(50, 300, 100)),
    Y = c(
      rbinom(50, 1, 0.8),
      rbinom(300, 1, 0.2),
      rbinom(100, 1, 0.5)
    )
  )
  
  p = magnet_plot(Y ~ X, df)
  
  expect_s3_class(p, "ggplot")
})


test_that("magnet_plot output is visually correct", {
  set.seed(123)
  df = data.frame(
    X = rep(c("A", "B", "C"), times = c(50, 300, 100)),
    Y = c(
      rbinom(50, 1, 0.8),
      rbinom(300, 1, 0.2),
      rbinom(100, 1, 0.5)
    )
  )
  
  p = magnet_plot(Y ~ X, data = df)
  
  vdiffr::expect_doppelganger("default magnet plot", p)
})

test_that("magnet plots ignore missing outcomes in binary validation and calculations", {
  for (kind in c("numeric", "factor", "ordered", "character")) {
    d = small
    d$y_bin[c(1, 8, 20)] = NA
    if (kind == "factor") d$y_bin = factor(d$y_bin, levels=c(0, 1, 2))
    if (kind == "ordered") d$y_bin = ordered(d$y_bin, levels=c(0, 1, 2))
    if (kind == "character") d$y_bin = as.character(d$y_bin)
    complete = d[!is.na(d$y_bin), , drop=FALSE]
    expect_true(is_binary(d$y_bin))
    for (renderer in list(magnet_plot, flexplot)) {
      expect_no_warning({ p = suppressMessages(renderer(y_bin ~ a, d, jitter=FALSE)) })
      expected = suppressMessages(renderer(y_bin ~ a, complete, jitter=FALSE))
      expect_no_warning({ built = ggplot2::ggplot_build(p) })
      expect_equal(built$data, ggplot2::ggplot_build(expected)$data, tolerance=1e-10)
      points = which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
      expect_equal(nrow(built$data[[points[1]]]), nrow(complete))
      counts = which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1)))
      expect_equal(sum(as.numeric(sub("N=", "", built$data[[counts]]$label))), nrow(complete))
    }
  }
  expect_error(magnet_plot(b ~ a, small), "exactly 2 levels")
})
