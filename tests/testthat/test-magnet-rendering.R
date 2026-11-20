magnet_ink_fixture = function(n) {
  one = data.frame(x=factor(rep(c("A", "B"), each=n)),
    y=c(rep(c(0, rep(1, 9)), n/10), rep(c(rep(0, 9), 1), n/10)))
  rbind(transform(one, panel="First"), transform(one, panel="Second"))
}

test_that("observation ink is bounded gray while positions and fitted dots are unchanged", {
  d = magnet_ink_fixture(400)
  d$group = factor(rep(c("one", "two"), length.out=nrow(d)))
  plots = list(flexplot(y ~ x | panel, data=d, jitter=FALSE),
    flexplot(y ~ x + group | panel, data=d, jitter=c(.03, .01)),
    magnet_plot(y ~ x | panel, data=d, jitter=FALSE))
  for (p in plots) {
    reference = p
    reference$layers[[2]] = ggplot2::ggproto(NULL, p$layers[[2]], geom=ggplot2::GeomPoint)
    built = withr::with_seed(902, ggplot2::ggplot_build(p))
    previous = withr::with_seed(902, ggplot2::ggplot_build(reference))
    expect_equal(built$data, previous$data)
    expect_equal(nrow(built$data[[2]]), nrow(d))
    expect_true(all(built$data[[2]]$shape == 1))
    expect_true(all(is.na(built$data[[3]]$alpha)))
    expect_identical(p$layers[[3]]$geom, ggplot2::GeomPoint)
    # Inspect the actual drawing primitives, after grouped offsets and scales.
    points = built$data[[2]]
    points = points[points$PANEL == points$PANEL[1], ]
    drawn = p$layers[[2]]$geom$draw_panel(points, built$layout$panel_params[[1]], built$layout$coord)
    ink = grDevices::col2rgb(drawn$gp$col, alpha=TRUE)
    expect_true(all(ink[1:3, ] == 166))
    expect_true(all(ink[4, ] > 0 & ink[4, ] < 255))
    if (!length(p$layers[[3]]$mapping$colour)) {
      expect_true(all(built$data[[3]]$colour == "blue"))
    }
  }
})

test_that("sparse and densely overlapping magnet observations render gray below fitted dots", {
  expect_flexplot_snapshot("magnet-rendering/sparse.svg", "magnet sparse gray observations",
    flexplot(y ~ x | panel, data=magnet_ink_fixture(10), jitter=FALSE))
  expect_flexplot_snapshot("magnet-rendering/dense.svg", "magnet dense gray observations",
    flexplot(y ~ x | panel, data=magnet_ink_fixture(400), jitter=FALSE))
})

test_that("rendered SVG keeps every dense observation gray and fitted markers blue", {
  skip_if_not_installed("vdiffr")
  d = magnet_ink_fixture(400)
  p = flexplot(y ~ x | panel, data=d, jitter=FALSE)
  file = withr::local_tempfile(fileext=".svg")
  vdiffr::write_svg(p, file, "magnet ink check")
  circles = readLines(file, warn=FALSE)
  circles = circles[grepl("^<circle ", circles)]
  expect_equal(sum(grepl("stroke: #A6A6A6;", circles, fixed=TRUE)), nrow(d))
  expect_equal(sum(grepl("fill: #0000FF;", circles, fixed=TRUE)), 4L)
  expect_false(any(grepl("stroke: #000000;", circles, fixed=TRUE)))
  expect_length(circles, nrow(d) + 4L)
})
