test_that("JASP intercept-only error bars preserve intervals without size deprecation", {
  local_options(lifecycle_verbosity="warning")
  model = lm(y ~ 1, data=small)
  results = list(linmod_results=list(object=list(model=model)))
  output = new.env(parent=emptyenv())
  options = list(dependent="y", variables=character(), ghost=FALSE, bw=FALSE,
                 alpha=.4, jitx=0, jity=0, theme="classic")
  # Supply only the host's column-name decoder; exercise the production helper.
  render = flexplot:::.create_flexplot_linmod
  environment(render) = list2env(list(decodeColNames=identity),
                                  parent=environment(render))
  expect_warning(render(results, output, options, "model"), NA)
  expect_warning({ built = ggplot2::ggplot_build(output$plotObject) }, NA)
  i = which(vapply(output$plotObject$layers,
                   function(layer) inherits(layer$geom, "GeomErrorbar"), logical(1)))
  expect_length(i, 1)
  interval = built$data[[i]]
  # Preserve the existing JASP interval definition rather than changing statistics.
  half_width = 1.96 * sd(small$y) / sqrt(length(small$y)-1)
  expect_equal(interval$ymin, mean(small$y)-half_width)
  expect_equal(interval$ymax, mean(small$y)+half_width)
  expect_equal(interval$x, 1)
  expect_equal(interval$xmin, .9)
  expect_equal(interval$xmax, 1.1)
  expect_equal(interval$linewidth, 1.25)
  expect_equal(interval$linetype, 1)
  expect_equal(interval$colour, "#bf0303")
})
