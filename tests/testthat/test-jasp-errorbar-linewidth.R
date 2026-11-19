test_that("JASP intercept-only error bars match lm confidence intervals without size deprecation", {
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
  expected = stats::confint(model, level=.95)[1, ]
  expect_equal(interval$ymin, unname(expected[1]))
  expect_equal(interval$ymax, unname(expected[2]))
  expect_equal(interval$x, 1)
  expect_equal(interval$xmin, .9)
  expect_equal(interval$xmax, 1.1)
  expect_equal(interval$linewidth, 1.25)
  expect_equal(interval$linetype, 1)
  expect_equal(interval$colour, "#bf0303")
})

test_that("JASP intercept-only confidence intervals handle finite sample edge cases", {
  cases = list(two=c(1,4), three=c(-2,1,6),
    missing=c(NA,-3,0,4,9,NA), large=sin(seq_len(500))+seq_len(500)/100,
    constant=rep(7,6), singleton=2, nonfinite=c(NA,NaN,-Inf,1,3,8,Inf),
    empty=c(NA,NaN,-Inf,Inf))
  render = flexplot:::.create_flexplot_linmod
  environment(render) = list2env(list(decodeColNames=identity), parent=environment(render))
  options = list(dependent="y", variables=character(), ghost=FALSE, bw=FALSE,
                 alpha=.4, jitx=0, jity=0, theme="classic")
  for (name in names(cases)) {
    values = cases[[name]]
    finite = values[is.finite(values)]
    fitted = lm(y ~ 1, data=data.frame(y=if (length(finite)) finite else c(0,1)))
    # lm rejects infinite responses; inject them only to exercise defensive plot handling.
    displayed = fitted
    displayed$model = data.frame(y=values)
    output = new.env(parent=emptyenv())
    expect_warning(render(list(linmod_results=list(object=list(model=displayed))),
                          output, options, "model"), NA, info=name)
    plot = output$plotObject
    expect_equal(plot$data$y, finite, info=name)
    expect_warning({ built = ggplot2::ggplot_build(plot) }, NA, info=name)
    expect_warning(ggplot2::ggplotGrob(plot), NA, info=name)
    bar = which(vapply(plot$layers, function(layer) inherits(layer$geom,"GeomErrorbar"), logical(1)))
    point = which(vapply(plot$layers, function(layer)
      inherits(layer$geom,"GeomPoint") && inherits(layer$stat,"StatSummary"), logical(1)))
    if (!length(finite)) {
      expect_equal(nrow(built$data[[bar]]), 0L, info=name)
      expect_equal(nrow(built$data[[point]]), 0L, info=name)
      next
    }
    expect_equal(built$data[[point]]$y, mean(finite), info=name)
    if (length(finite) == 1L) {
      expect_true(is.na(built$data[[bar]]$ymin), info=name)
      expect_true(is.na(built$data[[bar]]$ymax), info=name)
      next
    }
    expected = if (length(unique(finite)) == 1L) rep(finite[1],2) else
      unname(stats::confint(fitted, level=.95)[1, ])
    expect_equal(built$data[[bar]]$ymin, expected[1], tolerance=1e-12, info=name)
    expect_equal(built$data[[bar]]$ymax, expected[2], tolerance=1e-12, info=name)
  }
})
