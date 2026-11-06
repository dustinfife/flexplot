test_that("object renderer integrates with flexplot without changing its API", {
  old = options(flexplot.object_renderer=NULL)
  on.exit(options(old), add=TRUE)

  p = flexplot(mpg~wt, data=mtcars, method="lm", se=FALSE)
  expect_s3_class(p, "ggplot")
  expect_identical(p$formula, mpg~wt)
  expect_equal(length(p$layers), 2)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("object renderer supports facets and suppressed smoothing", {
  old = options(flexplot.object_renderer=TRUE)
  on.exit(options(old), add=TRUE)

  p = flexplot(mpg~wt | cyl, data=mtcars, suppress_smooth=TRUE)
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
  expect_silent(ggplot2::ggplot_build(p))
})

unexpected_formula_preprocessing = function(...) {
  stop("Formula preprocessing was called.")
}

unexpected_native_renderer = function(...) {
  stop("The native renderer was called.")
}

test_that("native rendering is the default", {
  withr::local_options(flexplot.object_renderer=NULL)

  p = flexplot(y ~ x, data=small, method="lm", se=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_equal(built$data[[1]]$y, small$y)
  fit = stats::lm(y ~ x, data=small)
  expect_equal(built$data[[2]]$y,
               unname(stats::predict(fit, newdata=built$data[[2]])))
})

test_that("the obsolete opt-out cannot select legacy rendering", {
  withr::local_options(flexplot.object_renderer=FALSE)

  p = flexplot(y ~ x, data=small, method="lm", se=FALSE, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_equal(built$data[[1]]$y, small$y)
  fit = stats::lm(y ~ x, data=small)
  expect_equal(built$data[[2]]$y,
               unname(stats::predict(fit, newdata=built$data[[2]])))
})

test_that("plot.string errors before preprocessing or either renderer can run", {
  withr::local_options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(
    formula_functions=unexpected_formula_preprocessing,
    flexplot_render_object=unexpected_native_renderer, .package="flexplot"
  )

  for (option in list(NULL, TRUE, FALSE)) {
    options(flexplot.object_renderer=option)
    expect_error(flexplot(y ~ x, small, plot.string=TRUE),
                   "String-based plotting has been retired", fixed=TRUE)
    expect_error(flexplot(y ~ x, small, plot.string=TRUE, return_data=TRUE),
                   "String-based plotting has been retired", fixed=TRUE)
  }
})

test_that("obsolete options cannot affect native preprocessing or supplied predictions", {
  withr::local_options(flexplot.object_renderer=NULL)
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  d$y_bin = factor(d$y_bin, levels=c(0, 1))
  model = glm(y_bin ~ a + z + b, d, family=binomial())
  predictions = compare_fits(y_bin ~ a + z | b, d, model, return.preds=TRUE)
  original = flexplot(y_bin ~ a + z | b, d, prediction=predictions, jitter=FALSE)
  original_data = flexplot(y_bin ~ a + z | b, d, prediction=predictions, return_data=TRUE, jitter=FALSE)
  expected = ggplot2::ggplot_build(original)
  for (option in list(NULL, TRUE, FALSE)) {
    options(flexplot.object_renderer=option)
    p = flexplot(y_bin ~ a + z | b, d, prediction=predictions, jitter=FALSE)
    actual_data = flexplot(y_bin ~ a + z | b, d, prediction=predictions, return_data=TRUE, jitter=FALSE)
    expect_equal(actual_data, original_data)
    expect_equal(levels(actual_data$a), c("b", "a"))
    expect_true("z_binned" %in% names(actual_data))
    expect_equal(ggplot2::ggplot_build(p)$data, expected$data)
    expect_equal(ggplot2::ggplot_build(p)$layout$layout, expected$layout$layout)
    expect_equal(p$data, original$data)
    expect_identical(p$formula, y_bin ~ a + z | b)
  }
})

renderer_calls_text_parser = function(object) {
  if (!is.function(object)) return(FALSE)
  any(c("parse", "parse_expr", "parse_exprs", "str2lang", "str2expression") %in%
        all.names(body(object)))
}

test_that("only the random-effects validator parses user syntax", {
  namespace = asNamespace("flexplot")
  objects = mget(ls(namespace, all.names=TRUE), envir=namespace, inherits=FALSE)
  parsers = names(objects)[vapply(objects, renderer_calls_text_parser, logical(1))]
  # Random-effects text is parsed without evaluation and its complete AST is
  # validated. Plot rendering must never acquire another text-parser path.
  expect_identical(parsers, "formula_random_component")
})

test_that("default native plots retain return_data and metadata for downstream consumers", {
  withr::local_options(flexplot.object_renderer=NULL)
  for (formula in list(y ~ x | a, y ~ a | b, y ~ 1 | a)) {
    p = flexplot(formula, small, jitter=FALSE)
    prepared = flexplot(formula, small, jitter=FALSE, return_data=TRUE)
    expect_equal(p$data, prepared)
    expect_identical(p$formula, formula)
  }
  m = lm(y ~ x + a, small)
  p = compare_fits(y ~ x | a, small, m, ghost.line="gray", ghost.reference=list(a="a"), jitter=FALSE)
  alias = compare.fits(y ~ x | a, small, m, ghost.line="gray", ghost.reference=list(a="a"), jitter=FALSE)
  expect_identical(compare.fits, compare_fits)
  expect_equal(ggplot2::ggplot_build(p)$data, ggplot2::ggplot_build(alias)$data)
  expect_equal(p$data, flexplot(y ~ x | a, small, return_data=TRUE))
  expect_identical(p$formula, y ~ x | a)
})
