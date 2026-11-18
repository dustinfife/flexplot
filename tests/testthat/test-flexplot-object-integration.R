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

test_that("only approved syntax-inspection helpers parse text", {
  namespace = asNamespace("flexplot")
  objects = mget(ls(namespace, all.names=TRUE), envir=namespace, inherits=FALSE)
  parsers = names(objects)[vapply(objects, renderer_calls_text_parser, logical(1))]
  # Both helpers inspect syntax without evaluating it. Keep this inventory exact
  # so an additional parser anywhere in the namespace requires explicit review.
  expect_identical(parsers, c("formula_random_component", "semi_partial_polynomial"))
})

test_that("polynomial term inspection never evaluates supplied expressions", {
  captured = new.env(parent=emptyenv())
  captured$executed = FALSE
  polynomial_security_probe = function() {
    captured$executed = TRUE
    stop("POLYNOMIAL_EXPRESSION_EXECUTED")
  }
  rlang::local_bindings(polynomial_security_probe=polynomial_security_probe, .env=globalenv())
  for (term in c("polynomial_security_probe()", "I(polynomial_security_probe()^2)",
                 "poly(polynomial_security_probe(), 2)", "poly(x, polynomial_security_probe())",
                 "{ polynomial_security_probe(); x }")) {
    expect_null(semi_partial_polynomial(term))
    expect_false(captured$executed)
  }
  expect_identical(semi_partial_polynomial("`polynomial_security_probe()`"),
    list(variable="polynomial_security_probe()", kind="linear"))
  expect_identical(semi_partial_polynomial("I(x^2)"), list(variable="x", kind="quadratic"))
  for (term in c("poly(x, 2)", "stats::poly(x, degree=2)",
                 "poly(x, 2, raw=polynomial_security_probe())")) {
    expect_identical(semi_partial_polynomial(term), list(variable="x", kind="poly"))
  }
  expect_false(captured$executed)
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
