reversal_unexpected_native_renderer = function(...) {
  stop("The native renderer was called for plot.string=TRUE.")
}

test_that("multicategory numeric plots reverse into ordinary categorical summaries", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    expect_message(
      { p = flexplot(b ~ x, data=small, jitter=FALSE) },
      "Multinomial probability plots are not yet supported.*axes were reversed"
    )
    expected = flexplot(x ~ b, data=small, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_identical(p$formula, x ~ b)
    expect_equal(built$data, ggplot2::ggplot_build(expected)$data)
    expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
    expect_s3_class(p$layers[[2]]$stat, "StatSummary")
    expect_s3_class(p$layers[[3]]$geom, "GeomErrorbar")
    expect_equal(built$data[[1]]$y, small$x)
    medians = aggregate(x ~ b, data=small, FUN=median)
    expect_equal(sort(built$data[[2]]$y), sort(medians$x))
  }
})

test_that("axis reversal preserves categorical and numeric conditioning", {
  withr::local_options(flexplot.object_renderer=TRUE)

  formulas = list(
    list(b ~ x | a, x ~ b | a),
    list(b ~ x | z, x ~ b | z),
    list(b ~ x | a + z, x ~ b | a + z)
  )
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (pair in formulas) {
      expect_message(
        { p = flexplot(pair[[1]], data=small, jitter=FALSE) },
        "axes were reversed"
      )
      expected = flexplot(pair[[2]], data=small, jitter=FALSE)
      built = ggplot2::ggplot_build(p)
      expected_built = ggplot2::ggplot_build(expected)
      expect_identical(p$formula, pair[[2]])
      expect_equal(built$data, expected_built$data)
      expect_equal(built$layout$layout, expected_built$layout$layout)
    }

    expect_message(
      {
        p = flexplot(b ~ x | z, data=small, jitter=FALSE,
                     breaks=list(z=c(-.5, .5)), labels=list(z=c("low", "middle", "high")))
      },
      "axes were reversed"
    )
    expect_equal(levels(p$data$z_binned), c("low", "middle", "high"))
    expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 3)
  }
})

test_that("axis reversal accepts character categories and preserves ordered category levels", {
  withr::local_options(flexplot.object_renderer=TRUE)

  ordered_data = small
  ordered_data$b = ordered(ordered_data$b, levels=c("z", "x", "y"))
  character_data = small
  character_data$b = as.character(character_data$b)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (d in list(ordered_data, character_data)) {
      expect_message({ p = flexplot(b ~ x, data=d, jitter=FALSE) }, "axes were reversed")
      expected = flexplot(x ~ b, data=d, jitter=FALSE)
      expect_equal(ggplot2::ggplot_build(p)$data, ggplot2::ggplot_build(expected)$data)
      if (is.ordered(d$b)) {
        expect_true(is.ordered(p$data$b))
        expect_identical(levels(p$data$b), levels(d$b))
      }
    }
  }
})

test_that("binary outcomes retain their original logistic plotting direction", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small
  d$binary = factor(d$y_bin, labels=c("No", "Yes"))
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (formula in list(y_bin ~ x, binary ~ x)) {
      expect_message(
        { p = flexplot(formula, data=d, method="logistic", se=FALSE, jitter=FALSE) },
        NA
      )
      built = ggplot2::ggplot_build(p)
      expect_identical(p$formula, formula)
      expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
      expect_equal(built$data[[1]]$x, d$x)
      expect_equal(built$data[[1]]$y, d$y_bin)
    }
    expect_message({ distribution = flexplot(b ~ 1 | x, data=d) }, NA)
    expect_identical(distribution$formula, b ~ 1 | x)
    expect_s3_class(distribution$layers[[1]]$stat, "StatCount")
    expect_equal(nrow(ggplot2::ggplot_build(distribution)$layout$layout), 3)
  }
})

test_that("multicategory model overlays are rejected instead of reversed", {
  withr::local_options(flexplot.object_renderer=TRUE)

  prediction = data.frame(x=small$x, prediction=small$y, model="Model 1")
  d = small
  d$b = ordered(d$b)
  model = MASS::polr(b ~ x, data=d, Hess=TRUE)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    expect_error(flexplot(b ~ x, data=small, prediction=prediction),
                  "Multinomial probability plots are not yet supported")
    expect_error(flexplot(b ~ I(x*2), data=small, prediction=prediction),
                  "Multinomial probability plots are not yet supported")
    for (comparison in list(compare.fits, compare_fits)) {
      for (return_predictions in c(TRUE, FALSE)) {
        expect_error(comparison(b ~ x, data=d, model1=model,
                                 return.preds=return_predictions),
                      "Multinomial probability plots are not yet supported")
      }
    }
  }
})

test_that("axis reversal preserves safely materialized transformations and formula environments", {
  withr::local_options(flexplot.object_renderer=TRUE)

  formula_env = new.env(parent=environment())
  formula = stats::as.formula("b ~ I(x*2) | a", env=formula_env)
  reversed = stats::as.formula("I(x*2) ~ b | a", env=formula_env)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    expect_message(
      { p = flexplot(formula, data=small, jitter=FALSE) },
      "axes were reversed"
    )
    expected = flexplot(reversed, data=small, jitter=FALSE)
    built = ggplot2::ggplot_build(p)
    expect_identical(environment(p$formula), formula_env)
    expect_equal(built$data, ggplot2::ggplot_build(expected)$data)
    expect_equal(as.numeric(built$data[[1]]$y), 2 * small$x)
  }
})

test_that("axis reversal rejects extra axes and unresolved formulas without dropping variables", {
  withr::local_options(flexplot.object_renderer=TRUE)

  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    for (formula in list(b ~ x + a, b ~ x + z, b ~ x*a, b ~ I(x + z),
                         b ~ x^2, cbind(b, a) ~ x)) {
      expect_error(flexplot(formula, data=small), "Automatic axis reversal requires")
    }
    expect_error(flexplot(b ~ x | a*z, data=small),
                  "Automatic axis reversal supports at most two additive conditioning variables")
    expect_error(flexplot(b ~ x | a + z + y_bin, data=small),
                  "Automatic axis reversal supports at most two additive conditioning variables")
    expect_error(flexplot(b ~ x, data=small, related=TRUE),
                  "Automatic axis reversal is not supported with related=TRUE", fixed=TRUE)
  }
})

test_that("reversed plots respect raw controls and legacy plot.string selection", {
  withr::local_options(flexplot.object_renderer=TRUE)
  withr::local_seed(123)

  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    expect_message(
      { p = flexplot(b ~ x, data=small, raw.data=FALSE, sample=8, alpha=.17, jitter=FALSE) },
      "axes were reversed"
    )
    built = ggplot2::ggplot_build(p)
    expect_equal(nrow(built$data[[1]]), 8)
    expect_true(all(built$data[[1]]$alpha == 0))
    medians = aggregate(x ~ b, data=small, FUN=median)
    expect_equal(sort(built$data[[2]]$y), sort(medians$x))
  }

  options(flexplot.object_renderer=TRUE)
  testthat::local_mocked_bindings(
    flexplot_render_object=reversal_unexpected_native_renderer, .package="flexplot"
  )
  expect_message(
    expect_output(
      { p = flexplot(b ~ x, data=small, jitter=FALSE, plot.string=TRUE) },
      "ggplot(small", fixed=TRUE
    ),
    "axes were reversed"
  )
  expect_identical(p$formula, x ~ b)
  expect_s3_class(p$layers[[2]]$stat, "StatSummary")
})
