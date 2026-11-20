

test_that("ToothGrowth default and UI-initialized analyses populate the original results and plot", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  expected = lm(len ~ supp+dose, ToothGrowth)
  saved = NULL
  for (terms in list(NULL, list("supp", "dose"))) {
    options = flexplot:::glmbasicOptions$new(out="len", preds=c("supp", "dose"),
      modelTerms=terms, compare=FALSE)
    analysis = flexplot:::glmbasicClass$new(options=options, data=ToothGrowth)
    expect_true(analysis$run())
    state = analysis$results$plot$state
    expect_identical(attr(stats::terms(state$formula), "term.labels"), c("supp", "dose"))
    expect_equal(coef(lm(state$formula, state$data)), coef(expected), tolerance=1e-12)
    table = analysis$results$glmcat$asDF
    rows = match(c("(Intercept)", "dose"), table$var)
    coefficients = summary(expected)$coefficients[c("(Intercept)", "dose"), ]
    expect_equal(as.numeric(table$means[rows]), unname(coefficients[, 1]), tolerance=1e-12)
    expect_equal(as.numeric(table$lower[rows]), unname(confint(expected)[c("(Intercept)","dose"),1]))
    expect_equal(as.numeric(table$upper[rows]), unname(confint(expected)[c("(Intercept)","dose"),2]))
    profile = data.frame(supp=factor(levels(ToothGrowth$supp), levels=levels(ToothGrowth$supp)),
                         dose=mean(ToothGrowth$dose))
    intervals = predict(expected, profile, interval="confidence")
    factor_table = analysis$results$factorMeans$asDF
    rows = match(levels(ToothGrowth$supp), factor_table$levels)
    expect_equal(as.numeric(factor_table$means[rows]), unname(round(intervals[, "fit"], 2)))
    expect_equal(as.numeric(factor_table$lower[rows]), unname(round(intervals[, "lwr"], 2)))
    expect_equal(as.numeric(factor_table$upper[rows]), unname(round(intervals[, "upr"], 2)))
    differences = analysis$results$diff$asDF
    difference = unname(diff(intervals[, "fit"]))
    expect_equal(differences$diff, round(difference, 2))
    expect_equal(differences$cohensd, round(difference/sigma(expected), 2))
    rsq = analysis$results$rsq$asDF
    expect_equal(rsq$Estimate[rsq$var == "model"], round(summary(expected)$r.squared, 4))
    contributions = vapply(c("supp","dose"),function(term)
      summary(expected)$r.squared-summary(lm(reformulate(setdiff(c("supp","dose"),term),"len"),model.frame(expected)))$r.squared,numeric(1))
    expect_equal(unname(rsq$Estimate[match(c("supp", "dose"), rsq$var)]), unname(contributions), tolerance=1e-12)
    if (!is.null(saved)) expect_equal(list(table, factor_table, differences, rsq), saved)
    saved = list(table, factor_table, differences, rsq)
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    plot = capture$plot
    expect_no_warning({ built = ggplot2::ggplot_build(plot) })
    layers = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
      "prediction" %in% names(layer$data) && inherits(layer$geom, c("GeomPoint", "GeomLine")), logical(1)))
    expect_gt(length(layers), 0L)
    for (i in layers) {
      profile = plot$layers[[i]]$data
      # Low-cardinality numeric predictors retain numeric model values but are
      # ordered categories in the display data. Restore the fitted input class.
      profile$dose = as.numeric(as.character(profile$dose))
      predictions = predict(expected, newdata=profile)
      expect_equal(plot$layers[[i]]$data$prediction, unname(predictions), tolerance=1e-10)
      expect_equal(sort(built$data[[i]]$y), sort(unname(predictions)), tolerance=1e-10)
    }
  }
})
