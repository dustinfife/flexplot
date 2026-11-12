jamovi_plot_coordinates = function(data) {
  result = data.frame(x=as.numeric(data$x), panel=as.character(data$PANEL))
  result = result[order(result$panel, result$x), , drop=FALSE]
  rownames(result) = NULL
  result
}

test_that("jamovi model callbacks render supplied-model uncertainty only when requested", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  capture = new.env(parent=emptyenv())
  # Observe the real helper's return value; the actual R6 callback still prints it.
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  cases = list(numeric="x", categorical="a", conditioned=c("x", "a"),
                 grouped=c("x", "z", "a", "b"))
  for (predictors in cases) {
    for (enabled in c(FALSE, TRUE)) {
      options = flexplot:::glmbasicOptions$new(out="y", preds=predictors,
        graphic=TRUE, se=enabled, line="Regression", center="Mean + sterr")
      analysis = flexplot:::glmbasicClass$new(options=options, data=small)
      expect_true(analysis$run())
      image = analysis$results$plot
      expect_no_warning({ rendered = analysis$.__enclos_env__$private$.plot(image) })
      expect_true(rendered)
      plot = capture$plot
      built = ggplot2::ggplot_build(plot)
      independent = lm(image$state$formula, image$state$data)
      model_layers = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
        "prediction" %in% names(layer$data) &&
        inherits(layer$geom, c("GeomPoint", "GeomLine")), logical(1)))
      intervals = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
        "prediction" %in% names(layer$data) &&
        inherits(layer$geom, c("GeomRibbon", "GeomErrorbar")), logical(1)))
      expect_true(length(model_layers) > 0)
      expect_length(intervals, as.integer(enabled))
      for (i in model_layers) {
        source = plot$layers[[i]]$data
        expected = predict(independent, newdata=source, interval="confidence")
        expect_equal(source$prediction, unname(expected[, "fit"]), tolerance=1e-10)
        expect_equal(sort(built$data[[i]]$y), sort(unname(expected[, "fit"])), tolerance=1e-10)
        if (enabled) {
          expect_equal(source$prediction.lwr, unname(expected[, "lwr"]), tolerance=1e-10)
          expect_equal(source$prediction.upr, unname(expected[, "upr"]), tolerance=1e-10)
        } else {
          expect_false(any(c("se", "prediction.lwr", "prediction.upr") %in% names(source)))
        }
      }
      if (!enabled) next
      interval = plot$layers[[intervals]]
      source = interval$data
      expected = predict(independent, newdata=source, interval="confidence")
      expect_equal(sort(built$data[[intervals]]$ymin), sort(unname(expected[, "lwr"])), tolerance=1e-10)
      expect_equal(sort(built$data[[intervals]]$ymax), sort(unname(expected[, "upr"])), tolerance=1e-10)
      numeric_x = is.numeric(rlang::eval_tidy(interval$mapping$x, data=source))
      expect_s3_class(interval$geom, if (numeric_x) "GeomRibbon" else "GeomErrorbar")
      parent = model_layers[vapply(plot$layers[model_layers], function(layer)
        inherits(layer$geom, if (numeric_x) "GeomLine" else "GeomPoint"), logical(1))]
      expect_length(parent, 1)
      expect_equal(source, plot$layers[[parent]]$data)
      expect_equal(jamovi_plot_coordinates(built$data[[intervals]]),
                     jamovi_plot_coordinates(built$data[[parent]]))
      expect_false(interval$show.legend)
    }
  }
})

test_that("jamovi residual callbacks remain independent of model uncertainty", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  for (enabled in c(FALSE, TRUE)) {
    options = flexplot:::glmbasicOptions$new(out="y", preds="x", graphic=FALSE,
      graphicassump=TRUE, se=enabled, line="Regression")
    analysis = flexplot:::glmbasicClass$new(options=options, data=small)
    expect_true(analysis$run())
    image = analysis$results$assumpplot
    state = image$state
    expect_no_warning({ rendered = analysis$.__enclos_env__$private$.assumpplot(image) })
    expect_true(rendered)
    expect_equal(image$state, state)
  }
})
