magnet_comparison_layer = function(p, geom, prediction=FALSE) {
  for (i in seq_along(p$layers)) {
    layer = p$layers[[i]]
    if (!inherits(layer$geom, geom)) next
    if (prediction && !(is.data.frame(layer$data) && "prediction" %in% names(layer$data))) next
    return(i)
  }
  stop("Missing magnet comparison layer")
}

magnet_comparison_text = function(grob) {
  text = if (inherits(grob, "text")) as.character(grob$label) else character()
  for (child in grob$grobs) text = c(text, magnet_comparison_text(child))
  for (child in grob$children) text = c(text, magnet_comparison_text(child))
  text
}

expect_simplified_magnet = function(p, data, group=NULL, conditioning=character(), models=1) {
  expect_no_warning({ built = ggplot2::ggplot_build(p) })
  expect_no_warning({ grob = ggplot2::ggplotGrob(p) })
  point = magnet_comparison_layer(p, "GeomPoint", prediction=TRUE)
  line = magnet_comparison_layer(p, "GeomLine", prediction=TRUE)
  source = p$layers[[point]]$data
  markers = built$data[[point]]
  curves = built$data[[line]]
  facet_models = !is.null(group) && models > 1
  expect_equal(nrow(markers), nrow(source))
  expect_equal(sort(markers$y), sort(source$prediction))
  expect_equal(nrow(built$data[[2]]), nrow(data) * if (facet_models) models else 1)
  expect_true(all(built$data[[2]]$shape == 1))
  expect_true(all(built$data[[2]]$alpha == .13))
  expect_equal(built$data[[1]]$yintercept, rep(.5, nrow(built$layout$layout)))
  expect_true(all(built$data[[1]]$linetype == "dashed"))
  point_count = 0L
  for (layer in p$layers) {
    expect_false(inherits(layer$stat, "StatSummary"))
    if (!inherits(layer$geom, "GeomPoint")) next
    point_count = point_count + 1L
    expect_false(is.data.frame(layer$data) && "pred_prob" %in% names(layer$data))
  }
  expect_equal(point_count, 2L)

  # Calculate which model/group/panel combinations have enough x levels for a line.
  keys = c("model", group, conditioning)
  key = do.call(paste, source[keys])
  count = tapply(as.character(source$a), key, dplyr::n_distinct)
  expect_equal(nrow(curves), sum(count[key] > 1))
  coordinates = paste(markers$PANEL, markers$group, round(markers$x, 12),
                        round(markers$y, 12), markers$colour)
  line_coordinates = paste(curves$PANEL, curves$group, round(curves$x, 12),
                             round(curves$y, 12), curves$colour)
  expect_true(all(line_coordinates %in% coordinates))
  for (panel in unique(curves$PANEL)) {
    block = curves[curves$PANEL == panel, ]
    for (id in unique(block$group)) {
      drawn = block[block$group == id, ]
      expect_equal(length(unique(drawn$colour)), 1)
      expect_true(all(diff(as.numeric(drawn$x)) > 0))
    }
  }

  # Check group offsets and colors independently of the overlay's position object.
  if (!is.null(group)) {
    colors = built$plot$scales$get_scales("colour")
    levels = colors$get_limits()
    model_var = if (facet_models) names(built$layout$layout)[
      names(built$layout$layout) %in% setdiff(names(source), names(data)) &
        grepl("^Model", names(built$layout$layout))][1] else character()
    panel_vars = c(conditioning, model_var)
    panels = if (length(panel_vars)) {
      match(do.call(paste, source[panel_vars]), do.call(paste, built$layout$layout[panel_vars]))
    } else rep(1L, nrow(source))
    expected_x = as.numeric(source$a)
    if (isTRUE(getOption("flexplot.object_renderer"))) {
      expected_x = expected_x + .5 * ((match(as.character(source[[group]]), levels) - .5) / length(levels) - .5)
    }
    expected = paste(built$layout$layout$PANEL[panels], round(expected_x, 12),
                      colors$map(source[[group]]), round(source$prediction, 12))
    actual = paste(markers$PANEL, round(markers$x, 12), markers$colour, round(markers$y, 12))
    expect_setequal(actual, expected)
  }
  if (isTRUE(getOption("flexplot.object_renderer"))) {
    count_layer = magnet_comparison_layer(p, "GeomText")
    expect_equal(sum(p$layers[[count_layer]]$data$n_total), nrow(data))
    expect_equal(nrow(built$data[[count_layer]]),
                   nrow(p$layers[[count_layer]]$data) * if (facet_models) models else 1)
  }
  invisible(list(built=built, grob=grob, prediction=source, markers=markers, lines=curves))
}

test_that("single-model magnets contain only raw points and connected model markers", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = ordered(d$a, levels=c("b", "a"))
  m = stats::glm(y_bin ~ a + b, data=d, family=stats::binomial())
  for (method in c("observed", "reference")) {
    expected = compare_fits(y_bin ~ a, d, m, return.preds=TRUE, prediction_method=method)
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      p = compare.fits(y_bin ~ a, d, m, prediction_method=method, jitter=FALSE, alpha=.13)
      actual = expect_simplified_magnet(p, d)
      expect_equal(sort(actual$prediction$prediction), sort(expected$prediction))
      expect_equal(actual$built$layout$panel_scales_x[[1]]$get_limits(), levels(d$a))
      expect_equal(nrow(actual$markers), 2)
      expect_equal(length(unique(actual$lines$group)), 1)
      expect_equal(p$theme$legend.position, "none")
    }
  }
})

test_that("two-model ungrouped magnets show distinct models and a rendered legend", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::glm(y_bin ~ a + b, data=small, family=stats::binomial())
  m2 = stats::glm(y_bin ~ a, data=small, family=stats::binomial())
  for (method in c("observed", "reference")) {
    for (native in c(TRUE, FALSE)) {
      options(flexplot.object_renderer=native)
      p = compare_fits(y_bin ~ a | b, small, m1, m2,
                       prediction_method=method, jitter=FALSE, alpha=.13)
      actual = expect_simplified_magnet(p, small, conditioning="b", models=2)
      expect_setequal(actual$markers$shape, c(17, 15))
      expect_equal(length(unique(actual$markers$colour)), 2)
      expect_setequal(actual$lines$linetype, c("solid", "dotdash"))
      expect_true(all(abs(actual$markers$x - round(actual$markers$x)) == .125))
      text = magnet_comparison_text(actual$grob)
      expect_true(all(as.character(unique(actual$prediction$model)) %in% text))
      legends = actual$grob$grobs[grepl("^guide-box", actual$grob$layout$name)]
      expect_equal(sum(vapply(legends, inherits, logical(1), "gtable")), 1)
    }
  }
})

test_that("grouped comparison lines retain group slots and conditioning/model panels", {
  withr::local_options(flexplot.object_renderer=TRUE)
  d = small
  d$a = factor(d$a, levels=c("b", "a"))
  d$b = ordered(d$b, levels=c("z", "x", "y"))
  for (formula in list(y_bin ~ a + b, y_bin ~ a + b | z, y_bin ~ a + x | b)) {
    variables = all.vars(formula)[-1]
    m1 = stats::glm(stats::reformulate(variables, response="y_bin"), data=d, family=stats::binomial())
    m2 = stats::glm(y_bin ~ a, data=d, family=stats::binomial())
    group = if ("x" %in% variables) "x_binned" else "b"
    conditioning = if ("z" %in% variables) "z_binned" else
      if ("x" %in% variables) "b" else character()
    for (method in c("observed", "reference")) {
      for (native in c(TRUE, FALSE)) {
        options(flexplot.object_renderer=native)
        for (two in c(FALSE, TRUE)) {
          second = if (two) m2 else NULL
          p = compare_fits(formula, d, m1, second, prediction_method=method,
                           jitter=FALSE, alpha=.13)
          actual = expect_simplified_magnet(p, d, group, conditioning, if (two) 2 else 1)
          colors = actual$built$plot$scales$get_scales("colour")
          expect_false(any(as.character(actual$prediction$model) %in% colors$get_limits()))
          expect_identical(actual$built$plot$scales$get_scales("shape")$guide, "none")
          if (two) {
            expect_equal(length(unique(actual$built$layout$layout$COL)), 2)
            for (model in as.character(unique(actual$prediction$model))) {
              expect_true(any(grepl(model, magnet_comparison_text(actual$grob), fixed=TRUE)))
            }
          }
        }
      }
    }
  }
})

test_that("identical predictions keep both models and use matching marker/line offsets", {
  withr::local_options(flexplot.object_renderer=TRUE)
  m1 = stats::glm(y_bin ~ a, data=small, family=stats::binomial())
  m2 = m1
  for (method in c("observed", "reference")) {
    p = compare_fits(y_bin ~ a, small, m1, m2, prediction_method=method,
                     jitter=FALSE, alpha=.13)
    actual = expect_simplified_magnet(p, small, models=2)
    expect_equal(nrow(actual$markers), 4)
    expect_equal(length(unique(actual$markers$x)), 4)
    values = split(actual$prediction$prediction, actual$prediction$model)
    expect_equal(values[[1]], values[[2]])
  }
})

test_that("singleton prediction groups keep markers without drawing a line", {
  withr::local_options(flexplot.object_renderer=TRUE)
  # The numeric second-axis bins contain categories absent from some panels.
  m = stats::glm(y_bin ~ a + x + b, data=small, family=stats::binomial())
  p = compare_fits(y_bin ~ a + x | b, small, m, jitter=FALSE, alpha=.13)
  actual = expect_simplified_magnet(p, small, "x_binned", "b")
  expect_true(nrow(actual$lines) < nrow(actual$markers))
})

test_that("ordinary magnet plots retain their original summaries and opacity", {
  withr::local_options(flexplot.object_renderer=TRUE)
  for (native in c(TRUE, FALSE)) {
    options(flexplot.object_renderer=native)
    p = flexplot(y_bin ~ a + b, data=small, jitter=FALSE, alpha=.13)
    expect_no_warning({ built = ggplot2::ggplot_build(p) })
    expect_equal(length(p$layers), if (native) 4 else 3)
    expect_equal(nrow(built$data[[2]]), nrow(small))
    expect_true(all(built$data[[2]]$shape == 1))
    expect_true(all(built$data[[2]]$alpha == .13))
    expect_true(nrow(built$data[[3]]) > 0)
    expect_true(all(built$data[[3]]$shape == 19))
    if (native) expect_true("pred_prob" %in% names(p$layers[[3]]$data))
    else expect_true(inherits(p$layers[[3]]$stat, "StatSummary"))
  }
})
