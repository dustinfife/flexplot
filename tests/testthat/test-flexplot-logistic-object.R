test_that("native numeric binary outcomes use binomial fitted probabilities", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y_numb ~ x, data=small_logistic, method="logistic", se=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_identical(p$layers[[2]]$stat_params$method, "glm")
  expect_equal(p$layers[[2]]$stat_params$method.args$family$family, "binomial")
  expect_equal(built$data[[1]]$x, small_logistic$x)
  expect_equal(built$data[[1]]$y, small_logistic$y_numb)

  fit = stats::glm(y_numb ~ x, data=small_logistic, family=stats::binomial())
  smooth = built$data[[2]]
  expect_equal(smooth$y, unname(stats::predict(fit, newdata=smooth, type="response")))
  expect_true(all(smooth$y >= 0 & smooth$y <= 1))
})

test_that("native factor binary outcomes retain labels and probability coordinates", {
  withr::local_options(flexplot.object_renderer=TRUE)

  d = small_logistic
  d$y_numb = factor(d$y_numb, levels=c(0, 1), labels=c("No", "Yes"))
  p = flexplot(y_numb ~ x, data=d, method="logistic", se=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_equal(p$scales$get_scales("y")$breaks, c(0, 1))
  expect_equal(p$scales$get_scales("y")$labels, c("No", "Yes"))
  expect_equal(built$data[[1]]$y, as.numeric(d$y_numb) - 1)
  expect_s3_class(p$layers[[2]]$geom, "GeomSmooth")

  fit = stats::glm(y_numb ~ x, data=d, family=stats::binomial())
  smooth = built$data[[2]]
  expect_equal(smooth$y, unname(stats::predict(fit, newdata=smooth, type="response")))
})

test_that("native logistic conditioning fits each panel separately", {
  withr::local_options(flexplot.object_renderer=TRUE)

  p = flexplot(y_bin ~ x | a, data=small, method="logistic", se=FALSE)
  built = ggplot2::ggplot_build(p)
  expect_s3_class(p$layers[[2]]$stat, "StatSmooth")
  expect_equal(nrow(built$layout$layout), nlevels(small$a))

  for (i in seq_len(nrow(built$layout$layout))) {
    panel = built$layout$layout[i, ]
    d = small[small$a == panel$a, ]
    points = built$data[[1]][built$data[[1]]$PANEL == panel$PANEL, ]
    smooth = built$data[[2]][built$data[[2]]$PANEL == panel$PANEL, ]
    expect_equal(points$x, d$x)
    expect_equal(points$y, d$y_bin)
    expect_gt(nrow(smooth), 0)
    expect_equal(length(unique(smooth$group)), 1)
    fit = stats::glm(y_bin ~ x, data=d, family=stats::binomial())
    expect_equal(smooth$y, unname(stats::predict(fit, newdata=smooth, type="response")))
  }
})
