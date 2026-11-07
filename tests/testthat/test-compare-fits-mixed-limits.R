test_that("mixed comparison limits use the full eligible range for every display selection", withr::with_seed(80, {
  fitted = lme4::lmer(y ~ x + (x | id), small_mixed)
  observed = range(small_mixed$y)
  selected_sets = character()
  narrower = FALSE
  for (seed in c(1, 2, 42)) {
    for (method in c("marginal", "observed", "reference")) {
      for (random in c(FALSE, TRUE)) {
        p = withr::with_seed(seed,
          compare_fits(y ~ x | id, small_mixed, fitted, prediction_method=method,
                         re=random, jitter=FALSE, num_points=4))
        built = ggplot2::ggplot_build(p)
        selected_sets = c(selected_sets, paste(sort(unique(as.character(p$data$id))), collapse="|"))
        selected = small_mixed[small_mixed$id %in% as.character(p$data$id), ]
        narrower = narrower || any(range(selected$y) != observed)
        expect_equal(p$coordinates$limits$y, observed)
        for (panel in built$layout$panel_params) {
          expect_lte(panel$y.range[1], observed[1])
          expect_gte(panel$y.range[2], observed[2])
          expect_gt(diff(panel$y.range), diff(observed))
        }
        expect_equal(length(unique(p$data$id)), 3)
        expect_equal(nrow(p$data), nrow(selected))
        expect_equal(sort(built$data[[1]]$y), sort(selected$y))
        expect_equal(nrow(built$layout$layout), 3)
        expect_s3_class(p$facet, "FacetGrid")
      }
    }
  }
  expect_true(narrower)
  expect_gt(length(unique(selected_sets)), 1)
}))

test_that("mixed comparison limits use common estimation rows rather than all supplied rows", withr::with_seed(81, {
  first = lme4::lmer(y ~ x + (x | id), small_mixed)
  extremes = c(which.min(small_mixed$y), which.max(small_mixed$y))
  eligible = small_mixed[-extremes, ]
  second = lme4::lmer(y ~ x + (1 | id), eligible)
  expect_false(identical(range(eligible$y), range(small_mixed$y)))
  for (method in c("marginal", "observed", "reference")) {
    p = withr::with_seed(42,
      compare_fits(y ~ x | id, small_mixed, first, second,
                     prediction_method=method, jitter=FALSE, num_points=4))
    expect_equal(p$coordinates$limits$y, range(eligible$y))
    selected = eligible[eligible$id %in% as.character(p$data$id), ]
    expect_equal(sort(p$data$y), sort(selected$y))
    built = ggplot2::ggplot_build(p)
    for (panel in built$layout$panel_params) {
      expect_lte(panel$y.range[1], min(eligible$y))
      expect_gte(panel$y.range[2], max(eligible$y))
    }
  }
}))

test_that("mixed observed limits retain facet scale modes", withr::with_seed(82, {
  fitted = lme4::lmer(y ~ x + (x | id), small_mixed)
  p = withr::with_seed(42, compare_fits(y ~ x | id, small_mixed, fitted, jitter=FALSE))
  for (mode in c("fixed", "free_x", "free_y", "free")) {
    faceted = p + ggplot2::facet_wrap(ggplot2::vars(id), scales=mode)
    built = ggplot2::ggplot_build(faceted)
    free_x = mode %in% c("free_x", "free")
    free_y = mode %in% c("free_y", "free")
    expect_identical(faceted$facet$params$free, list(x=free_x, y=free_y))
    expect_equal(length(unique(built$layout$layout$SCALE_X)), if (free_x) 3 else 1)
    expect_equal(length(unique(built$layout$layout$SCALE_Y)), if (free_y) 3 else 1)
    expect_equal(faceted$coordinates$limits$y, range(small_mixed$y))
    expect_setequal(as.character(built$layout$layout$id), as.character(p$data$id))
  }
}))

test_that("mixed limits do not expand to include out-of-range fitted predictions", withr::with_seed(83, {
  fitted = lme4::lmer(y ~ poly(x, 8) + (1 | id), small_mixed)
  prediction = withr::with_seed(1,
    compare_fits(y ~ x | id, small_mixed, fitted, re=TRUE,
                   return.preds=TRUE, num_points=100))
  expect_lt(min(prediction$prediction), min(small_mixed$y))
  expect_warning({
    p = withr::with_seed(1,
      compare_fits(y ~ x | id, small_mixed, fitted, re=TRUE, jitter=FALSE, num_points=100))
  }, "truncating the y-axis")
  expect_equal(p$coordinates$limits$y, range(small_mixed$y))
  built = ggplot2::ggplot_build(p)
  expect_lt(min(prediction$prediction), built$layout$panel_params[[1]]$y.range[1])
}))
