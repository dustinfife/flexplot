test_that("object facets preserve absent and single-variable behavior", {
  expect_null(flexplot_panel_variables_object(NA_character_, character()))
  f = flexplot_panel_variables_object("group", character())
  expect_s3_class(f, "FacetGrid")
  expect_length(f$params$rows, 0)
  expect_identical(rlang::as_name(rlang::get_expr(f$params$cols[[1]])), "group")
})

test_that("two conditioning variables preserve row and column orientation", {
  f = flexplot_panel_variables_object(c("first", "second"), character())
  expect_identical(rlang::as_name(rlang::get_expr(f$params$rows[[1]])), "second")
  expect_identical(rlang::as_name(rlang::get_expr(f$params$cols[[1]])), "first")
})

test_that("blank first variable creates row-only facets", {
  f = flexplot_panel_variables_object(c("", "second"), character())
  expect_length(f$params$cols, 0)
  expect_identical(rlang::as_name(rlang::get_expr(f$params$rows[[1]])), "second")
})

test_that("binned variables are used and the existing labeller is retained", {
  f = flexplot_panel_variables_object(c("first", "second"), "first")
  expect_identical(rlang::as_name(rlang::get_expr(f$params$cols[[1]])), "first_binned")
  expect_identical(f$params$labeller, custom.labeler)
})

test_that("facet object can be added to ggplot", {
  d = data.frame(x = 1:4, y = 1:4, group = c("a", "a", "b", "b"))
  p = ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    flexplot_panel_variables_object("group", character())
  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
})
