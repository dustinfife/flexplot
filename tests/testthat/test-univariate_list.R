test_that("univariate_list works", {
  expect_flexplot_snapshot("univariate_list/univariate-list.svg", "univariate_list",
    univariate_list(c("a", "b", "z"), small)[[1]]
  )
})
