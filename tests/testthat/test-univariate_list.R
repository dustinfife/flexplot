test_that("univariate_list works", {
  vdiffr::expect_doppelganger(
    "univariate_list", 
    univariate_list(c("a", "b", "z"), small)[[1]]
  )
})
