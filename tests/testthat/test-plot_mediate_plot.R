context("mediate_plot()")

test_that("mediate_plot works", {
  #p = mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data)
  vdiffr::expect_doppelganger("mediate_plot with numeric",     mediate_plot(y~a + x, data=small))
  vdiffr::expect_doppelganger("mediate_plot with categorical", mediate_plot(y~x + a, data=small))
})
