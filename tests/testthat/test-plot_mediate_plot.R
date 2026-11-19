context("mediate_plot()")

test_that("mediate_plot works", {
  #p = mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data)
  set.seed(12312)
  expect_flexplot_snapshot("plot_mediate_plot/mediate-plot-with-numeric.svg", "mediate_plot with numeric",     mediate_plot(y~a + x, data=small))
  expect_flexplot_snapshot("plot_mediate_plot/mediate-plot-with-categorical.svg", "mediate_plot with categorical", mediate_plot(y~x + a, data=small))
})

