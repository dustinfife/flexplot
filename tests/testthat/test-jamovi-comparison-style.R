test_that("jamovi comparison legends distinguish models without changing predictions", {
  skip_if_not_installed("jmvcore")
  set.seed(81)
  data = data.frame(x=seq(-1,1,length.out=120), a=factor(rep(c("A","B"),60)))
  for (family in c("gaussian", "binomial", "poisson")) {
    data$y = switch(family, gaussian=data$x+rnorm(120),
      binomial=rbinom(120,1,plogis(data$x)), poisson=rpois(120,exp(data$x)))
    options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family=family,
      compare=TRUE,comparisonX="x",modelTerms=list("x","a"),comparisonTerms=list("x"),graphic=FALSE)
    analysis = flexplot:::glmgeneralizedClass$new(options=options,data=data)
    expect_true(analysis$run())
    plot = flexplot:::jamovi_glm_generalized_comparison_plot(analysis$results$comparisonplot$state, se=TRUE)
    built = ggplot2::ggplot_build(plot)
    colour = built$plot$scales$get_scales("colour")
    expect_equal(as.character(colour$get_labels()), c("Model 1", "Model 2"))
    line = which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine") &&
      is.data.frame(layer$data) && "prediction" %in% names(layer$data), logical(1)))
    expect_setequal(unique(built$data[[line]]$linetype), c("solid", "dashed"))
    expect_equal(sort(built$data[[line]]$y), sort(plot$layers[[line]]$data$prediction))
    expect_no_warning(ggplot2::ggplotGrob(plot))
    notes = analysis$results$comparison$notes
    expect_match(notes$models$note, "\n\nModel 2:")
    expect_match(notes$bayes$note, "BIC")
  }
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),compare=TRUE,
    modelTerms=list("x","a"),comparisonTerms=list("x"),comparisonX="x",graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=small)
  expect_true(analysis$run())
  plot = flexplot:::jamovi_glm_comparison_plot(analysis$results$comparisonplot$state)
  built = ggplot2::ggplot_build(plot)
  expect_equal(as.character(built$plot$scales$get_scales("colour")$get_labels()), c("Model 1", "Model 2"))
  expect_no_warning(ggplot2::ggplotGrob(plot))
})
