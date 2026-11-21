test_that("GLM comparison emphasis changes only curve width and band opacity on both scales", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
    out="y_bin",preds=c("x","a"),family="binomial",compare=TRUE,
    modelTerms=list("x","a"),comparisonTerms=list("x"),comparisonX="x",graphic=FALSE),small)
  expect_true(analysis$run())
  withr::local_seed(913)
  state = analysis$results$comparisonplot$state
  original = flexplot:::jamovi_glm_comparison_emphasis
  emphasize = FALSE
  local_mocked_bindings(jamovi_glm_comparison_emphasis=function(plot) {
    if (emphasize) original(plot) else plot
  },.package="flexplot")
  for (scale in c("probability","logit")) for (se in c(FALSE,TRUE)) {
    emphasize = FALSE
    set.seed(913)
    before = flexplot:::jamovi_glm_generalized_comparison_plot(state,se=se,scale=scale)
    baseline = ggplot2::ggplot_build(before)
    emphasize = TRUE
    set.seed(913)
    after = flexplot:::jamovi_glm_generalized_comparison_plot(state,se=se,scale=scale)
    built = ggplot2::ggplot_build(after)
    expect_length(after$layers,length(before$layers))
    lines = ribbons = integer()
    for (i in seq_along(after$layers)) {
      layer = after$layers[[i]]
      model_layer = is.data.frame(layer$data) && all(c("prediction","model") %in% names(layer$data))
      curve = model_layer && inherits(layer$geom,"GeomLine")
      band = model_layer && inherits(layer$geom,"GeomRibbon")
      expect_equal(layer$data,before$layers[[i]]$data)
      expect_equal(layer$mapping,before$layers[[i]]$mapping)
      remove = c(if(curve) "linewidth",if(band) "alpha")
      columns = setdiff(names(built$data[[i]]),remove)
      expect_equal(built$data[[i]][columns],baseline$data[[i]][columns])
      if (curve) {
        lines = c(lines,i)
        expect_equal(unique(built$data[[i]]$linewidth),1.2)
        expect_setequal(unique(built$data[[i]]$linetype),c("solid","dashed"))
      }
      if (band) {
        ribbons = c(ribbons,i)
        expect_equal(unique(built$data[[i]]$alpha),.13)
      }
    }
    expect_length(lines,1L)
    expect_equal(length(ribbons),if(se) 2L else 0L)
    for (aesthetic in c("colour","linetype")) {
      expect_equal(built$plot$scales$get_scales(aesthetic)$get_labels(),baseline$plot$scales$get_scales(aesthetic)$get_labels())
      expect_equal(built$plot$scales$get_scales(aesthetic)$map(built$plot$scales$get_scales(aesthetic)$get_limits()),
        baseline$plot$scales$get_scales(aesthetic)$map(baseline$plot$scales$get_scales(aesthetic)$get_limits()))
    }
    expect_equal(built$layout$panel_params,baseline$layout$panel_params)
    expect_no_warning(ggplot2::ggplotGrob(after))
  }
})
