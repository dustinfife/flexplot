display_frames = function(plot) lapply(plot$layers,function(layer) layer$data)
display_band_count = function(plot) sum(vapply(plot$layers,function(layer)
  inherits(layer$geom,"GeomRibbon") || inherits(layer$geom,"GeomErrorbar") ||
    (inherits(layer$geom,"GeomSmooth") && isTRUE(layer$geom_params$se)),logical(1)))
display_logit = function(plot) identical(plot$scales$get_scales("y")$name,"Logit (log odds)")

test_that("GLM callbacks independently route bands, styles, scales and labels without changing inference", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  options = flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","z","a"),family="binomial",
    modelTerms=list("x","z","a"),comparisonTerms=list("x","a"),compare=TRUE,
    plotX="x",plotPanels="a",comparisonX="x",comparisonPanels="a",ghostLines=TRUE)
  analysis = jamovi_reporting_host(options,small)
  plots = new.env()
  main = flexplot:::jamovi_glm_generalized_plot
  comparison = flexplot:::jamovi_glm_generalized_comparison_plot
  local_mocked_bindings(jamovi_glm_generalized_plot=function(...) {plots$main=main(...);plots$main},
    jamovi_glm_generalized_comparison_plot=function(...) {plots$comparison=comparison(...);plots$comparison})
  expect_true(analysis$run())
  fit = analysis$results$plot$state
  tables = lapply(c("coefficients","irt","probabilities","comparison"),function(name)analysis$results[[name]]$asDF)
  render = function() {
    withr::with_seed(773, expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    withr::with_seed(773, expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot)))
  }
  render()
  main_data = display_frames(plots$main); comparison_data = display_frames(plots$comparison)
  expect_gt(display_band_count(plots$main),0L);expect_gt(display_band_count(plots$comparison),0L)
  expect_true(jamovi_reporting_update(analysis,list(comparisonSe=FALSE,comparisonPlotStyle="raw",comparisonPlotScale="logit")))
  render()
  expect_equal(display_frames(plots$main),main_data)
  expect_equal(display_band_count(plots$comparison),0L)
  expect_false(display_logit(plots$comparison))
  expect_match(analysis$results$comparisonPlotExplanation$content,"Probability scale is used",fixed=TRUE)
  expect_identical(options$comparisonPlotScale,"logit")
  expect_true(jamovi_reporting_update(analysis,list(comparisonPlotStyle="auto",comparisonSe=TRUE)))
  render();expect_true(display_logit(plots$comparison));expect_false(display_logit(plots$main))
  comparison_data = display_frames(plots$comparison)
  comparison_guide = analysis$results$comparisonPlotExplanation$content
  expect_true(jamovi_reporting_update(analysis,list(se=FALSE,plotStyle="raw",plotScale="logit")))
  render()
  expect_equal(display_frames(plots$comparison),comparison_data)
  expect_identical(analysis$results$comparisonPlotExplanation$content,comparison_guide)
  expect_equal(display_band_count(plots$main),0L);expect_gt(display_band_count(plots$comparison),0L)
  expect_false(display_logit(plots$main));expect_true(display_logit(plots$comparison))
  expect_true(jamovi_reporting_update(analysis,list(plotStyle="auto",se=TRUE)))
  render();expect_true(display_logit(plots$main))
  for (values in list(list(plotX="a",plotPanels=NULL),list(plotX="x",plotPanels="a"),
      list(comparisonX="a",comparisonPanels=NULL),list(comparisonX="x",comparisonPanels="a"),
      list(compare=FALSE),list(compare=TRUE))) {
    expect_true(jamovi_reporting_update(analysis,values))
    expect_identical(options$plotScale,"logit");expect_identical(options$comparisonPlotScale,"logit")
    expect_equal(coef(analysis$results$plot$state$model),coef(fit$model))
    expect_equal(vcov(analysis$results$plot$state$model),vcov(fit$model))
    expect_equal(predict(analysis$results$plot$state$model,type="response"),predict(fit$model,type="response"))
    if (options$compare) {
      render()
      expect_identical(display_logit(plots$main),identical(options$plotX,"x"))
      expect_identical(display_logit(plots$comparison),identical(options$comparisonX,"x"))
      expect_equal(lapply(c("coefficients","irt","probabilities","comparison"),
        function(name)analysis$results[[name]]$asDF),tables)
    }
  }
  # Route edits through the actual callback and keep stores independent of the other display.
  edit = function(rows,title) {
    index = which(vapply(rows,function(row)row$role=="title",logical(1)))[1]
    rows[[index]]$label = title;rows
  }
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=edit(options$plotLabels,"Ordinary only"),
    comparisonPlotLabels=edit(options$comparisonPlotLabels,"Comparison only"))))
  render()
  main_info = flexplot:::jamovi_glm_label_info(analysis$results$plot$state,options)
  expect_identical(flexplot:::jamovi_plot_apply_labels(plots$main,options$plotLabels,
    main_info$context,main_info$layout)$labels$title,"Ordinary only")
  store = options$comparisonPlotLabelStore
  keys = vapply(options$comparisonPlotLabels,`[[`,character(1),"key")
  expect_true(jamovi_reporting_update(analysis,list(plotScale="probability",plotStyle="raw",se=FALSE)))
  expect_identical(options$comparisonPlotLabelStore,store)
  expect_identical(vapply(options$comparisonPlotLabels,`[[`,character(1),"key"),keys)
  info = flexplot:::jamovi_glm_label_info(analysis$results$comparisonplot$state,options,TRUE)
  labeled = flexplot:::jamovi_plot_apply_labels(plots$comparison,options$comparisonPlotLabels,info$context,info$layout)
  expect_identical(labeled$labels$title,"Comparison only")
  # Family changes retain preferences and use the existing effective response scale.
  expect_true(jamovi_reporting_update(analysis,list(family="gaussian")))
  suppressWarnings(render()) # Existing Gaussian-on-binary range warning.
  expect_false(display_logit(plots$main));expect_false(display_logit(plots$comparison))
  expect_identical(options$comparisonPlotScale,"logit")
  expect_true(jamovi_reporting_update(analysis,list(family="binomial")))
  render();expect_true(display_logit(plots$comparison));expect_false(display_logit(plots$main))
})
