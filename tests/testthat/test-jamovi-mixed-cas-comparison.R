jamovi_cas_children = function(analysis) attr(analysis$results$comparisonplot$state$displayPlot,"jamovi_cas_children")

jamovi_cas_check_curves = function(state, children, variables) {
  for (i in 1:2) {
    model = state[[paste0("model",i)]]
    curve = attr(children[[i]],"cas_prediction")
    expected = vapply(seq_len(nrow(curve)),function(row) {
      profile = state$data
      for (variable in variables) profile[[variable]] = curve[[variable]][rep(row,nrow(profile))]
      mean(predict(model,newdata=profile,re.form=NA))
    },numeric(1))
    expect_equal(curve$prediction,unname(expected),tolerance=1e-10)
    expect_equal(children[[i]]$data$y_adj,
      unname(residuals(model)+predict(model,re.form=NA)),ignore_attr=TRUE,tolerance=1e-10)
  }
}

test_that("two-model CAS uses independent adjustments and shared marginal prediction conditions", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  analysis = jamovi_mixed_analysis(data,graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    plotPanels="z",modelTerms=list("x","z","a",c("x","z")),interactions=TRUE,
    polynomials=TRUE,quadratic="x",randomSlopes="x",comparisonTerms=list("a"),comparisonEstimation="ML",casGhost=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_false(is.null(state))
  children = jamovi_cas_children(analysis)
  expect_identical(vapply(children,function(p) p$labels$title,character(1)),c("Model 1","Model 2"))
  expect_true(lme4::isREML(state$model1)); expect_false(lme4::isREML(state$model2))
  jamovi_cas_check_curves(state,children,c("x","z"))
  curves = lapply(children,attr,which="cas_prediction")
  expect_equal(curves[[1]][c("x","z","z_binned")],curves[[2]][c("x","z","z_binned")])
  expect_equal(length(unique(curves[[2]]$prediction)),1L)
  expect_gt(max(abs(children[[1]]$data$y_adj-children[[2]]$data$y_adj)),.01)
  for (i in 1:2) {
    ghosts = Filter(function(layer) identical(layer$aes_params$linetype,"dashed"),children[[i]]$layers)
    expect_null(children[[i]]$labels$subtitle)
    expect_null(children[[i]]$labels$caption)
    expect_length(ghosts,if (i==1L) 1L else 0L)
    if (i==1L) {
      expect_equal(ghosts[[1]]$data,curves[[2]])
      expect_identical(ghosts[[1]]$aes_params$alpha,.55)
    }
  }
  expect_equal(children[[1]]$coordinates$limits,children[[2]]$coordinates$limits)
  coverage = range(unlist(lapply(children,function(p) c(p$data$y_adj,attr(p,"cas_prediction")$prediction))))
  expect_lte(children[[1]]$coordinates$limits$y[1],coverage[1])
  expect_gte(children[[1]]$coordinates$limits$y[2],coverage[2])
  expect_no_warning(invisible(patchwork::patchworkGrob(state$displayPlot)))
  models = list(state$model1,state$model2)
  local_mocked_bindings(jamovi_mixed_fit=function(...) stop("Unexpected refit"))
  expect_true(jamovi_reporting_update(analysis,list(casGhost=FALSE)))
  updated = analysis$results$comparisonplot$state
  expect_identical(updated$model1,models[[1]]);expect_identical(updated$model2,models[[2]])
  expect_equal(ggplot2::ggplot_build(jamovi_cas_children(analysis)[[2]])$data,
    ggplot2::ggplot_build(children[[2]])$data)
  expect_equal(attr(jamovi_cas_children(analysis)[[1]],"cas_prediction"),curves[[1]])
  for (child in jamovi_cas_children(analysis)) {
    expect_false(any(vapply(child$layers,function(layer) identical(layer$aes_params$linetype,"dashed"),logical(1))))
  }
})

test_that("CAS comparison synchronizes categorical encodings, cluster facets and custom labels", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  analysis = jamovi_mixed_analysis(data,graphic=TRUE,compare=TRUE,predictionType="cas",plotX="a",plotPanels="id",
    plotGroup="z",modelTerms=list("x","a","z"),comparisonTerms=list("x"),casGhost=TRUE)
  expect_true(analysis$run())
  children = jamovi_cas_children(analysis)
  jamovi_cas_check_curves(analysis$results$comparisonplot$state,children,c("a","id","z"))
  for (i in 1:2) {
    ghosts = Filter(function(layer) identical(layer$aes_params$linetype,"dashed"),children[[i]]$layers)
    expect_length(ghosts,if (i==1L) 1L else 0L)
    if (i==1L) expect_equal(ghosts[[1]]$data$prediction,attr(children[[2]],"cas_prediction")$prediction)
  }
  builds = lapply(children,ggplot2::ggplot_build)
  expect_equal(builds[[1]]$layout$layout,builds[[2]]$layout$layout)
  expect_equal(builds[[1]]$layout$panel_scales_x[[1]]$get_limits(),levels(data$a))
  expect_equal(builds[[1]]$layout$panel_scales_x[[1]]$get_limits(),builds[[2]]$layout$panel_scales_x[[1]]$get_limits())
  for (aesthetic in c("colour","shape")) {
    first = builds[[1]]$plot$scales$get_scales(aesthetic)
    second = builds[[2]]$plot$scales$get_scales(aesthetic)
    if (!is.null(first)) expect_equal(first$map(first$get_limits()),second$map(first$get_limits()))
  }
  labels = analysis$options$plotLabels
  for (i in seq_along(labels)) {
    if (labels[[i]]$role=="x") labels[[i]]$label="Category"
    if (labels[[i]]$role=="y") labels[[i]]$label="Adjusted outcome"
    if (labels[[i]]$role=="legend") labels[[i]]$label="Moderator"
    if (labels[[i]]$role=="columnTitle") labels[[i]]$label="Cluster"
    if (labels[[i]]$role=="facetLevel" && labels[[i]]$value=="1") labels[[i]]$label="First cluster"
  }
  labeled = flexplot:::jamovi_mixed_apply_labels(analysis$results$comparisonplot$state$displayPlot,labels)
  for (child in attr(labeled,"jamovi_cas_children")) {
    expect_identical(child$labels$x,"Category")
    expect_identical(child$labels$y,"Adjusted outcome")
    expect_identical(child$labels$colour,"Moderator")
    strip = child$facet$params$labeller(data.frame(id="1"))
    expect_match(strip[[1]],"Cluster.*First cluster")
  }
  expect_no_warning(invisible(patchwork::patchworkGrob(labeled)))
})

test_that("identical and singular CAS fits retain their own estimation samples and diagnostics", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$z[c(2,5)] = NA
  analysis = jamovi_mixed_analysis(data,graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x","z"),comparisonTerms=list("x","z"),casGhost=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  children = jamovi_cas_children(analysis)
  expect_equal(nrow(children[[1]]$data),nrow(data)-2L)
  expect_equal(children[[1]]$data$y_adj,children[[2]]$data$y_adj)
  expect_equal(attr(children[[1]],"cas_prediction"),attr(children[[2]],"cas_prediction"))
  state$data$x[1] = 999
  expect_error(flexplot:::jamovi_mixed_cas_comparison(state,"x"),"Data do not match")
  singular = small_mixed
  singular$y = 1+.4*singular$x+.01*sin(seq_len(nrow(singular)))
  analysis = jamovi_mixed_analysis(singular,graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x"),comparisonTerms=list("x"))
  expect_true(analysis$run())
  expect_true(lme4::isSingular(analysis$results$comparisonplot$state$model1))
  expect_no_warning(invisible(patchwork::patchworkGrob(analysis$results$comparisonplot$state$displayPlot)))
})

test_that("unordered categories and binary numeric displays keep common scales", {
  skip_if_not_installed("jmvcore")
  for (numeric in c(FALSE,TRUE)) {
    data = jamovi_mixed_test_data()
    if (numeric) data$x = as.numeric(data$x>0) else data$a = factor(data$a,levels=c("high","low","middle"))
    x = if (numeric) "x" else "a"
    analysis = jamovi_mixed_analysis(data,graphic=TRUE,compare=TRUE,predictionType="cas",plotX=x,
      modelTerms=list("x","a"),comparisonTerms=list("z"),casGhost=TRUE)
    expect_true(analysis$run())
    children = jamovi_cas_children(analysis)
    jamovi_cas_check_curves(analysis$results$comparisonplot$state,children,x)
    builds = lapply(children,ggplot2::ggplot_build)
    expect_equal(builds[[1]]$layout$panel_scales_x[[1]]$get_limits(),builds[[2]]$layout$panel_scales_x[[1]]$get_limits())
    expect_null(children[[1]]$coordinates$limits$x)
    expect_no_warning(invisible(patchwork::patchworkGrob(analysis$results$comparisonplot$state$displayPlot)))
  }
  expect_false(flexplot:::mixedmodelOptions$new()$casGhost)
})

test_that("a failed child CAS is identified and never leaves a partial comparison", {
  skip_if_not_installed("jmvcore")
  render = flexplot:::cas_render_model
  local_mocked_bindings(cas_render_model=function(formula,object,...) {
    if (!"x" %in% all.vars(stats::formula(object))) stop("child rendering probe")
    render(formula,object,...)
  })
  analysis = jamovi_mixed_analysis(graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x"),comparisonTerms=list("z"))
  expect_true(analysis$run())
  expect_identical(analysis$results$comparisonplot$status,"error")
  expect_null(analysis$results$comparisonplot$state)
  expect_match(analysis$results$comparisonplot$.__enclos_env__$private$.error,"Model 2 CAS: child rendering probe")
})
