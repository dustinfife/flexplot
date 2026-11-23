comparison_role_data = function() {
  withr::with_seed(235, {
    data = data.frame(x=rnorm(180), z=rnorm(180), a=factor(rep(c("A","B"),90)),
      b=factor(rep(c("low","middle","high"),60)))
    data$y = .3+.6*data$x+.2*data$z+.5*(data$a=="B")+rnorm(180)
    data$binary = rbinom(180,1,plogis(.2+.3*data$x+.4*(data$a=="B")))
    data
  })
}

comparison_role_layers = function(plot) Filter(function(layer)
  is.data.frame(layer$data) && "prediction" %in% names(layer$data), plot$layers)

expect_comparison_identity = function(plot) {
  layers = comparison_role_layers(plot)
  expect_gt(length(layers),0L)
  for (layer in layers) {
    expect_identical(rlang::as_label(if (is.null(layer$mapping$colour)) layer$mapping$fill else layer$mapping$colour), 'model')
    if (inherits(layer$geom,"GeomLine")) {
      expect_identical(rlang::as_label(layer$mapping$group), 'model')
      expect_identical(rlang::as_label(layer$mapping$linetype), 'model')
    }
  }
  built = ggplot2::ggplot_build(plot)
  colour = built$plot$scales$get_scales("colour")
  expect_length(unique(colour$map(colour$get_breaks())),2L)
  expect_identical(as.character(colour$get_labels()),c("Model 1","Model 2"))
}

test_that("ordinary LM and GLM comparisons reserve model aesthetics with categorical and continuous x", {
  skip_if_not_installed("jmvcore")
  data = comparison_role_data()
  for (module in c("glmbasic","glmgeneralized")) for (x in c("x","a")) {
    panels = if (x=="x") c("a","b") else c("b","z")
    for (count in 1:2) {
      values = list(out="y",preds=c("x","z","a","b"),compare=TRUE,graphic=FALSE,
        modelTerms=list("x","z","a","b"),comparisonTerms=list("x","a"),
        comparisonX=x,comparisonPanels=panels[seq_len(count)])
      if (module=="glmgeneralized") values$family = "gaussian"
      options = do.call(get(paste0(module,"Options"),asNamespace("flexplot"))$new,values)
      state = if (module=="glmbasic") flexplot:::jamovi_glm_comparison(options,data) else
        flexplot:::jamovi_glm_generalized_comparison(options,data)
      plot = if (module=="glmbasic") flexplot:::jamovi_glm_comparison_plot(state) else
        flexplot:::jamovi_glm_generalized_comparison_plot(state,se=TRUE)
      expect_comparison_identity(plot)
      roles = flexplot:::flexplot_axis_given(state$formula)
      expect_identical(roles$axis,x)
      expect_identical(roles$given,panels[seq_len(count)])
      expect_null(attr(state$formula,"jamovi_comparison_note"))
    }
  }
})

test_that("comparison fallback also retains groups no longer present in either fitted formula", {
  data = comparison_role_data()
  models = list(lm(y~x+a,data),lm(y~x,data))
  formula = flexplot:::jamovi_comparison_layout(models,data,x="x",group="z")
  expect_identical(flexplot:::flexplot_axis_given(formula)$axis,"x")
  expect_match(attr(formula,"jamovi_comparison_note"),"saved grouping choice is retained",fixed=TRUE)
  expect_error(flexplot:::jamovi_model_layout(models,data,x="x",group="z"),"predictors in the fitted model",fixed=TRUE)
})

test_that("LM saved grouping falls back visibly while estimates, standalone grouping and labels survive", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  data = comparison_role_data()
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(out="y",
    preds=c("x","a","b","z"),modelTerms=list("x","a","b","z"),comparisonTerms=list("x","a"),
    compare=TRUE,graphic=FALSE,comparisonX="x",plotX="x",plotGroup="a"),data=NULL)
  reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  coefficients = analysis$results$glmcat$asDF
  statistics = analysis$results$comparison$asDF
  state = analysis$results$comparisonplot$state
  original = flexplot:::jamovi_glm_comparison_plot(state)
  expected = lapply(comparison_role_layers(original),function(layer)layer$data)
  saved = list(list(key="temporarily unavailable",role="legendLevel",variable="a",value="A",display="Saved label",label="Custom A"))
  expect_true(jamovi_reporting_update(analysis,list(comparisonGroup="a",comparisonPlotLabelStore=saved)))
  expect_identical(analysis$options$comparisonGroup,"a")
  expect_identical(analysis$options$plotGroup,"a")
  expect_false(analysis$options$comparisonMagnet)
  expect_match(analysis$results$explanation$content,"x and panels only",fixed=TRUE)
  expect_equal(analysis$results$glmcat$asDF,coefficients)
  expect_equal(analysis$results$comparison$asDF,statistics)
  fallback = flexplot:::jamovi_glm_comparison_plot(analysis$results$comparisonplot$state)
  expect_comparison_identity(fallback)
  expect_equal(lapply(comparison_role_layers(fallback),function(layer)layer$data),expected)
  expect_true(suppressWarnings(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot)))
  expect_false(is.null(analysis$results$comparisonplot$state))
  expect_identical(analysis$options$comparisonPlotLabelStore[[1]]$label,"Custom A")
  # An explicit panel selection is honored; the saved group is not moved there.
  expect_true(jamovi_reporting_update(analysis,list(comparisonPanels="a")))
  expect_identical(analysis$options$comparisonGroup,"a")
  expect_identical(flexplot:::flexplot_axis_given(analysis$results$comparisonplot$state$formula)$given,"a")
  expect_true(jamovi_reporting_update(analysis,list(comparisonGroup=NULL)))
  expect_false(analysis$results$explanation$visible)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE,graphic=TRUE)))
  ordinary = flexplot:::jamovi_lm_display(lm(y~x+a+b+z,data),data,analysis$options)
  expect_true(any(vapply(comparison_role_layers(ordinary),function(layer)
    identical(rlang::as_label(layer$mapping$colour),"a"),logical(1))))
})

test_that("grouped native magnets identify models in separate facets without changing prediction geometry", {
  skip_if_not_installed("jmvcore")
  data = comparison_role_data()
  options = flexplot:::glmbasicOptions$new(out="binary",preds=c("a","b","z"),compare=TRUE,
    modelTerms=list("a","b","z"),comparisonTerms=list("a","z"),comparisonX="a",comparisonGroup="b")
  state = flexplot:::jamovi_glm_comparison(options,data)
  expect_identical(flexplot:::flexplot_axis_given(state$formula)$axis,c("a","b"))
  expect_null(attr(state$formula,"jamovi_comparison_note"))
  plot = flexplot:::jamovi_glm_comparison_plot(state)
  model1 = state$model1;model2 = state$model2
  original = flexplot:::jamovi_comparison_style(compare_fits(state$formula,state$data,model1=model1,model2=model2))
  expect_equal(lapply(comparison_role_layers(plot),function(layer)layer$data),
    lapply(comparison_role_layers(original),function(layer)layer$data))
  built = ggplot2::ggplot_build(plot)
  column = names(plot$facet$params$cols)
  expect_length(column,1L)
  expect_setequal(as.character(built$layout$layout[[column]]),c("model1 (lm)","model2 (lm)"))
  # Every prediction PANEL belongs to exactly one competing model.
  layers = comparison_role_layers(plot)
  expect_true(all(vapply(layers,function(layer)all(as.character(layer$data[[column]])==as.character(layer$data$model)),logical(1))))
  expect_true(flexplot:::jamovi_comparison_magnet(data,"binary","a"))
  duplicate = flexplot:::jamovi_comparison_layout(list(model1,model2),data,"a",group="a",allow_magnet=TRUE)
  expect_identical(flexplot:::flexplot_axis_given(duplicate)$axis,"a")
  expect_match(attr(duplicate,"jamovi_comparison_note"),"saved grouping choice is retained",fixed=TRUE)
  data$binary = data$binary+2
  expect_false(flexplot:::jamovi_comparison_magnet(data,"binary","a"))
})

test_that("Mixed overlays reserve model identity but standalone and CAS grouping remain legitimate", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_comparison_analysis(graphic=TRUE,plotX="x",plotPanels="id",plotGroup="a",predictionType="population",clusters=16)
  expect_true(analysis$run())
  statistics = analysis$results$comparison$asDF
  fixed = analysis$results$fixed$asDF
  for (type in c("population","cluster")) {
    expect_true(jamovi_reporting_update(analysis,list(predictionType=type)))
    state = analysis$results$comparisonplot$state
    expect_false(is.null(state))
    plot = state$displayPlot
    expect_identical(flexplot:::jamovi_mixed_comparison_plot(state,"x","id","a",type,16),plot)
    expect_comparison_identity(plot)
    expect_match(analysis$results$comparisonPlotNote$content,"saved grouping choice is retained",fixed=TRUE)
    expect_identical(analysis$options$plotGroup,"a")
    expect_equal(analysis$results$comparison$asDF,statistics)
    expect_equal(analysis$results$fixed$asDF,fixed)
    withr::with_seed(419, {
      baseline = flexplot:::jamovi_mixed_comparison_plot(state,"x","id",NULL,type,16)
      expect_equal(lapply(comparison_role_layers(plot),function(layer)layer$data),
        lapply(comparison_role_layers(baseline),function(layer)layer$data))
    })
  }
  # The directional CAS ghost and grouping stay in separate model-specific views.
  expect_true(jamovi_reporting_update(analysis,list(predictionType="cas",plotPanels=NULL,casGhost=TRUE)))
  plot = analysis$results$comparisonplot$state$displayPlot
  expect_length(attr(plot,"jamovi_cas_children"),2L)
  expect_true(attr(plot,"jamovi_cas_ghost"))
  expect_null(attr(plot,"jamovi_comparison_note"))
  expect_identical(analysis$options$plotGroup,"a")
  expect_match(analysis$results$comparisonPlotNote$content,"Model 2's fixed-effects predictions",fixed=TRUE)
  expect_equal(analysis$results$comparison$asDF,statistics)
  # Saved cluster-as-group layouts need an explicit panel choice, never an implicit move.
  expect_true(jamovi_reporting_update(analysis,list(predictionType="cluster",plotGroup="id",plotPanels=NULL)))
  expect_null(analysis$results$comparisonplot$state)
  expect_match(analysis$results$comparisonPlotNote$content,"place the shared grouping variable in panels",fixed=TRUE)
  expect_null(analysis$options$plotPanels)
  expect_identical(analysis$options$plotGroup,"id")
  expect_equal(analysis$results$comparison$asDF,statistics)
  expect_true(jamovi_reporting_update(analysis,list(plotPanels="id")))
  expect_false(is.null(analysis$results$comparisonplot$state))
  expect_match(analysis$results$comparisonPlotNote$content,"x and panels only",fixed=TRUE)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE,plotGroup="a",predictionType="population",clusters=16)))
  expect_false(is.null(analysis$results$plot$state))
  expect_false(analysis$results$comparisonPlotNote$visible)
})
