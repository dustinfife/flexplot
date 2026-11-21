test_that("LM legacy graphic choices cannot change fitted coordinates or confidence intervals", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(421)
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  withr::defer(ggplot2::theme_set(previous_theme))
  previous_theme = ggplot2::theme_get()
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  cases = list(
    list(preds=c("x","a"),modelTerms=list("x","a",c("x","a")),
         polynomials=TRUE,quadratic="x",plotX="x",plotPanels="a",formula=y~x*a+I(x^2)),
    list(preds=c("a","b"),modelTerms=list("a","b"),plotX="a",plotPanels="b",formula=y~a+b))
  legacy = list(c("Loess","Median + quartiles"),c("Regression","Mean + sterr"),c("Logistic","Mean + stdev"))
  for (case in cases) for (enabled in c(FALSE,TRUE)) {
    formula = case$formula;model_args = case;model_args$formula = NULL
    independent = lm(formula,small)
    baseline = NULL
    for (values in legacy) {
      options = do.call(flexplot:::glmbasicOptions$new,c(list(out="y",graphic=TRUE,se=enabled,
        line=values[1],center=values[2]),model_args))
      analysis = flexplot:::glmbasicClass$new(options=options,data=small)
      expect_true(analysis$run())
      set.seed(421)
      if (isTRUE(case$polynomials)) {
        # The existing full-range grid extrapolates in this small fixture.
        expect_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)),"beyond the range")
      } else expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
      plot = capture$plot
      built = ggplot2::ggplot_build(plot)
      # Raw observations and fitted geometries share identical coordinates for
      # all legacy settings, including categorical positions and facet layouts.
      if (is.null(baseline)) baseline = built else {
        expect_equal(built$data,baseline$data,tolerance=1e-12)
        expect_equal(built$layout$layout,baseline$layout$layout)
      }
      expect_false(any(vapply(plot$layers,function(layer) inherits(layer$stat,"StatSmooth"),logical(1))))
      interval = which(vapply(plot$layers,function(layer) inherits(layer$geom,c("GeomRibbon","GeomErrorbar")) &&
        is.data.frame(layer$data) && "prediction.lwr" %in% names(layer$data),logical(1)))
      expect_length(interval,as.integer(enabled))
      curves = Filter(function(layer) inherits(layer$geom,c("GeomLine","GeomPoint")) &&
        is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
      expect_true(length(curves)>0)
      for (layer in curves) {
        expected = predict(independent,newdata=layer$data,interval="confidence")
        expect_equal(layer$data$prediction,unname(expected[,"fit"]),tolerance=1e-10)
        if (enabled) {
          expect_equal(layer$data$prediction.lwr,unname(expected[,"lwr"]),tolerance=1e-10)
          expect_equal(layer$data$prediction.upr,unname(expected[,"upr"]),tolerance=1e-10)
        }
      }
      if (enabled) {
        source = plot$layers[[interval]]$data
        expected = predict(independent,newdata=source,interval="confidence")
        expect_equal(sort(built$data[[interval]]$ymin),sort(unname(expected[,"lwr"])),tolerance=1e-10)
        expect_equal(sort(built$data[[interval]]$ymax),sort(unname(expected[,"upr"])),tolerance=1e-10)
      }
    }
  }
})

test_that("LM comparisons ignore legacy graphic choices and retain original prediction streams", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(421)
  withr::local_options(device=function(...) grDevices::pdf(file=NULL))
  previous_theme = ggplot2::theme_get();withr::defer(ggplot2::theme_set(previous_theme))
  capture = new.env(parent=emptyenv())
  original = flexplot:::jamovi_glm_comparison_plot
  local_mocked_bindings(jamovi_glm_comparison_plot=function(state) {
    capture$plot = original(state);capture$plot
  })
  eligible = small
  expected = list(lm(y~x*a+I(x^2),eligible),lm(y~x+a,eligible))
  baseline = NULL
  for (enabled in c(FALSE,TRUE)) {
    options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),modelTerms=list("x","a",c("x","a")),
      polynomials=TRUE,quadratic="x",compare=TRUE,comparisonTerms=list("x","a"),
      comparisonX="x",comparisonPanels="a",se=enabled,line="Logistic",center="Mean + stdev")
    analysis = flexplot:::glmbasicClass$new(options=options,data=eligible)
    expect_true(analysis$run())
    set.seed(421)
    expect_warning(expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot)),"beyond the range")
    plot = capture$plot;built = ggplot2::ggplot_build(plot)
    if (is.null(baseline)) baseline = built$data else expect_equal(built$data,baseline,tolerance=1e-12)
    expect_false(any(vapply(plot$layers,function(layer) inherits(layer$geom,c("GeomRibbon","GeomErrorbar")),logical(1))))
    source = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    for (label in unique(as.character(source$model))) {
      model = expected[[if(grepl("model1",label))1 else 2]]
      rows = source[as.character(source$model)==label,,drop=FALSE]
      expect_equal(rows$prediction,unname(predict(model,newdata=rows)),tolerance=1e-10)
    }
    expect_equal(analysis$results$comparisonplot$state$statistics$aic,round(vapply(expected,AIC,numeric(1)),3))
  }
})
