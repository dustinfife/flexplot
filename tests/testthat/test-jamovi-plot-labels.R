label_test_grob_text = function(grob) {
  text = if (is.character(grob$label)) grob$label else character()
  for (child in c(grob$grobs,as.list(grob$children))) text = c(text,label_test_grob_text(child))
  text
}

label_test_edit = function(rows, role, text, variable=NULL) {
  index = which(vapply(rows,function(row)row$role==role && (is.null(variable) || row$variable==variable),logical(1)))[1]
  stopifnot(!is.na(index))
  rows[[index]]$label = text
  rows
}

test_that("shared catalogs and edits preserve plot data and isolate legend guides", {
  d = expand.grid(x=1:5,a=factor(c("A","B")),b=factor(c("A","B")))
  d$y = d$x+as.integer(d$a)
  plot = ggplot2::ggplot(d,ggplot2::aes(x,y,colour=a,shape=b))+
    ggplot2::geom_point()+ggplot2::facet_grid(rows=ggplot2::vars(b))+
    ggplot2::labs(title="Automatic title",subtitle="Automatic interpretation")
  context = list(output="model",outcome="y",meaning="response")
  layout = list(x="x")
  catalog = flexplot:::jamovi_plot_label_catalog(plot,layout,context)
  expect_setequal(unique(vapply(catalog,`[[`,character(1),"role")),
    c("title","subtitle","x","y","legend","legendLevel","rowTitle","facetLevel"))
  rows = label_test_edit(catalog,"title","Literal <>& ' ` name")
  rows = label_test_edit(rows,"subtitle","Custom interpretation")
  rows = label_test_edit(rows,"legend","Treatment","a")
  rows = label_test_edit(rows,"legendLevel","Arm A","a")
  rows = label_test_edit(rows,"rowTitle","Panel")
  rows = label_test_edit(rows,"facetLevel","First")
  labeled = flexplot:::jamovi_plot_apply_labels(plot,rows,context,layout)
  before = ggplot2::ggplot_build(plot);after = ggplot2::ggplot_build(labeled)
  expect_equal(after$data,before$data)
  expect_equal(after$layout$layout,before$layout$layout)
  expect_equal(after$layout$panel_params[[1]]$x.range,before$layout$panel_params[[1]]$x.range)
  expect_identical(after$plot$scales$get_scales("colour")$name,"Treatment")
  expect_true("Arm A" %in% after$plot$scales$get_scales("colour")$get_labels())
  expect_equal(after$plot$scales$get_scales("shape")$get_labels(),before$plot$scales$get_scales("shape")$get_labels())
  expect_true(all(c("Literal <>& ' ` name","Custom interpretation","Treatment","Arm A") %in%
    label_test_grob_text(ggplot2::ggplotGrob(labeled))))
  expect_true(any(grepl("Panel.*First",label_test_grob_text(ggplot2::ggplotGrob(labeled)))))
  expect_identical(flexplot:::jamovi_plot_apply_labels(plot,catalog,context,layout),plot)
  other = context;other$meaning="logit"
  expect_identical(flexplot:::jamovi_plot_apply_labels(plot,rows,other,layout),plot)
  expect_false(identical(flexplot:::jamovi_plot_label_prefix(context),flexplot:::jamovi_plot_label_prefix(other)))
  key = flexplot:::jamovi_plot_label_row
  expect_false(identical(key("x","a:b",display="x")$key,key("x:a","b",display="x")$key))
})

test_that("Linear Model has independent dynamic editors and actual rendered labels", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = small
  data$y = .5*data$x+seq(-40,40,length.out=nrow(data))
  analysis = lm_quadratic_analysis(data=data,graphic=TRUE,compare=TRUE,plotX="x",plotPanels="a",comparisonX="x",comparisonPanels="a",
    modelTerms=list("x","z","a",c("x","a")),comparisonTerms=list("x","a"),polynomials=TRUE,quadratic="x")
  expect_true(analysis$run())
  options = analysis$options
  expect_true(all(c("title","subtitle","x","y","facetLevel") %in% vapply(options$plotLabels,`[[`,character(1),"role")))
  expect_true("legendLevel" %in% vapply(options$comparisonPlotLabels,`[[`,character(1),"role"))
  expect_length(intersect(vapply(options$plotLabels,`[[`,character(1),"key"),vapply(options$comparisonPlotLabels,`[[`,character(1),"key")),0L)
  formula = analysis$results$plot$state$formula
  stats = analysis$results$comparison$asDF
  expect_true(jamovi_reporting_update(analysis,list(
    plotLabels=label_test_edit(options$plotLabels,"title","Primary title"),
    comparisonPlotLabels=label_test_edit(options$comparisonPlotLabels,"title","Comparison title"))))
  capture = new.env(parent=emptyenv());original = flexplot:::jamovi_plot_apply_labels
  local_mocked_bindings(jamovi_plot_apply_labels=function(plot,entries,context,layout) {
    result = original(plot,entries,context,layout);capture[[context$output]]=result;result
  },.package="flexplot")
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_identical(capture$model$labels$title,"Primary title")
  expect_identical(capture$comparison$labels$title,"Comparison title")
  expect_true("Comparison title" %in% label_test_grob_text(ggplot2::ggplotGrob(capture$comparison)))
  expect_equal(analysis$results$plot$state$formula,formula)
  expect_equal(analysis$results$comparison$asDF,stats)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE,graphic=FALSE)))
  expect_length(options$plotLabels,0L)
  expect_true(any(vapply(options$plotLabelStore,function(row)row$label=="Primary title",logical(1))))
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE,graphic=TRUE)))
  expect_true(any(vapply(options$comparisonPlotLabels,function(row)row$label=="Comparison title",logical(1))))
})

test_that("GLM editors retain edits across scales, failures and comparison switching", {
  skip_if_not_installed("jmvcore")
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","z","a"),family="binomial",
    plotX="x",plotPanels="a",comparisonX="x",comparisonPanels="a",compare=TRUE,
    modelTerms=list("x","z","a",c("x","a")),quadratic="x",comparisonTerms=list("x","a"),ghostLines=TRUE),small)
  expect_true(analysis$run())
  options = analysis$options
  estimates = analysis$results$coefficients$asDF
  comparison = analysis$results$comparison$asDF
  probability = options$plotLabels
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=label_test_edit(probability,"y","Chance"),
    comparisonPlotLabels=label_test_edit(options$comparisonPlotLabels,"title","Two models"))))
  expect_true(jamovi_reporting_update(analysis,list(plotScale="logit")))
  expect_false(any(vapply(options$plotLabels,function(row)row$label=="Chance",logical(1))))
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=label_test_edit(options$plotLabels,"y","Log odds custom"))))
  capture = new.env(parent=emptyenv());original = flexplot:::jamovi_plot_apply_labels
  local_mocked_bindings(jamovi_plot_apply_labels=function(plot,entries,context,layout) {
    result = original(plot,entries,context,layout);capture[[context$output]]=result;result
  },.package="flexplot")
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_identical(capture$model$labels$y,"Log odds custom")
  expect_true("Log odds custom" %in% label_test_grob_text(ggplot2::ggplotGrob(capture$model)))
  expect_true(any(vapply(capture$model$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
  expect_equal(analysis$results$coefficients$asDF,estimates)
  expect_equal(analysis$results$comparison$asDF,comparison)
  expect_true(jamovi_reporting_update(analysis,list(plotScale="probability")))
  expect_true(any(vapply(options$plotLabels,function(row)row$label=="Chance",logical(1))))
  expect_true(any(vapply(options$comparisonPlotLabels,function(row)row$label=="Two models",logical(1))))
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE,out="y")))
  expect_length(options$plotLabels,0L)
  expect_true(analysis$results$explanation$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="y_bin",compare=TRUE)))
  expect_true(any(vapply(options$plotLabels,function(row)row$label=="Chance",logical(1))))
  expect_true(jamovi_reporting_update(analysis,list(plotPanels=NULL)))
  expect_false("facetLevel" %in% vapply(options$plotLabels,`[[`,character(1),"role"))
  expect_true(jamovi_reporting_update(analysis,list(plotPanels="a")))
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=label_test_edit(options$plotLabels,"y",""))))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_false(identical(capture$model$labels$y,"Chance"))
})

test_that("GLM family catalogs and effective fallback use actual display meaning", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(825)
  data = small
  data$positive = exp(.02*data$x+rnorm(nrow(data),sd=.2))
  data$count = rpois(nrow(data),exp(.02*data$x+1))
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    out = if (family=="binomial") "y_bin" else if (family %in% c("poisson","negativebinomial")) "count" else "positive"
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=out,preds=c("x","z","a"),family=family,
      modelTerms=list("x","z","a",c("x","a")),quadratic="x",plotX="x",plotPanels="z"),data)
    expect_true(analysis$run())
    rows = analysis$options$plotLabels
    expect_true(all(c("title","subtitle","x","y","facetLevel") %in% vapply(rows,`[[`,character(1),"role")))
    before = analysis$results$coefficients$asDF
    expect_true(jamovi_reporting_update(analysis,list(plotLabels=label_test_edit(rows,"subtitle","Caption"))))
    expect_equal(analysis$results$coefficients$asDF,before)
  }
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),family="binomial",
    plotX="x",plotStyle="raw",plotScale="logit"),data)
  expect_true(analysis$run())
  keys = vapply(analysis$options$plotLabels,`[[`,character(1),"key")
  expect_true(jamovi_reporting_update(analysis,list(plotScale="probability")))
  expect_identical(vapply(analysis$options$plotLabels,`[[`,character(1),"key"),keys)
})
