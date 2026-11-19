flexplot_test_draws = function(prepared, count=30) {
  rows = rep(seq_len(nrow(prepared$grid)),count)
  result = prepared$grid[rows,,drop=FALSE]
  result$draw = rep(seq_len(count),each=nrow(prepared$grid))
  x = result[[prepared$roles$x]]
  result$prediction = result$draw/10 + .2*if (is.numeric(x)) x else as.numeric(factor(x))
  rownames(result) = NULL
  result
}

flexplot_test_draw_plot = function(prepared, prediction, ...) {
  flexplot(prepared$formula,prepared$data,prediction=prediction,prediction.draw="draw",
    bins=prepared$binning$bins,breaks=prepared$binning$breaks,labels=prepared$binning$labels,
    jitter=FALSE,...)
}

flexplot_test_prediction_layer = function(plot, geometry="GeomLine") {
  which(vapply(plot$layers,function(layer) inherits(layer$geom,geometry) &&
    is.data.frame(layer$data) && "prediction" %in% names(layer$data),logical(1)))
}

test_that("thirty external draws are exact coherent paths with shared aesthetics and no draw legend", {
  prepared = prepare_flexplot(small,model=y~x)
  prediction = flexplot_test_draws(prepared)
  local_mocked_bindings(flexplot_smooth_layer=function(...) stop("No refitting permitted"))
  p = flexplot_test_draw_plot(prepared,prediction,prediction.alpha=.15)
  expect_false("model" %in% names(prediction))
  expect_false(any(vapply(p$layers,function(layer) inherits(layer$stat,"StatSmooth"),logical(1))))
  layer = flexplot_test_prediction_layer(p)
  expect_length(layer,1)
  expect_identical(p$layers[[layer]]$show.legend,FALSE)
  expect_equal(p$layers[[layer]]$aes_params$alpha,.15)
  built = ggplot2::ggplot_build(p)$data[[layer]]
  expect_length(unique(built$group),30)
  expect_equal(nrow(built),nrow(prediction))
  intercepts = vapply(split(built,built$group),function(curve) {
    expect_true(all(diff(curve$x)>=0))
    expect_equal(curve$y-.2*curve$x,rep(curve$y[1]-.2*curve$x[1],nrow(curve)))
    curve$y[1]-.2*curve$x[1]
  },numeric(1))
  expect_equal(sort(unname(intercepts)),seq_len(30)/10)
  expect_true(all(built$alpha==.15))
  expect_length(unique(built$colour),1)
  expect_length(unique(built$linetype),1)
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("draws stay separate within categorical and binned numeric second-axis groups", {
  for (formula in list(y~x+b,y~x+z)) {
    prepared = prepare_flexplot(small,model=y~x+b+z,formula=formula)
    prediction = flexplot_test_draws(prepared,5)
    prediction$model = "One model"
    p = flexplot_test_draw_plot(prepared,prediction,prediction.alpha=.3)
    layer = flexplot_test_prediction_layer(p)
    built = ggplot2::ggplot_build(p)
    curves = built$data[[layer]]
    groups = prepared$plot_data[[prepared$display_roles$group]]
    expect_length(unique(curves$group),5*length(unique(groups)))
    for (curve in split(curves,curves$group)) {
      expect_length(unique(curve$colour),1)
      expect_length(unique(curve$linetype),1)
      expect_equal(curve$y-.2*curve$x,rep(curve$y[1]-.2*curve$x[1],nrow(curve)))
    }
    # Only the real categorical groups can enter legends; draw IDs never do.
    expect_length(built$plot$scales$get_scales("colour")$get_breaks(),length(unique(groups)))
    expect_false("draw" %in% names(p$mapping))
    expect_identical(p$layers[[layer]]$show.legend,FALSE)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("draw paths retain numeric bins and multiple conditioning panels", {
  for (formula in list(y~x|z,y~x|a+b,y~x+b|z,y~x|z+a,y~x+z|a+b)) {
    prepared = prepare_flexplot(small,model=y~x+a+b+z,formula=formula)
    prediction = flexplot_test_draws(prepared,4)
    p = flexplot_test_draw_plot(prepared,prediction)
    layer = flexplot_test_prediction_layer(p)
    built = ggplot2::ggplot_build(p)
    actual = built$data[[layer]]
    input = p$layers[[layer]]$data
    expect_equal(sort(actual$y),sort(input$prediction))
    prediction_layers = which(vapply(p$layers,function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),logical(1)))
    expect_equal(sum(vapply(built$data[prediction_layers],nrow,integer(1))),nrow(prediction))
    expect_equal(length(unique(built$layout$layout$PANEL)),nrow(unique(prepared$plot_data[prepared$display_roles$panels])))
    # Every native path ID belongs to just one panel; points cannot cross facets.
    panel_counts = tapply(as.character(actual$PANEL),actual$group,function(panel) length(unique(panel)))
    expect_true(all(panel_counts==1L))
    for (variable in prepared$display_roles$panels) expect_true(variable %in% names(input))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("categorical draws share stable group offsets instead of dodging by draw", {
  d = small
  d$a = ordered(d$a,levels=rev(levels(d$a)))
  prepared = prepare_flexplot(d,model=y~a+b,formula=y~a+b)
  prediction = flexplot_test_draws(prepared,5)
  p = flexplot_test_draw_plot(prepared,prediction)
  point = flexplot_test_prediction_layer(p,"GeomPoint")
  line = flexplot_test_prediction_layer(p)
  built = ggplot2::ggplot_build(p)
  expect_identical(built$layout$panel_params[[1]]$x$get_limits(),levels(d$a))
  input = p$layers[[point]]$data
  actual = built$data[[point]]
  slots = match(as.character(input$b),levels(d$b))
  expected_x = match(as.character(input$a),levels(d$a)) + .5*((slots-.5)/nlevels(d$b)-.5)
  expect_equal(as.numeric(actual$x),expected_x)
  expect_equal(actual$y,input$prediction)
  expect_equal(length(unique(built$data[[line]]$group)),5*nlevels(d$b))
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("magnet draw overlays reuse probabilities without fitting observed logistic models", {
  prepared = prepare_flexplot(small,model=y_bin~a+b,formula=y_bin~a+b)
  prediction = flexplot_test_draws(prepared,3)
  prediction$prediction = plogis(prediction$prediction)
  local_mocked_bindings(flexplot_magnet_fit=function(...) stop("No refitting permitted"))
  p = flexplot_test_draw_plot(prepared,prediction)
  point = flexplot_test_prediction_layer(p,"GeomPoint")
  expect_equal(ggplot2::ggplot_build(p)$data[[point]]$y,p$layers[[point]]$data$prediction)
  expect_false(any(vapply(p$layers,function(layer) is.data.frame(layer$data) && "pred_prob" %in% names(layer$data),logical(1))))
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("ordinary single and two-model overlays retain their existing renderer", {
  single = data.frame(x=sort(small$x),prediction=.3+.5*sort(small$x),model="First")
  p = flexplot(y~x,small,prediction=single,suppress_smooth=TRUE,jitter=FALSE)
  index = flexplot_test_prediction_layer(p)
  expect_equal(ggplot2::ggplot_build(p)$data[[index]]$y,single$prediction)
  second = single;second$prediction = second$prediction+.5;second$model="Second"
  p = flexplot(y~x,small,prediction=rbind(single,second),suppress_smooth=TRUE,jitter=FALSE)
  built = ggplot2::ggplot_build(p)$data[[flexplot_test_prediction_layer(p)]]
  expect_length(unique(built$group),2)
  expect_setequal(unique(built$linetype),c("solid","dotdash"))
  expect_equal(sort(built$y),sort(c(single$prediction,second$prediction)))
})

test_that("invalid draw metadata and ambiguous coordinates fail explicitly", {
  prepared = prepare_flexplot(small,model=y~x)
  prediction = flexplot_test_draws(prepared,3)
  expect_error(flexplot_test_draw_plot(prepared,prediction,prediction.alpha=2),"between zero and one")
  invalid = prediction;invalid$draw[1]=NA
  expect_error(flexplot_test_draw_plot(prepared,invalid),"nonmissing")
  invalid = prediction;invalid$prediction[1]=Inf
  expect_error(flexplot_test_draw_plot(prepared,invalid),"finite numeric")
  invalid = prediction;invalid$model=rep(c("A","B"),length.out=nrow(invalid))
  expect_error(flexplot_test_draw_plot(prepared,invalid),"one model")
  expect_error(flexplot_test_draw_plot(prepared,rbind(prediction,prediction[1,])),"one prediction per draw")
  expect_error(flexplot_test_draw_plot(prepared,prediction,ghost.line="gray"),"ghost overlays")
  expect_error(flexplot_test_draw_plot(prepared,prediction,related=TRUE),"paired-difference")
  expect_error(flexplot(y~x,small,prediction=prediction,prediction.draw="x"),"separate draw-identifier")
  expect_error(flexplot(y~log(x),small,prediction=prediction,prediction.draw="draw"),"untransformed visualization")
  expect_error(flexplot(b~a,small,prediction=prediction,prediction.draw="draw"),"association plots")
})


test_that("draw identifiers may use .draw and sampling only changes raw observations", {
  prepared = prepare_flexplot(small,model=y~x+b,formula=y~x+b)
  prediction = flexplot_test_draws(prepared,4)
  names(prediction)[names(prediction)=="draw"] = ".draw"
  full = flexplot(prepared$formula,prepared$data,prediction=prediction,prediction.draw=".draw",jitter=FALSE)
  set.seed(32)
  sampled = flexplot(prepared$formula,prepared$data,prediction=prediction,prediction.draw=".draw",jitter=FALSE,sample=1)
  full_built = ggplot2::ggplot_build(full)
  sampled_built = ggplot2::ggplot_build(sampled)
  expect_equal(sampled_built$data[[flexplot_test_prediction_layer(sampled)]],full_built$data[[flexplot_test_prediction_layer(full)]])
  expect_equal(nrow(sampled_built$data[[1]]),1L)
  expect_equal(nrow(full_built$data[[1]]),nrow(prepared$data))
})


test_that("unknown draw group levels fail before native positioning can lose rows", {
  prepared = prepare_flexplot(small,model=y~a+b,formula=y~a+b)
  prediction = flexplot_test_draws(prepared,3)
  prediction$b = as.character(prediction$b)
  prediction$b[1] = "unobserved group"
  expect_error(flexplot_test_draw_plot(prepared,prediction),"observed display levels")
})

test_that("native draw layers work without utils attached in an isolated R process", {
  # Serialize the real constructor and its real axis/facet helpers, so source
  # tests do not accidentally exercise an older installed flexplot in the child.
  scope = new.env(parent=globalenv())
  for (name in c("flexplot_draw_prediction_layers", "flexplot_object_axis",
                 "flexplot_magnet_display_given")) {
    scope[[name]] = get(name,envir=asNamespace("flexplot"))
    environment(scope[[name]]) = scope
  }
  data = data.frame(x=seq_len(5), y=seq_len(5))
  prediction = data.frame(x=rep(data$x,3),prediction=rep(data$x,3)+rep(1:3,each=5),
                          model="One model",draw=rep(1:3,each=5))
  input = withr::local_tempfile(fileext=".rds")
  output = withr::local_tempfile(fileext=".rds")
  script = withr::local_tempfile(fileext=".R")
  saveRDS(list(renderer=scope$flexplot_draw_prediction_layers,data=data,prediction=prediction),input)
  writeLines(c(
    'options(device=function(...) grDevices::pdf(file=NULL))',
    'if ("package:utils" %in% search()) detach("package:utils",unload=FALSE)',
    'stopifnot(!"package:utils" %in% search())',
    'args = commandArgs(TRUE)',
    'input = readRDS(args[1])',
    'layers = input$renderer(input$prediction,"x",input$data,NULL,"draw",.2)',
    'plot = ggplot2::ggplot(input$data,ggplot2::aes(x=x,y=y)) + layers',
    'coordinates = ggplot2::ggplot_build(plot)$data[[1]]',
    'invisible(ggplot2::ggplotGrob(plot))',
    'stopifnot(!"package:utils" %in% search())',
    'saveRDS(list(search=search(),coordinates=coordinates),args[2])'
  ),script)
  log = system2(file.path(R.home("bin"),"Rscript"),
    shQuote(c("--vanilla",script,input,output)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(file.exists(output))
  if (!file.exists(output)) return(invisible(NULL))
  actual = readRDS(output)
  expect_false("package:utils" %in% actual$search)
  expect_equal(actual$coordinates$x,prediction$x)
  expect_equal(actual$coordinates$y,prediction$prediction)
  expect_length(unique(actual$coordinates$group),3L)
  expect_true(all(actual$coordinates$alpha==.2))
})
