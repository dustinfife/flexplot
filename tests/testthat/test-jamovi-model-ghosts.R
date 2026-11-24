model_ghost_signature = function(layers) unname(lapply(layers,function(layer) list(data=layer$data,mapping=layer$mapping,aes=layer$aes_params,geom=class(layer$geom)[1])))
model_ghost_layers = function(plot) vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))
model_ghost_fixture = function() {
  data = expand.grid(x=seq(-1,1,length.out=14),a=factor(c("last","first","middle"),levels=c("last","first","middle")),
    b=factor(c("B","A")),id=factor(1:8))
  data$z = rnorm(nrow(data))
  data$y = .4+data$x+.3*data$x^2+.6*(data$a=="middle")*data$x+rnorm(nrow(data),sd=.2)
  data
}
model_ghost_expect_copy = function(plot, baseline, formula) {
  ghosts = model_ghost_layers(plot)
  expect_equal(model_ghost_signature(plot$layers[!ghosts]),model_ghost_signature(baseline$layers),tolerance=0)
  expect_equal(sum(ghosts),1L)
  ghost = plot$layers[[which(ghosts)]]
  expect_equal(ghost$aes_params[c("colour","linewidth","alpha","linetype")],
    list(colour="#707070",linewidth=1.2,alpha=.85,linetype="dashed"))
  expect_equal(unname(which(ghosts)),length(plot$layers))
  roles = flexplot:::flexplot_axis_given(formula)
  reference = suppressMessages(flexplot:::create_ghost_reference(data=plot$data,given=roles$given,axis=roles$axis))
  curve = Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),baseline$layers)[[1]]$data
  source = flexplot:::flexplot_ghost_reference_data(curve,roles$given,reference,roles$axis)
  expected = flexplot:::flexplot_ghost_prediction_data(source,roles$axis,roles$given,plot$data)
  expect_equal(ghost$data,expected,tolerance=0)
  given = flexplot:::flexplot_magnet_display_given(plot$data,roles$given)
  is_source = rep(TRUE,nrow(ghost$data))
  for (name in given) is_source = is_source & as.character(ghost$data[[name]])==as.character(source[[name]][1])
  expect_false(any(is_source))
  expect_no_warning(ggplot2::ggplotGrob(plot))
}

test_that("LM reference ghosts copy existing interaction/quadratic curves with automatic x and unchanged intervals", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(902)
  data = model_ghost_fixture();model = lm(y~x*a+b+z+I(x^2),data)
  for (x in list(NULL,"x")) for (panels in list("a",c("a","b"),c("a","z"))) {
    options = flexplot:::glmbasicOptions$new(plotX=x,plotPanels=panels)
    baseline = flexplot:::jamovi_lm_display(model,data,options)
    option = options$option("ghostLines");option$value = TRUE
    plot = suppressMessages(flexplot:::jamovi_lm_display(model,data,options))
    model_ghost_expect_copy(plot,baseline,flexplot:::jamovi_model_layout(list(model),data,x,panels))
    expect_null(attr(plot,"jamovi_ghost_note"))
    option = options$option("ghostLines");option$value = FALSE
    expect_equal(model_ghost_signature(flexplot:::jamovi_lm_display(model,data,options)$layers),model_ghost_signature(baseline$layers),tolerance=0)
  }
  for (choice in list(list(plotX="a",plotPanels="b"),list(plotX="x",plotPanels="a",plotGroup="b"),list(plotX="x"))) {
    options = do.call(flexplot:::glmbasicOptions$new,c(choice,list(ghostLines=TRUE)))
    plot = suppressMessages(flexplot:::jamovi_lm_display(model,data,options))
    expect_match(attr(plot,"jamovi_ghost_note"),"Ghost lines unavailable",fixed=TRUE)
    expect_false(any(model_ghost_layers(plot)))
  }
})

test_that("Mixed population and cluster ghosts reuse cached streams without sampling or changing CAS", {
  skip_if_not_installed("jmvcore");skip_if_not_installed("lme4")
  withr::local_seed(903);data = model_ghost_fixture()
  for (type in c("population","cluster")) {
    panels = if(type=="cluster") c("a","id") else c("a","b")
    options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","b"),cluster="id",
      modelTerms=list("x","a","b",c("x","a")),quadratic="x",plotPanels=panels,predictionType=type)
    state = flexplot:::jamovi_mixed_fit(options,data)
    baseline = flexplot:::jamovi_mixed_display(state,options)
    state$displayPlot = baseline;state$displayLayout=list(x=NULL,panels=panels,group=NULL,type=type,clusters=3)
    seed = .Random.seed;fit = lme4::fixef(state$model)
    option = options$option("ghostLines");option$value = TRUE
    plot = suppressMessages(flexplot:::jamovi_mixed_display(state,options))
    expect_identical(.Random.seed,seed)
    expect_identical(lme4::fixef(state$model),fit)
    model_ghost_expect_copy(plot,baseline,flexplot:::jamovi_model_layout(list(state$model),data,"x",panels))
    expect_equal(model_ghost_signature(state$displayPlot$layers),model_ghost_signature(baseline$layers),tolerance=0)
    option = options$option("predictionType");option$value="cas"
    plot = flexplot:::jamovi_mixed_display(state,options)
    expect_match(attr(plot,"jamovi_ghost_note"),"CAS uses adjusted observations",fixed=TRUE)
    expect_false(any(model_ghost_layers(plot)))
  }
})

test_that("shared validation retains ordinary plots for missing references and unavailable prepared panels", {
  skip_if_not_installed("jmvcore");withr::local_seed(904)
  data=model_ghost_fixture();model=lm(y~x*a+b,data)
  plot=flexplot:::jamovi_lm_display(model,data,flexplot:::glmbasicOptions$new(plotPanels=c("a","b")))
  missing=flexplot:::jamovi_model_reference_ghosts(plot,y~x|a+b,TRUE,list(a="absent"))
  expect_match(attr(missing,"jamovi_ghost_note"),"No observations match",fixed=TRUE)
  expect_equal(model_ghost_signature(missing$layers),model_ghost_signature(plot$layers),tolerance=0)
  one=plot;one$data=one$data[one$data$a=="first" & one$data$b=="A",]
  expect_match(flexplot:::jamovi_model_ghost_reason(one,y~x|a+b),"two prepared panels",fixed=TRUE)
})
