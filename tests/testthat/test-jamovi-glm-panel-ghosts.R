glm_panel_ghost_fixture = function(family) {
  data = expand.grid(x=seq(-1,1,length.out=30),a=factor(c("last","first","middle"),levels=c("last","first","middle")),
    b=factor(c("B","A"),levels=c("B","A")),replicate=1:2)
  data$z = rnorm(nrow(data))
  eta = .4+.3*data$x+.15*data$x^2+.1*data$z+.2*(as.integer(data$a)-2)+.15*data$x*(data$a=="middle")
  data$y = switch(family,gaussian=eta+rnorm(nrow(data),sd=.4),binomial=rbinom(nrow(data),1,plogis(eta)),
    poisson=rpois(nrow(data),exp(eta)),gammaLog=rgamma(nrow(data),shape=10,scale=exp(eta)/10),
    Gamma=rgamma(nrow(data),shape=10,scale=1/(2+eta)/10),negativebinomial=rnbinom(nrow(data),size=3,mu=exp(eta)))
  data
}

test_that("actual GLM callbacks copy reference predictions independently of comparison", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(751)
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  capture = new.env(parent=emptyenv())
  original = flexplot:::jamovi_glm_generalized_plot
  local_mocked_bindings(jamovi_glm_generalized_plot=function(...) {capture$plot=original(...);capture$plot},.package="flexplot")
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    data = glm_panel_ghost_fixture(family)
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","b","z"),
      family=family,plotX="x",plotPanels="a",modelTerms=list("x","a","b","z",c("x","a")),quadratic="x"),data)
    expect_false(analysis$options$ghostLines)
    expect_true(analysis$run())
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    baseline = capture$plot
    values = analysis$results$coefficients$asDF
    fit = analysis$results$plot$state$model
    before = lapply(Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),baseline$layers),function(layer)layer$data)
    expect_true(jamovi_reporting_update(analysis,list(ghostLines=TRUE)))
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    plot = capture$plot
    expect_null(attr(plot,"jamovi_ghost_note"))
    ghosts = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),plot$layers)
    expect_length(ghosts,1L)
    curves = Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
    expect_equal(lapply(curves,function(layer)layer$data),before)
    source = curves[[1]]$data[curves[[1]]$data$a=="first",]
    ghost = ghosts[[1]]$data;axes=attr(ghost,"ghost_axes")
    expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$x)],tolerance=1e-12)
    expect_false(any(as.character(ghost$a)=="first"))
    expected = vapply(source$x,function(x) {
      profile=data;profile$x=x;profile$a=factor("first",levels=levels(data$a))
      mean(predict(fit,newdata=profile,type="response"))
    },numeric(1))
    expect_equal(source$prediction,expected,tolerance=1e-10)
    expect_equal(analysis$results$coefficients$asDF,values)
    expect_false(analysis$options$compare)
    expect_true(jamovi_reporting_update(analysis,list(ghostLines=FALSE)))
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_false(any(vapply(capture$plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
  }
})

test_that("binomial panel ghosts preserve transformed marginal probabilities and intervals", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(752)
  data = glm_panel_ghost_fixture("binomial")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","b","z"),family="binomial",
    modelTerms=list("x","a","b","z",c("x","a")),quadratic="x",ghostLines=TRUE),data)
  expect_true(analysis$run())
  state = analysis$results$plot$state
  for (panels in list("a",c("a","b"),"z",c("z","b"))) {
    probability = flexplot:::jamovi_glm_generalized_plot(state,x="x",panels=panels,ghostLines=TRUE)
    logit = flexplot:::jamovi_glm_generalized_plot(state,x="x",panels=panels,ghostLines=TRUE,scale="logit")
    getghost = function(plot)Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),plot$layers)[[1]]$data
    p = getghost(probability);l=getghost(logit);axes=attr(p,"ghost_axes")
    expect_equal(l[[axes[2]]],qlogis(p[[axes[2]]]),tolerance=1e-12)
    ordinary = function(plot)Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    expect_equal(ordinary(logit)$prediction,qlogis(ordinary(probability)$prediction),tolerance=1e-12)
    expect_equal(ordinary(logit)$ymin,qlogis(ordinary(probability)$ymin),tolerance=1e-12)
    expect_equal(ordinary(logit)$ymax,qlogis(ordinary(probability)$ymax),tolerance=1e-12)
    expect_no_warning(ggplot2::ggplotGrob(logit))
  }
  override = flexplot:::jamovi_glm_generalized_plot(state,x="x",panels="a",ghostLines=TRUE,ghost.reference=list(a="last"))
  ghost = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),override$layers)[[1]]$data
  expect_false(any(ghost$a=="last"))
})

test_that("unsupported or unavailable ghosts leave an ordinary plot and a visible explanation", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(753)
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = glm_panel_ghost_fixture("binomial")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","b"),family="binomial",
    modelTerms=list("x","a","b"),plotX="x",ghostLines=TRUE),data)
  expect_true(analysis$run())
  for (values in list(list(plotPanels=NULL),list(plotPanels="b",plotGroup="a"),
      list(plotX="a",plotPanels="b",plotGroup=NULL))) {
    expect_true(jamovi_reporting_update(analysis,values))
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_true(analysis$results$plotExplanation$visible)
    expect_match(analysis$results$plotExplanation$content,"Ghost lines unavailable",fixed=TRUE)
    expect_identical(analysis$results$plot$status,"complete")
    expect_true(analysis$options$ghostLines)
  }
  expect_true(jamovi_reporting_update(analysis,list(plotX="x",plotPanels="a",plotGroup=NULL)))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_false(grepl("Ghost lines unavailable",analysis$results$plotExplanation$content,fixed=TRUE))
  missing = flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",panels="a",ghostLines=TRUE,
    ghost.reference=list(a="absent <reference>"))
  expect_match(attr(missing,"jamovi_ghost_note"),"No observations match the ghost reference",fixed=TRUE)
  expect_false(any(vapply(missing$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
})

test_that("comparison mode does not alter panel ghost semantics or comparison plots", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(754)
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = glm_panel_ghost_fixture("binomial")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","b","z"),family="binomial",
    modelTerms=list("x","a","b","z"),comparisonTerms=list("x","a"),compare=TRUE,plotX="x",plotPanels=c("a","b"),
    comparisonX="x",comparisonPanels="b",ghostLines=TRUE),data)
  expect_true(analysis$run())
  statistics = analysis$results$comparison$asDF
  state = analysis$results$comparisonplot$state
  own = flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",panels=c("a","b"),ghostLines=TRUE)
  expect_true(any(vapply(own$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  comparison = flexplot:::jamovi_glm_generalized_comparison_plot(state)
  expect_false(any(vapply(comparison$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
  expect_true(jamovi_reporting_update(analysis,list(ghostLines=FALSE)))
  expect_equal(analysis$results$comparison$asDF,statistics)
  expect_equal(coef(analysis$results$comparisonplot$state$model1),coef(state$model1))
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE,ghostLines=TRUE)))
  expect_true(analysis$options$ghostLines)
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
})

test_that("ghost validation rejects single prepared panels and nonfinite reference curves", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(755)
  data = glm_panel_ghost_fixture("gaussian")
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family="gaussian")
  state = flexplot:::jamovi_glm_generalized_fit(options,data)
  base = flexplot:::jamovi_glm_generalized_plot(state,x="x",panels="a")
  one = base;one$data = one$data[one$data$a=="first",]
  expect_match(flexplot:::jamovi_glm_ghost_reason(one,y~x|a),"at least two prepared panels",fixed=TRUE)
  original = flexplot:::jamovi_glm_generalized_render
  local_mocked_bindings(jamovi_glm_generalized_render=function(...,ghost.line=NULL) {
    plot = original(...,ghost.line=ghost.line)
    if (!is.null(ghost.line)) for (layer in plot$layers) {
      axes = attr(layer$data,"ghost_axes")
      if (length(axes)) layer$data[[axes[2]]] = Inf
    }
    plot
  },.package="flexplot")
  invalid = flexplot:::jamovi_glm_generalized_plot(state,x="x",panels="a",ghostLines=TRUE)
  expect_match(attr(invalid,"jamovi_ghost_note"),"no finite prediction curve",fixed=TRUE)
  expect_false(any(vapply(invalid$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1))))
})

test_that("adapter logit ghosts respect collision-safe coordinate names", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(756)
  data = glm_panel_ghost_fixture("binomial")
  names(data)[names(data)=="x"] = ".ghost_y"
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c(".ghost_y","a"),family="binomial",
    modelTerms=list(".ghost_y","a",c(".ghost_y","a")),quadratic=".ghost_y",graphic=FALSE),data)
  expect_true(analysis$run())
  plot = flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x=".ghost_y",panels="a",scale="logit",ghostLines=TRUE)
  ghost = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),plot$layers)[[1]]$data
  axes = attr(ghost,"ghost_axes")
  expect_identical(axes[2],".ghost_y.1")
  source = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
  source = source[source$a=="first",]
  expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$.ghost_y)],tolerance=1e-12)
  expect_no_warning(ggplot2::ggplotGrob(plot))
})
