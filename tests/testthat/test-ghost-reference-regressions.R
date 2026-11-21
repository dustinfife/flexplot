test_that("automatic ghost references retain legacy valid choices, including even counts", {
  for (n in c(1,2,3,4,5,6,9)) {
    labels = paste0("level",rev(seq_len(n)))
    data = expand.grid(x=seq(-2,2,length.out=20),panel=factor(labels,levels=labels))
    data$y = data$x + 10*as.integer(data$panel)
    expected_index = c(`1`=1L,`2`=1L,`3`=2L,`4`=2L,`5`=2L,`6`=3L,`9`=4L)[as.character(n)]
    reference = suppressMessages(flexplot:::create_ghost_reference(data=data,given="panel",axis="x"))
    expect_identical(reference$panel,labels[expected_index],info=paste(n,"levels"))
    supplied = data.frame(x=data$x,panel=data$panel,prediction=data$y,model="supplied")
    plot = suppressMessages(flexplot(y~x|panel,data,prediction=supplied,suppress_smooth=TRUE,ghost.line="gray",jitter=FALSE))
    index = which(vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1)))
    expect_length(index,1L)
    ghost = plot$layers[[index]]$data
    axes = attr(ghost,"ghost_axes")
    expect_equal(nrow(ghost),20L*(n-1L))
    if (n>1L) {
      expect_false(any(as.character(ghost$panel)==reference$panel))
      expect_equal(ghost[[axes[2]]],ghost[[axes[1]]]+10*unname(expected_index))
    }
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("partial references handle singletons, low-cardinality numbers and prepared bin order", {
  data = expand.grid(x=1:12,a=factor("only"),b=factor(c("z","a","m"),levels=c("z","a","m")))
  reference = suppressMessages(flexplot:::create_ghost_reference(data=data,given=c("a","b"),axis="x",ghost.reference=list(b="m")))
  expect_identical(reference,list(b="m",a="only"))
  data$a = rep(c(20,10),length.out=nrow(data))
  reference = suppressMessages(flexplot:::create_ghost_reference(data=data,given=c("a","b"),axis="x",ghost.reference=list(b="m")))
  expect_identical(reference$a,"10")
  data$a_binned = factor(rep(c("last","first","center"),length.out=nrow(data)),levels=c("last","first","center"))
  reference = suppressMessages(flexplot:::create_ghost_reference(data=data,given=c("a","b"),axis="x",ghost.reference=list(b="m")))
  expect_identical(reference$a,"first")
  reference = suppressMessages(flexplot:::create_ghost_reference(data=data,given=c("a","b"),axis="x"))
  expect_identical(reference,list(a="first",b="a"))
  data$c = factor(rep(c("A","B","C","D"),length.out=nrow(data)),levels=c("unused","A","B","C","D"))
  partial = suppressMessages(flexplot:::create_ghost_reference(data=data,given=c("c","b"),axis="x",ghost.reference=list(b="m")))
  expect_identical(partial$c,"A") # Existing partial-reference factor-level rule.
  selected = flexplot:::flexplot_ghost_reference_data(data,c("a","b"),reference)
  expect_true(all(as.character(selected$a_binned)=="first"))
  expect_true(all(as.character(selected$b)=="a"))
})

test_that("missing reference combinations error without choosing another source", {
  data = expand.grid(x=seq(-2,2,length.out=20),a=factor(c("A","B","C")),b=factor(c("U","V","W")))
  data = data[!(data$a=="B" & data$b=="V"),]
  data$y = data$x + as.integer(data$a) + as.integer(data$b)
  predictions = data.frame(x=data$x,a=data$a,b=data$b,prediction=data$y,model="supplied")
  expect_error(suppressMessages(flexplot(y~x|a+b,data,prediction=predictions,suppress_smooth=TRUE,ghost.line="gray")),
    "No observations match the ghost reference",fixed=TRUE)
  plot = flexplot(y~x|a+b,data,prediction=predictions,suppress_smooth=TRUE,ghost.line="gray",ghost.reference=list(a="A",b="V"))
  expect_no_warning(ggplot2::ggplotGrob(plot))
  expect_error(flexplot(y~x|a+b,data,prediction=predictions,suppress_smooth=TRUE,ghost.line="gray",ghost.reference=list(a="missing",b="V")),
    "No observations match the ghost reference",fixed=TRUE)
})

# One fitted model, with panel interactions, curvature and an unshown covariate.
ghost_logit_fixture = function() {
  data = expand.grid(x=seq(-2,2,length.out=20),
    a=factor(c("delta","alpha","echo","bravo","charlie"),levels=c("delta","alpha","echo","bravo","charlie")),
    b=factor(c("right","left","middle"),levels=c("right","left","middle")),replicate=1:2)
  data$z = rnorm(nrow(data))
  eta = -.2+.6*data$x-.25*data$x^2+.7*data$z+
    .2*(as.integer(data$a)-3)*(1+data$x)+.2*(as.integer(data$b)-2)
  data$y = rbinom(nrow(data),1,plogis(eta))
  data
}

test_that("binomial ghosts copy original marginal curves on probability and logit scales", {
  withr::local_seed(741)
  data = ghost_logit_fixture()
  model = glm(y~x*a+b+I(x^2)+z,data=data,family=binomial())
  coefficients = coef(model)
  local_mocked_bindings(glm.fit=function(...)stop("Ghost lines must not refit a model"),.package="stats")
  for (reference in list(NULL,list(a="charlie",b="right"))) {
    chosen = if (is.null(reference)) list(a="alpha",b="left") else reference
    ordinary = list()
    for (scale in c("probability","logit")) {
      base = flexplot:::visualize_binomial_model(model,y~x|a+b,data,10,"dot",num_points=13,se=TRUE,scale=scale)
      plot = suppressMessages(flexplot:::visualize_binomial_model(model,y~x|a+b,data,10,"dot",num_points=13,
        se=TRUE,scale=scale,ghost.line="gray",ghost.reference=reference))
      main = which(vapply(plot$layers,function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),logical(1)))
      baseline = which(vapply(base$layers,function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),logical(1)))
      expect_length(main,1L)
      expect_equal(plot$layers[[main]]$data,base$layers[[baseline]]$data)
      ordinary[[scale]] = plot$layers[[main]]$data
      reference_rows = ordinary[[scale]][ordinary[[scale]]$a==chosen$a & ordinary[[scale]]$b==chosen$b,]
      expected = lapply(reference_rows$x,function(value) {
        profile = data;profile$x=value
        profile$a=factor(chosen$a,levels=levels(data$a));profile$b=factor(chosen$b,levels=levels(data$b))
        probability = predict(model,newdata=profile,type="response")
        matrix = model.matrix(delete.response(terms(model)),profile,contrasts.arg=model$contrasts)
        gradient = colMeans(matrix*as.numeric(probability*(1-probability)))
        mean = mean(probability)
        se = sqrt(drop(t(gradient)%*%vcov(model)%*%gradient))
        critical = qnorm(.975)*se/(mean*(1-mean))
        c(prediction=mean,lower=plogis(qlogis(mean)-critical),upper=plogis(qlogis(mean)+critical))
      })
      expected = do.call(rbind,expected)
      if (scale=="logit") expected = qlogis(expected)
      expect_equal(reference_rows$prediction,unname(expected[,"prediction"]),tolerance=1e-10)
      expect_equal(reference_rows$ymin,unname(expected[,"lower"]),tolerance=1e-10)
      expect_equal(reference_rows$ymax,unname(expected[,"upper"]),tolerance=1e-10)
      ghost_index = which(vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1)))
      expect_length(ghost_index,1L)
      ghost = plot$layers[[ghost_index]]$data
      axes = attr(ghost,"ghost_axes")
      matched = match(ghost[[axes[1]]],reference_rows$x)
      expect_false(anyNA(matched))
      expect_equal(ghost[[axes[2]]],reference_rows$prediction[matched],tolerance=1e-12)
      expect_false(any(ghost$a==chosen$a & ghost$b==chosen$b))
      expect_equal(nrow(ghost),13L*14L)
      built = ggplot2::ggplot_build(plot)
      layout = built$layout$layout
      source = layout$PANEL[layout$a==chosen$a & layout$b==chosen$b]
      expect_false(any(built$data[[ghost_index]]$PANEL %in% source))
      expect_setequal(unique(built$data[[ghost_index]]$PANEL),layout$PANEL[!layout$PANEL %in% source])
      expect_equal(built$data[[ghost_index]]$y,ghost[[axes[2]]])
      expect_no_warning(ggplot2::ggplotGrob(plot))
    }
    expect_equal(ordinary$logit$prediction,qlogis(ordinary$probability$prediction),tolerance=1e-12)
    expect_equal(ordinary$logit$ymin,qlogis(ordinary$probability$ymin),tolerance=1e-12)
    expect_equal(ordinary$logit$ymax,qlogis(ordinary$probability$ymax),tolerance=1e-12)
  }
  expect_identical(coef(model),coefficients)
})

test_that("numeric conditioning ghosts retain prepared bin ordering and source predictions", {
  withr::local_seed(742)
  data = ghost_logit_fixture()
  model = glm(y~x*z+I(x^2)+a+b,data=data,family=binomial())
  labels = c("zeta","alpha","theta","beta","omega")
  for (reference in list(NULL,list(z=median(data$z)))) for (scale in c("probability","logit")) {
    plot = suppressMessages(flexplot:::visualize_binomial_model(model,y~x|z,data,10,"dot",num_points=13,
      bins=5,labels=list(z=labels),scale=scale,ghost.line="gray",ghost.reference=reference))
    expect_identical(levels(plot$data$z_binned),labels)
    edges = quantile(data$z,seq(0,1,length.out=6))
    chosen = if (is.null(reference)) labels[2] else labels[cut(median(data$z),edges,include.lowest=TRUE,labels=FALSE)]
    index = which(vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1)))
    ghost = plot$layers[[index]]$data
    axes = attr(ghost,"ghost_axes")
    main = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    source = main[as.character(main$z_binned)==chosen,]
    expected = vapply(seq_len(nrow(source)),function(i) {
      profile = data;profile$x=source$x[i];profile$z=source$z[i]
      mean(predict(model,newdata=profile,type="response"))
    },numeric(1))
    if (scale=="logit") expected = qlogis(expected)
    expect_equal(source$prediction,expected,tolerance=1e-10)
    expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$x)],tolerance=1e-12)
    expect_false(any(as.character(ghost$z_binned)==chosen))
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("two numeric conditioning variables keep their own prepared references", {
  withr::local_seed(743)
  data = ghost_logit_fixture()
  data$w = rnorm(nrow(data))+.3*data$z
  model = glm(y~x*z+I(x^2)+w+a+b,data=data,family=binomial())
  local_mocked_bindings(glm.fit=function(...)stop("Ghost lines must not refit a model"),.package="stats")
  labels = list(z=c("upper","central","lower"),w=c("right","center","left"))
  for (scale in c("probability","logit")) {
    plot = suppressMessages(flexplot:::visualize_binomial_model(model,y~x|z+w,data,10,"dot",num_points=13,
      bins=3,labels=labels,scale=scale,ghost.line="gray"))
    expect_identical(levels(plot$data$z_binned),labels$z)
    baseline = flexplot:::visualize_binomial_model(model,y~x|z+w,data,10,"dot",num_points=13,bins=3,labels=labels,scale=scale)
    expect_identical(levels(plot$data$w_binned),levels(baseline$data$w_binned))
    index = which(vapply(plot$layers,function(layer)!is.null(attr(layer$data,"ghost_axes")),logical(1)))
    ghost = plot$layers[[index]]$data
    axes = attr(ghost,"ghost_axes")
    main = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    source = main[main$z_binned=="central" & main$w_binned=="center",]
    expect_equal(nrow(source),13L)
    expected = vapply(seq_len(nrow(source)),function(i) {
      profile = data;profile$x=source$x[i];profile$z=source$z[i];profile$w=source$w[i]
      mean(predict(model,newdata=profile,type="response"))
    },numeric(1))
    if (scale=="logit") expected = qlogis(expected)
    expect_equal(source$prediction,expected,tolerance=1e-10)
    expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$x)],tolerance=1e-12)
    expect_false(any(ghost$z_binned=="central" & ghost$w_binned=="center"))
    expect_equal(nrow(ghost),13L*8L)
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("logit ghost transformation follows collision-safe coordinate metadata", {
  withr::local_seed(744)
  data = ghost_logit_fixture()
  names(data)[names(data)=="x"] = ".ghost_y"
  model = glm(y~.ghost_y*a+I(.ghost_y^2)+z,data=data,family=binomial())
  plot = flexplot:::visualize_binomial_model(model,y~.ghost_y|a,data,10,"dot",num_points=13,
    scale="logit",ghost.line="gray",ghost.reference=list(a="alpha"))
  ghost = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),plot$layers)[[1]]$data
  axes = attr(ghost,"ghost_axes")
  expect_identical(axes[2],".ghost_y.1")
  main = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
  source = main[main$a=="alpha",]
  expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$.ghost_y)],tolerance=1e-12)
  expect_no_warning(ggplot2::ggplotGrob(plot))
})

test_that("public binomial visualization transforms the same reference ghost", {
  withr::local_seed(745)
  data = ghost_logit_fixture()
  model = glm(y~x*a,data=data,family=binomial())
  plot = suppressMessages(visualize(model,plot="model",scale="logit",ghost.line="gray",num_points=13,se=FALSE))
  ghosts = Filter(function(layer)!is.null(attr(layer$data,"ghost_axes")),plot$layers)
  expect_length(ghosts,1L)
  main = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && "prediction" %in% names(layer$data),plot$layers)
  expect_length(main,1L)
  source = main[[1]]$data[main[[1]]$data$a=="alpha",]
  ghost = ghosts[[1]]$data
  axes = attr(ghost,"ghost_axes")
  expect_equal(ghost[[axes[2]]],source$prediction[match(ghost[[axes[1]]],source$x)],tolerance=1e-12)
  expect_no_warning(ggplot2::ggplotGrob(plot))
})
