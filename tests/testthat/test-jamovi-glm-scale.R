test_that("binomial display scales transform the same marginal probabilities and intervals", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(741)
  d = data.frame(x=rep(seq(-2,2,length.out=80),4),z=rnorm(320),a=factor(rep(c("low","high"),each=160)))
  d$y = rbinom(320,1,plogis(-.3+.6*d$x+.5*d$z+.7*(d$a=="high")-.3*d$x*d$z+.2*d$x^2))
  d$z[1:9] = NA_real_
  d$y = factor(d$y,levels=c(0,1),labels=c("no","yes"),ordered=TRUE)
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","a"),family="binomial",
    modelTerms=list("x","z","a",c("x","z")),quadratic="x",polynomials=TRUE,graphic=FALSE)
  analysis = jamovi_reporting_host(options,d)
  expect_true(analysis$run())
  state = analysis$results$plot$state
  original = glm(y~x*z+a+I(x^2),data=state$data,family=binomial())
  expect_equal(coef(state$model),coef(original),tolerance=1e-12)
  local_mocked_bindings(glm.fit=function(...)stop("Unexpected model refit"),.package="stats")
  for (se in c(FALSE,TRUE)) for (layout in list(list(panels="a"),list(group="a"))) {
    p = do.call(flexplot:::jamovi_glm_generalized_plot,c(list(state=state,x="x",se=se),layout))
    l = do.call(flexplot:::jamovi_glm_generalized_plot,c(list(state=state,x="x",se=se,scale="logit"),layout))
    curve_index = function(plot) which(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomSmooth") && is.data.frame(layer$data) && "prediction" %in% names(layer$data),logical(1)))
    ip = curve_index(p);il = curve_index(l)
    expect_length(ip,1);expect_length(il,1)
    prob = p$layers[[ip]]$data;logit = l$layers[[il]]$data
    expected = vapply(seq_len(nrow(prob)),function(i) {
      profile = state$data;profile$x = prob$x[i];profile$a = prob$a[i]
      mean(predict(original,profile,type="response"))
    },numeric(1))
    expect_equal(prob$prediction,expected,tolerance=1e-10)
    expect_equal(logit$prediction,qlogis(prob$prediction),tolerance=1e-10)
    expect_equal(logit$ymin,qlogis(prob$ymin),tolerance=1e-10)
    expect_equal(logit$ymax,qlogis(prob$ymax),tolerance=1e-10)
    # Conditional expectation and the link of the standardized mean differ.
    links = vapply(seq_len(nrow(prob)),function(i) {
      profile = state$data;profile$x = prob$x[i];profile$a = prob$a[i]
      mean(predict(original,profile,type="link"))
    },numeric(1))
    expect_gt(max(abs(logit$prediction-links)),.005)
    b = ggplot2::ggplot_build(l)
    expect_equal(sort(b$data[[il]]$y),sort(logit$prediction),tolerance=1e-10)
    if (se) {
      expect_equal(sort(b$data[[il]]$ymin),sort(logit$ymin),tolerance=1e-10)
      expect_equal(sort(b$data[[il]]$ymax),sort(logit$ymax),tolerance=1e-10)
    }
    observed = l$layers[[length(l$layers)-1]]$data
    bins = cut(state$data$x,quantile(state$data$x,seq(0,1,length.out=11)),include.lowest=TRUE)
    key = interaction(bins,state$data$a,drop=TRUE)
    proportions = tapply(as.numeric(state$data$y)-1,key,mean)
    count = table(key)
    index = as.character(interaction(observed$bin,observed$a,drop=TRUE))
    corrected = pmax(.5/count[index],pmin((count[index]-.5)/count[index],proportions[index]))
    expect_equal(observed$proportion,as.numeric(qlogis(corrected)),tolerance=1e-10)
    expect_equal(sum(observed$count),nrow(state$data))
    expect_equal(l$scales$get_scales("y")$name,"Logit (log odds)")
    expect_no_warning(ggplot2::ggplotGrob(l))
  }
  expect_equal(coef(state$model),coef(original),tolerance=1e-12)
})

test_that("GLM comparison curves and endpoints use the selected display scale without refitting", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(742)
  d = data.frame(x=seq(-2,2,length.out=300),z=rnorm(300))
  d$y = rbinom(300,1,plogis(.3+.6*d$x+.8*d$z+.3*d$x*d$z+.2*d$x^2))
  d$z[1:15] = NA_real_
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),family="binomial",
    modelTerms=list("x","z",c("x","z")),quadratic="x",polynomials=TRUE,
    compare=TRUE,comparisonTerms=list("x","z"),comparisonX="x",graphic=FALSE),d)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  before = state
  independent = list(glm(y~x*z+I(x^2),state$data,family=binomial()),glm(y~x+z,state$data,family=binomial()))
  expect_equal(coef(state$model1),coef(independent[[1]]),tolerance=1e-12)
  expect_equal(coef(state$model2),coef(independent[[2]]),tolerance=1e-12)
  local_mocked_bindings(glm.fit=function(...)stop("Unexpected refit"),.package="stats")
  for (se in c(FALSE,TRUE)) {
    p = flexplot:::jamovi_glm_generalized_comparison_plot(state,se=se)
    l = flexplot:::jamovi_glm_generalized_comparison_plot(state,se=se,scale="logit")
    for (i in seq_along(p$layers)) {
      if (!is.data.frame(p$layers[[i]]$data) || !"prediction" %in% names(p$layers[[i]]$data)) next
      a = p$layers[[i]]$data;b = l$layers[[i]]$data
      displayed = if (inherits(p$layers[[i]]$geom,c("GeomRibbon","GeomErrorbar"))) c("prediction","prediction.lwr","prediction.upr") else "prediction"
      for (name in intersect(displayed,names(a)))
        expect_equal(b[[name]],qlogis(a[[name]]),tolerance=1e-10)
      for (label in unique(as.character(a$model))) {
        rows = a[as.character(a$model)==label,,drop=FALSE]
        model = independent[[if(startsWith(label,"model1"))1 else 2]]
        expected = vapply(rows$x,function(x){profile=state$data;profile$x=x;mean(predict(model,profile,type="response"))},numeric(1))
        expect_equal(rows$prediction,expected,tolerance=1e-10)
      }
    }
    expect_no_warning(ggplot2::ggplotGrob(l))
    expect_equal(l$scales$get_scales("colour")$name,"Model (logit scale)")
  }
  expect_identical(state,before)
})

test_that("logit displays reject unsupported layouts and preserve extreme limits without clipping", {
  skip_if_not_installed("jmvcore")
  d = small_logistic;d$y = d$y_numb;d$a = factor(rep(c("one","two"),length.out=nrow(d)))
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family="binomial",graphic=FALSE),d)
  expect_true(analysis$run());state = analysis$results$plot$state
  expect_error(flexplot:::jamovi_glm_generalized_plot(state,x="a",scale="logit"),"continuous numeric")
  expect_error(flexplot:::jamovi_glm_generalized_plot(state,x="x",style="raw",scale="logit"),"Raw binary")
  p = ggplot2::ggplot()+ggplot2::geom_ribbon(data=data.frame(x=1:3,prediction=c(0,.5,1),
    prediction.lwr=c(0,0,.5),prediction.upr=c(.5,1,1)),ggplot2::aes(x=x,y=prediction,ymin=prediction.lwr,ymax=prediction.upr))
  l = flexplot:::jamovi_glm_logit_display(p,"y")
  expect_equal(l$layers[[1]]$data$prediction,c(-Inf,0,Inf))
  expect_equal(l$layers[[1]]$data$prediction.lwr,c(-Inf,-Inf,0))
  expect_equal(l$layers[[1]]$data$prediction.upr,c(0,Inf,Inf))
  bad = ggplot2::ggplot()+ggplot2::geom_line(data=data.frame(x=1:2,prediction=c(.5,NA_real_)),ggplot2::aes(x=x,y=prediction))
  expect_error(flexplot:::jamovi_glm_logit_display(bad,"y"),"invalid probabilities")
  p$layers[[1]]$data = p$layers[[1]]$data[0,,drop=FALSE]
  expect_error(flexplot:::jamovi_glm_logit_display(p,"y"),"No finite logit")
})


test_that("logit overlays retain literal predictor names and numeric panel bins", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(745)
  d = data.frame(x=seq(-2,2,length.out=240),z=rnorm(240))
  d$y = rbinom(240,1,plogis(.2+.5*d$x+.4*d$z))
  names(d)[1] = "has space"
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("has space","z"),family="binomial",graphic=FALSE),d)
  expect_true(analysis$run())
  state = analysis$results$plot$state
  p = flexplot:::jamovi_glm_generalized_plot(state,x="has space",panels="z",scale="logit")
  expect_no_warning(ggplot2::ggplotGrob(p))
  curve = Filter(function(layer)inherits(layer$geom,"GeomSmooth") && is.data.frame(layer$data),p$layers)[[1]]$data
  expected = qlogis(predict(state$model,newdata=curve,type="response"))
  expect_equal(curve$prediction,unname(expected),tolerance=1e-10)
  expect_gt(nrow(ggplot2::ggplot_build(p)$layout$layout),1)
})

test_that("extreme fitted probabilities keep their exact fitted-model logits", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(748)
  d = data.frame(x=seq(-30,30,length.out=1000))
  d$y = rbinom(1000,1,plogis(.3+2*d$x))
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds="x",family="binomial")
  state = flexplot:::jamovi_glm_generalized_fit(options,d)
  for (se in c(FALSE,TRUE)) {
    p = flexplot:::jamovi_glm_generalized_plot(state,x="x",se=se)
    l = flexplot:::jamovi_glm_generalized_plot(state,x="x",se=se,scale="logit")
    a = Filter(function(layer)inherits(layer$geom,"GeomSmooth"),p$layers)[[1]]$data
    b = Filter(function(layer)inherits(layer$geom,"GeomSmooth"),l$layers)[[1]]$data
    expect_lt(min(a$prediction),1e-12);expect_gt(max(a$prediction),1-1e-12)
    expect_equal(b$prediction,qlogis(a$prediction),tolerance=1e-10)
    expect_equal(b$ymin,qlogis(a$ymin),tolerance=1e-10)
    expect_equal(b$ymax,qlogis(a$ymax),tolerance=1e-10)
    observed = l$layers[[length(l$layers)-1]]$data
    expect_true(all(is.finite(observed$proportion)))
    expect_no_warning(ggplot2::ggplotGrob(l))
  }
})
