test_that("Linear Model labels preserve the analysis identifiers and public API", {
  skip_if_not_installed("jmvcore")
  options = flexplot:::glmbasicOptions$new()
  expect_identical(flexplot:::glmbasicResults$new(options)$title, "Linear Model")
  expect_identical(flexplot:::glmgeneralizedResults$new(flexplot:::glmgeneralizedOptions$new())$title,
                   "Generalized Linear Model")
  expect_true(is.function(glmbasic))
  expect_s3_class(options, "glmbasicOptions")
})

test_that("visualization roles are validated against fitted terms in R", {
  skip_if_not_installed("jmvcore")
  theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(theme))
  for (role in c("plotX", "plotPanels", "plotGroup")) {
    values = list(out="y", preds=c("x", "a", "z"), modelTerms=list("x", "a"))
    values[[role]] = "z" # In the selected variable pool, absent from the fitted terms.
    options = do.call(flexplot:::glmgeneralizedOptions$new, values)
    analysis = flexplot:::glmgeneralizedClass$new(options=options, data=small)
    expect_true(analysis$run())
    expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_match(analysis$results$plot$.__enclos_env__$private$.error, "must be predictors")
    options = do.call(flexplot:::glmbasicOptions$new, values)
    analysis = flexplot:::glmbasicClass$new(options=options, data=small)
    expect_true(analysis$run())
    expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    expect_match(analysis$results$plot$.__enclos_env__$private$.error, "must be predictors")
  }
  model1 = lm(y ~ x+a, small)
  model2 = lm(y ~ z, small)
  expect_identical(all.vars(flexplot:::jamovi_model_layout(list(model1, model2), small, "z", "a")),
                   c("y", "z", "a"))
  expect_error(flexplot:::jamovi_model_layout(list(model1, model2), small, "b"), "must be predictors")
  expect_error(flexplot:::jamovi_model_layout(list(model1), small, "x", "x"), "different model predictors")
  expect_identical(all.vars(flexplot:::jamovi_model_layout(list(lm(y ~ x:a, small)), small, "x", group="a")),
                   c("y", "x", "a"))
})

test_that("Linear Model renders chosen single-model grouping and preserves comparison restrictions", {
  skip_if_not_installed("jmvcore")
  theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(theme))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("x", "a", "b"),
    plotX="x", plotPanels="a", plotGroup="b")
  analysis = flexplot:::glmbasicClass$new(options=options, data=small)
  expect_true(analysis$run())
  expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
  plot = capture$plot
  expect_no_warning({ built = ggplot2::ggplot_build(plot) })
  expect_equal(nrow(built$layout$layout), 2L)
  curves = which(vapply(plot$layers, function(layer) inherits(layer$geom,"GeomLine") &&
    is.data.frame(layer$data) && "prediction" %in% names(layer$data), logical(1)))
  expect_length(curves, 1L)
  expect_equal(length(unique(built$data[[curves]]$group)), 3L)
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("x", "a", "z"), compare=TRUE,
    modelTerms=list("x", "a"), comparisonTerms=list("z"), comparisonX="z", comparisonPanels="a")
  analysis = flexplot:::glmbasicClass$new(options=options, data=small)
  expect_true(analysis$run())
  expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("x", "a", "z"), compare=TRUE,
    modelTerms=list("x", "a"), comparisonTerms=list("z"), comparisonX="x", comparisonGroup="a")
  analysis = flexplot:::glmbasicClass$new(options=options, data=small)
  expect_true(analysis$run())
  expect_false(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_match(analysis$results$comparisonplot$.__enclos_env__$private$.error, "panels")
})

test_that("Gamma and Negative Binomial tables and plotted intervals use the actual fitted model", {
  skip_if_not_installed("jmvcore")
  set.seed(3214)
  data = data.frame(x=rep(seq(-.5,.5,length.out=120), 2), a=factor(rep(c("a","b"), each=120)))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_generalized_plot=jamovi_generalized_recorder(capture))
  for (family in c("Gamma", "negativebinomial")) {
    mean = if (family == "Gamma") 1/(.8+.12*data$x+.06*(data$a=="b")) else exp(.7+.4*data$x+.2*(data$a=="b"))
    data$y = if (family == "Gamma") rgamma(nrow(data),shape=6,scale=mean/6) else rnbinom(nrow(data),mu=mean,size=1.8)
    options = flexplot:::glmgeneralizedOptions$new(out="y", preds=c("x","a"), family=family,
      modelTerms=list("x","a",c("x","a")), plotX="x", plotGroup="a")
    analysis = flexplot:::glmgeneralizedClass$new(options=options, data=data)
    expect_true(analysis$run())
    expected = if (family == "Gamma") glm(y ~ x*a, data=data, family=Gamma(link="inverse")) else MASS::glm.nb(y ~ x*a, data=data)
    state = analysis$results$plot$state
    expect_equal(coef(state$model), coef(expected), tolerance=1e-10)
    table = analysis$results$coefficients$asDF
    coefficients = summary(expected)$coefficients
    critical = if (family == "Gamma") qt(.975,df.residual(expected)) else qnorm(.975)
    expect_equal(table$estimate, unname(coefficients[,1]), tolerance=1e-10)
    expect_equal(table$se, unname(coefficients[,2]), tolerance=1e-10)
    expect_equal(table$p, unname(coefficients[,4]), tolerance=1e-10)
    expect_equal(table$lower, unname(coefficients[,1]-critical*coefficients[,2]), tolerance=1e-10)
    fit = rbind(analysis$results$fit$asDF, analysis$results$likelihood$asDF)
    expect_equal(fit$value[fit$measure=="AIC"], AIC(expected), tolerance=1e-10)
    expect_equal(fit$value[fit$measure=="BIC"], BIC(expected), tolerance=1e-10)
    expect_equal(fit$value[fit$measure=="Log likelihood"], as.numeric(logLik(expected)), tolerance=1e-10)
    if (family == "negativebinomial") {
      expect_s3_class(state$model,"negbin")
      expect_equal(fit$value[fit$measure=="Size (theta)"],expected$theta,tolerance=1e-10)
      expect_equal(fit$value[fit$measure=="SE (theta)"],expected$SE.theta,tolerance=1e-10)
    }
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    plot = capture$plot
    expect_no_warning({ built=ggplot2::ggplot_build(plot) })
    ribbons=which(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomRibbon"),logical(1)))
    expect_length(ribbons,1L)
    source=plot$layers[[ribbons]]$data
    predicted=predict(expected,newdata=source,type="link",se.fit=TRUE)
    bounds=cbind(expected$family$linkinv(predicted$fit-critical*predicted$se.fit),
                 expected$family$linkinv(predicted$fit+critical*predicted$se.fit))
    expect_equal(source$prediction,unname(predict(expected,newdata=source,type="response")),tolerance=1e-10)
    expect_equal(source$prediction.lwr,unname(apply(bounds,1,min)),tolerance=1e-10)
    expect_equal(source$prediction.upr,unname(apply(bounds,1,max)),tolerance=1e-10)
    expect_true(all(source$prediction.lwr>0))
  }
})

test_that("logistic overlays use adjusted response predictions and preserve observed data", {
  skip_if_not_installed("jmvcore")
  set.seed(710)
  data=data.frame(x=rep(seq(-2,2,length.out=160),2),a=factor(rep(c("a","b"),each=160)),z=rnorm(320))
  data$y=rbinom(320,1,plogis(-.2+.6*data$x+.5*data$z+.4*(data$a=="b")+.4*data$x*(data$a=="b")))
  capture=new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_generalized_plot=jamovi_generalized_recorder(capture))
  for(style in c("auto","raw")) {
    options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","z"),family="binomial",
      modelTerms=list("x","a","z",c("x","a")),plotX="x",plotPanels="a",plotStyle=style,se=TRUE)
    analysis=flexplot:::glmgeneralizedClass$new(options=options,data=data)
    expect_true(analysis$run())
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    plot=capture$plot
    expect_no_warning({built=ggplot2::ggplot_build(plot)})
    expected=glm(y~x*a+z,data=data,family=binomial())
    type=if(style=="auto") "GeomSmooth" else "GeomLine"
    curves=which(vapply(plot$layers,function(layer) inherits(layer$geom,type) &&
      is.data.frame(layer$data) && "prediction" %in% names(layer$data),logical(1)))
    expect_length(curves,1L)
    source=plot$layers[[curves]]$data
    predictions=vapply(seq_len(nrow(source)),function(i) {
      profile=data;profile$x=source$x[i];profile$a=source$a[i]
      mean(predict(expected,profile,type="response"))
    },numeric(1))
    expect_equal(source$prediction,predictions,tolerance=1e-10)
    expect_true(all(source$prediction>=0 & source$prediction<=1))
    if(style=="auto") {
      observed=Filter(function(layer) is.data.frame(layer$data) && "proportion" %in% names(layer$data),plot$layers)[[1]]$data
      expect_equal(sum(observed$count),nrow(data))
      expect_equal(sum(observed$count*observed$proportion),sum(data$y),tolerance=1e-10)
      expect_true(all(source$ymin>=0 & source$ymax<=1))
      for (i in unique(round(seq(1,nrow(source),length.out=5)))) {
        profile=data;profile$x=source$x[i];profile$a=source$a[i]
        probability=predict(expected,profile,type="response")
        matrix=model.matrix(delete.response(terms(expected)),profile,contrasts.arg=expected$contrasts)
        gradient=colMeans(matrix*as.numeric(probability*(1-probability)))
        response_se=sqrt(drop(gradient %*% vcov(expected) %*% gradient))
        mean_p=mean(probability)
        link_se=response_se/(mean_p*(1-mean_p))
        expect_equal(source$ymin[i],plogis(qlogis(mean_p)-qnorm(.975)*link_se),tolerance=1e-10)
        expect_equal(source$ymax[i],plogis(qlogis(mean_p)+qnorm(.975)*link_se),tolerance=1e-10)
      }

    } else {
      raw=Filter(function(layer) inherits(layer$geom,"GeomPoint") && is.data.frame(layer$data) &&
        all(c("x","y","a") %in% names(layer$data)) && !"prediction" %in% names(layer$data),plot$layers)[[1]]$data
      expect_equal(nrow(raw),nrow(data))
      expect_setequal(raw$y,c(0,1))
    }
  }
})

test_that("new family outcome validation reports errors rather than changing the outcome", {
  skip_if_not_installed("jmvcore")
  for (family in c("Gamma","negativebinomial")) {
    for (response in c(-1,0,.5)) {
      if (family=="Gamma" && response>0) next
      data=transform(small,y=response)
      options=flexplot:::glmgeneralizedOptions$new(out="y",preds="x",family=family)
      analysis=flexplot:::glmgeneralizedClass$new(options=options,data=data)
      expect_true(analysis$run())
      expect_identical(analysis$results$fit$status,"error")
      expect_match(analysis$results$fit$.__enclos_env__$private$.error,
                   if(family=="Gamma") "strictly positive" else "nonnegative integer")
    }
  }
})

test_that("default overlays handle sparse bins and numeric grouping without refitting", {
  skip_if_not_installed("jmvcore")
  data=small
  data$y=data$y_bin
  capture=new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_generalized_plot=jamovi_generalized_recorder(capture))
  options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family="binomial",plotX="x",plotPanels="a")
  analysis=flexplot:::glmgeneralizedClass$new(options=options,data=data)
  expect_true(analysis$run())
  expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
  observed=Filter(function(layer) is.data.frame(layer$data) && "proportion" %in% names(layer$data),capture$plot$layers)[[1]]$data
  breaks=unique(quantile(data$x,probs=seq(0,1,length.out=11)))
  widths=diff(breaks)[as.numeric(observed$bin)]
  expect_equal(observed$xmax-observed$xmin,.8*as.numeric(widths),tolerance=1e-12)
  expect_equal(sum(observed$count),nrow(data))
  set.seed(7103)
  data=data.frame(x=rep(seq(-1,1,length.out=80),3),z=rnorm(240))
  data$y=rbinom(240,1,plogis(.3+.5*data$x+.3*data$z+.2*data$x*data$z))
  options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),family="binomial",
    modelTerms=list("x","z",c("x","z")),plotX="x",plotGroup="z")
  analysis=flexplot:::glmgeneralizedClass$new(options=options,data=data)
  expect_true(analysis$run())
  expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
  plot=capture$plot
  curve=which(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomSmooth"),logical(1)))
  expect_length(curve,1L)
  source=plot$layers[[curve]]$data
  expected=glm(y~x*z,data=data,family=binomial())
  expect_equal(source$prediction,unname(predict(expected,source,type="response")),tolerance=1e-10)
  built=ggplot2::ggplot_build(plot)
  expect_equal(length(unique(built$data[[curve]]$group)),3L)
})

test_that("Gamma inverse-link extrapolation is rejected instead of plotting invalid means", {
  skip_if_not_installed("jmvcore")
  set.seed(7210)
  data=data.frame(x=c(seq(-2,-1,length.out=80),seq(1,2,length.out=80)),
                   a=factor(rep(c("a","b"),each=80)))
  mean=1/(1+.9*data$x+1.9*(data$a=="a"))
  data$y=rgamma(160,shape=1000,scale=mean/1000)
  options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family="Gamma",plotX="x",plotGroup="a")
  analysis=flexplot:::glmgeneralizedClass$new(options=options,data=data)
  expect_true(analysis$run())
  model=analysis$results$plot$state$model
  profiles=expand.grid(x=range(data$x),a=levels(data$a))
  expect_true(any(predict(model,profiles,type="response")<0))
  expect_warning(expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot)), "beyond the range")
  expect_match(analysis$results$plot$.__enclos_env__$private$.error,"positive domain")
})
