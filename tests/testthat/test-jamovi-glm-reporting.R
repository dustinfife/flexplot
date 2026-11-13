test_that("jamovi logistic sections reuse estimates.glm and independent conditional quantities", {
  skip_if_not_installed("jmvcore")
  set.seed(784)
  d = data.frame(x=rnorm(260), z=rnorm(260), a=factor(rep(c("A", "B"), 130)))
  d$y = rbinom(260, 1, plogis(.3+.7*d$x-.2*d$z+.4*(d$a=="B")+.3*d$x*d$z))
  options = flexplot:::glmgeneralizedOptions$new(out="y", preds=c("x","z","a"),
    modelTerms=list("x","z","a",c("x","z")), family="binomial", plotX="x", plotPanels=c("z","a"))
  analysis = flexplot:::glmgeneralizedClass$new(options=options, data=d)
  expect_true(analysis$run())
  model = glm(y~x*z+a, d, family=binomial())
  values = estimates(model)
  irt = analysis$results$irt$asDF
  expected = attr(values,"irt")
  expected = expected[expected$variable %in% c("x","z"),]
  expect_equal(irt$slope, expected$instantaneous_slope, tolerance=1e-12)
  expect_equal(irt$threshold, expected$intercept_threshold, tolerance=1e-12)
  b = coef(model)
  slope = b["x"]+b["x:z"]*mean(d$z)
  intercept = b["(Intercept)"]+b["z"]*mean(d$z)
  expect_equal(irt$slope[irt$variable=="x"], unname(slope/4), tolerance=1e-10)
  expect_equal(irt$threshold[irt$variable=="x"], unname(-intercept/slope), tolerance=1e-10)
  expect_equal(analysis$results$odds$asDF$odds, values$OR)
  expect_equal(analysis$results$standardized$asDF$odds, values$standardized.OR)
  probabilities = analysis$results$probabilities$asDF
  expected = vapply(levels(d$a), function(level) {
    profile=d; profile$a=factor(level,levels=levels(d$a))
    mean(predict(model,profile,type="response"))
  },numeric(1))
  expect_equal(probabilities$probability, unname(expected), tolerance=1e-12)
  expect_match(analysis$results$irt$notes$conditioning$note, "conditional")
  expect_false(grepl("Unshown",analysis$results$plotExplanation$content))
  expect_length(analysis$results$plot$getRefs(),0L)
  expect_setequal(analysis$results$coefficients$getRefs(),c("flexplot","graphicalAnalysis"))
})

test_that("all families retain fitted-model inference and existing family summaries", {
  skip_if_not_installed("jmvcore")
  set.seed(785)
  d = data.frame(x=seq(-1,1,length.out=200),a=factor(rep(c("A","B"),100)))
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    eta = .8+.2*d$x+.1*(d$a=="B")
    d$y = switch(family,gaussian=eta+rnorm(200),binomial=rbinom(200,1,plogis(eta)),
      poisson=rpois(200,exp(eta)),gammaLog=rgamma(200,shape=5,scale=exp(eta)/5),
      Gamma=rgamma(200,shape=5,scale=1/eta/5),negativebinomial=rnbinom(200,size=2,mu=exp(eta)))
    options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a"),family=family,plotX="x",plotPanels="a")
    analysis = flexplot:::glmgeneralizedClass$new(options=options,data=d)
    expect_true(analysis$run())
    expected = if(family=="negativebinomial") MASS::glm.nb(y~x+a,d) else
      glm(y~x+a,d,family=switch(family,gaussian=gaussian(),binomial=binomial(),poisson=poisson(),
        gammaLog=Gamma(link="log"),Gamma=Gamma(link="inverse")))
    model = analysis$results$plot$state$model
    expect_equal(coef(model),coef(expected),tolerance=1e-10)
    expect_equal(analysis$results$coefficients$asDF$se,unname(summary(expected)$coefficients[,2]),tolerance=1e-10)
    expect_equal(analysis$results$likelihood$asDF$value,
      c(as.numeric(logLik(expected)),AIC(expected),BIC(expected)),tolerance=1e-10)
    values = estimates(expected)
    if(family!="binomial") {
      table = analysis$results$effects$asDF
      expect_identical(table$term,attr(values,"row.names"))
      for (name in setdiff(names(table),"term")) expect_equal(table[[name]],values[[name]])
    }
    plot = flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",panels="a",se=FALSE)
    curves = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)
    expect_gt(length(curves),0L)
    coordinates = curves[[1]]$data
    expect_equal(coordinates$prediction,unname(predict(expected,coordinates,type="response")),tolerance=1e-10)
    expect_null(plot$labels$caption)
    expect_false(grepl("Unshown",analysis$results$plotExplanation$content))
  }
})

test_that("quadratic terms preserve linear defaults, predictions, and intercept-only validation", {
  skip_if_not_installed("jmvcore")
  set.seed(786)
  d = data.frame(x=seq(-2,2,length.out=240),z=rnorm(240))
  for(family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    eta=if (family=="Gamma") 1+.05*d$x+.08*d$x^2+.01*d$z else .5+.4*d$x-.3*d$x^2+.2*d$z
    d$y=switch(family,gaussian=eta+rnorm(240),binomial=rbinom(240,1,plogis(eta)),
      poisson=rpois(240,exp(eta)),gammaLog=rgamma(240,5,scale=exp(eta)/5),
      Gamma=rgamma(240,5,scale=1/eta/5),negativebinomial=rnbinom(240,size=2,mu=exp(eta)))
    options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),family=family,quadratic="x",plotX="x")
    analysis=flexplot:::glmgeneralizedClass$new(options=options,data=d)
    expect_true(analysis$run())
    expected=if(family=="negativebinomial") MASS::glm.nb(y~x+z+I(x^2),d) else
      glm(y~x+z+I(x^2),d,family=switch(family,gaussian=gaussian(),binomial=binomial(),poisson=poisson(),gammaLog=Gamma(link="log"),Gamma=Gamma(link="inverse")))
    expect_equal(coef(analysis$results$plot$state$model),coef(expected),tolerance=1e-10)
    expect_match(analysis$results$plotExplanation$content,"Unshown predictors")
    plot=flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",se=FALSE)
    curve=Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    expected_means=vapply(curve$x,function(x) {profile=d;profile$x=x;mean(predict(expected,profile,type="response"))},numeric(1))
    expect_equal(curve$prediction,expected_means,tolerance=1e-10)
    expect_no_warning(ggplot2::ggplotGrob(plot))
    if(family!="binomial") expect_match(analysis$results$explanation$content,"Additional estimates.glm summaries are unavailable")
    if(family=="binomial") expect_true(is.na(analysis$results$irt$asDF$threshold[analysis$results$irt$asDF$variable=="x"]))
  }
  options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),modelTerms=list(),plotX=NULL)
  analysis=flexplot:::glmgeneralizedClass$new(options=options,data=d)
  expect_true(analysis$run())
  expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_match(analysis$results$plot$.__enclos_env__$private$.error,"exactly one")
  options=flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),plotX=NULL)
  analysis=flexplot:::glmgeneralizedClass$new(options=options,data=d)
  expect_true(analysis$run())
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_error(flexplot:::jamovi_glm_generalized_plot(analysis$results$plot$state,x="x",group="x"),"different model predictors")
})
