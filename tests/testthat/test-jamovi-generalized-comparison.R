jamovi_generalized_comparison_data = function(family) {
  set.seed(731)
  data = data.frame(x=rep(seq(-1, 1, length.out=60), 3), z=rnorm(180),
    a=factor(rep(c("A", "B", "C"), each=60)))
  eta = .5 + .2*data$x + .1*data$z + .15*(data$a=="B") + .2*data$x*(data$a=="C")
  data$y = switch(family, gaussian=eta+rnorm(180), binomial=rbinom(180, 1, plogis(eta)),
    poisson=rpois(180, exp(eta)), gammaLog=rgamma(180, shape=4, scale=exp(eta)/4),
    Gamma=rgamma(180, shape=1, scale=1/eta),
    negativebinomial=rnbinom(180, mu=exp(eta), size=2))
  data
}

jamovi_generalized_comparison_analysis = function(data, family, ...) {
  options = flexplot:::glmgeneralizedOptions$new(out="y", preds=c("x", "a", "z"),
    family=family, compare=TRUE, comparisonX="x", ...)
  flexplot:::glmgeneralizedClass$new(options=options, data=data)
}

jamovi_generalized_independent = function(formula, data, family) {
  if (family == "negativebinomial") return(MASS::glm.nb(formula, data=data))
  glm(formula, data=data, family=switch(family, gaussian=gaussian(), binomial=binomial(),
    poisson=poisson(), gammaLog=Gamma(link="log"), Gamma=Gamma(link="inverse")))
}

jamovi_generalized_comparison_recorder = function(capture) {
  recorder = jamovi_capture_model_plot
  environment(recorder) = list2env(list(capture=capture,
    original=flexplot:::jamovi_glm_generalized_comparison_plot), parent=environment(recorder))
  recorder
}

test_that("GLM comparisons fit common observations and use family likelihoods, not LM F-tests", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian", "binomial", "poisson", "gammaLog", "Gamma", "negativebinomial")) {
    data = jamovi_generalized_comparison_data(family)
    data$z[1] = NA
    data$x[2] = NA
    data$y[3] = NA
    eligible = data[complete.cases(data[c("x", "y", "a", "z")]), ]
    analysis = jamovi_generalized_comparison_analysis(data, family,
      modelTerms=list("x", "a", "z", c("x", "a")),
      comparisonTerms=list("x", "a"), graphic=FALSE)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    expect_false(is.null(state), info=family)
    if (is.null(state)) next
    models = list(jamovi_generalized_independent(y~x+a+z+x:a, eligible, family),
      jamovi_generalized_independent(y~x+a, eligible, family))
    for (i in 1:2) {
      fitted = state[[paste0("model", i)]]
      expect_equal(coef(fitted), coef(models[[i]]), tolerance=1e-12)
      expect_identical(rownames(model.frame(fitted)), rownames(eligible))
      expect_equal(summary(fitted)$coefficients[,2], summary(models[[i]])$coefficients[,2])
      if (family == "negativebinomial") expect_equal(fitted$theta, models[[i]]$theta)
    }
    table = analysis$results$comparison$asDF
    expect_equal(table$n, rep(nrow(eligible), 2))
    expect_equal(table$aic, round(vapply(models, AIC, numeric(1)), 3))
    bic = vapply(models, BIC, numeric(1))
    expect_equal(table$bic, round(bic, 3))
    expect_equal(table$bayes.factor, round(exp(c(diff(bic), -diff(bic))/2), 3))
    lr = 2*as.numeric(logLik(models[[1]])-logLik(models[[2]]))
    df = attr(logLik(models[[1]]), "df")-attr(logLik(models[[2]]), "df")
    expect_equal(table$lr[1], lr, tolerance=1e-12)
    expect_equal(table$p[1], pchisq(lr, df, lower.tail=FALSE), tolerance=1e-12)
    expect_null(analysis$results$comparison$getCell("p", rowNo=2)$value)
    expect_false(any(c("rsq", "adj.rsq") %in% names(table)))
    expect_match(analysis$results$comparison$notes$test$note, "Nested-model likelihood-ratio test")
    expect_equal(analysis$results$plot$state$data, state$data)
    expect_false(analysis$results$comparisonplot$visible)
  }
})

test_that("GLM comparison callbacks preserve response predictions, interactions, quadratics and panels", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_generalized_comparison_plot=jamovi_generalized_comparison_recorder(capture))
  for (family in c("gaussian", "binomial", "poisson", "gammaLog", "Gamma", "negativebinomial")) {
    data = jamovi_generalized_comparison_data(family)
    analysis = jamovi_generalized_comparison_analysis(data, family,
      modelTerms=list("x", "a", c("x", "a")), quadratic="x",
      comparisonTerms=list("x", "a", "z"), comparisonPanels=c("a", "z"), se=FALSE)
    expect_true(analysis$run())
    image = analysis$results$comparisonplot
    state = image$state
    expect_false(is.null(state), info=family)
    if (is.null(state)) next
    expect_true(image$visible)
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.comparisonplot(image)))
    plot = capture$plot
    expect_no_warning({ built = ggplot2::ggplot_build(plot) })
    expect_true(all(c("a", "z_binned") %in% names(built$layout$layout)))
    curves = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data) &&
      inherits(layer$geom, "GeomLine"), plot$layers)
    expect_length(curves, 1L)
    predictions = curves[[1]]$data
    expect_length(unique(predictions$model), 2L)
    expected = vapply(seq_len(nrow(predictions)), function(i) {
      profile = state$data
      for (variable in c("x", "a", "z")) profile[[variable]] = predictions[[variable]][i]
      model = if (grepl("^model1", predictions$model[i])) state$model1 else state$model2
      mean(predict(model, newdata=profile, type="response"))
    }, numeric(1))
    expect_equal(predictions$prediction, expected, tolerance=1e-10)
    if (family == "binomial") {
      observed = Filter(function(layer) is.data.frame(layer$data) && "proportion" %in% names(layer$data), plot$layers)
      expect_length(observed, 1L)
      expect_equal(sum(observed[[1]]$data$count), nrow(data))
      expect_true(all(predictions$prediction >= 0 & predictions$prediction <= 1))
    }
    expect_equal(image$state, state)
  }
})

test_that("nonnested and equivalent GLMs have genuinely blank unsupported likelihood tests", {
  skip_if_not_installed("jmvcore")
  for (terms in list(list("a", "z"), list("x", "a"))) {
    analysis = jamovi_generalized_comparison_analysis(jamovi_generalized_comparison_data("poisson"),
      "poisson", modelTerms=list("x", "a"), comparisonTerms=terms, graphic=FALSE)
    expect_true(analysis$run())
    for (column in c("lr", "df", "p")) for (row in 1:2) {
      expect_null(analysis$results$comparison$getCell(column, rowNo=row)$value)
    }
    expect_length(analysis$results$comparison$asDF$aic, 2L)
  }
})

test_that("GLM comparison toggles clear results and preserve the default single-model analysis", {
  skip_if_not_installed("jmvcore")
  data = jamovi_generalized_comparison_data("binomial")
  options = flexplot:::glmgeneralizedOptions$new(out="y", preds=c("x", "a", "z"),
    family="binomial", modelTerms=list("x", "a"), comparisonTerms=list("x", "a", "z"),
    comparisonQuadratic="x", comparisonX="x", graphic=FALSE)
  analysis = jamovi_reporting_host(options, data)
  expect_true(analysis$run())
  original = analysis$results$coefficients$asDF
  expect_false(options$compare)
  expect_false(options$includeP)
  expect_false(analysis$results$comparison$visible)
  expect_null(analysis$results$comparisonplot$state)
  expect_true(jamovi_reporting_update(analysis, list(compare=TRUE)))
  expect_true(analysis$results$comparison$visible)
  expect_equal(coef(analysis$results$comparisonplot$state$model2),
    coef(glm(y~x+a+z+I(x^2), data=data, family=binomial())))
  expect_true(jamovi_reporting_update(analysis, list(compare=FALSE)))
  expect_false(analysis$results$comparison$visible)
  expect_false(analysis$results$comparisonplot$visible)
  expect_null(analysis$results$comparisonplot$state)
  expect_equal(analysis$results$comparison$rowCount, 0L)
  expect_equal(analysis$results$coefficients$asDF, original)
  # Server-side validation, independent of JavaScript supplier filtering.
  option = options$option("compare")
  option$value = TRUE
  option = options$option("comparisonX")
  option$value = "missing"
  expect_error(flexplot:::jamovi_glm_generalized_comparison(options, data), "Visualization variables")
})

test_that("comparison uncertainty uses each model's existing family-specific link intervals", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian", "binomial", "poisson", "gammaLog", "negativebinomial")) {
    analysis = jamovi_generalized_comparison_analysis(jamovi_generalized_comparison_data(family),
      family, modelTerms=list("x"), quadratic="x", comparisonTerms=list("x"), graphic=FALSE)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    plot = flexplot:::jamovi_glm_generalized_comparison_plot(state, se=TRUE, style="raw")
    bands = Filter(function(layer) inherits(layer$geom, "GeomRibbon"), plot$layers)
    expect_length(bands, 2L)
    for (i in 1:2) {
      data = bands[[i]]$data
      model = state[[paste0("model", i)]]
      expected = predict(model, newdata=data.frame(x=data$x), type="link", se.fit=TRUE)
      critical = if (family %in% c("gaussian", "gammaLog")) qt(.975, df.residual(model)) else qnorm(.975)
      expect_equal(data$prediction.lwr, unname(model$family$linkinv(expected$fit-critical*expected$se.fit)), tolerance=1e-10)
      expect_equal(data$prediction.upr, unname(model$family$linkinv(expected$fit+critical*expected$se.fit)), tolerance=1e-10)
    }
    expect_no_warning(ggplot2::ggplot_build(plot))
  }
})

test_that("GLM comparisons count omissions from the original analysis input without changing fits", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(752)
  input = data.frame(x=rnorm(120),z=rnorm(120),unused=NA_real_)
  input$y = 1+.3*input$x-.2*input$z+rnorm(120)
  rownames(input) = paste0("source_",31:150)
  cases = list(none=list(),primary=list(x=1:10),secondary=list(z=1:30),outcome=list(y=1:5),
    overlap=list(x=1:20,z=11:30,y=21:35))
  captured = new.env(parent=emptyenv());original = flexplot:::jamovi_glm_generalized_fit
  local_mocked_bindings(jamovi_glm_generalized_fit=function(...) {
    state = original(...);captured$fits = c(captured$fits,list(state));state
  },.package="flexplot")
  for (missing in cases) {
    data = input
    for (name in names(missing)) data[missing[[name]],name] = NA_real_
    mask = stats::complete.cases(data[c("y","x","z")])
    eligible = data[mask,,drop=FALSE]
    options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","unused"),family="gaussian",
      compare=TRUE,modelTerms=list("x"),comparisonTerms=list("x","z"),comparisonX="x",graphic=FALSE)
    captured$fits = list()
    analysis = jamovi_reporting_host(options,data)
    expect_true(analysis$run())
    comparison = analysis$results$comparisonplot$state
    state = analysis$results$plot$state
    expect_equal(state$omitted,sum(!mask))
    expect_equal(comparison$primary$omitted,sum(!mask))
    expect_identical(rownames(state$data),rownames(eligible))
    expect_equal(analysis$results$comparison$asDF$n,rep(sum(mask),2))
    expect_length(captured$fits,2L)
    expect_identical(captured$fits[[1]]$omitted,0L)
    for (i in 1:2) {
      model = comparison[[paste0("model",i)]]
      independent = stats::glm(if (i==1L) y~x else y~x+z,data=eligible,family=gaussian())
      expect_identical(model,captured$fits[[i]]$model)
      expect_identical(state$data,captured$fits[[i]]$data)
      expect_identical(rownames(stats::model.frame(model)),rownames(eligible))
      expect_equal(stats::coef(model),stats::coef(independent),tolerance=1e-12)
      expect_equal(stats::vcov(model),stats::vcov(independent),tolerance=1e-12)
      expect_equal(summary(model)$coefficients[,2],summary(independent)$coefficients[,2],tolerance=1e-12)
      expect_equal(as.numeric(stats::logLik(model)),as.numeric(stats::logLik(independent)),tolerance=1e-12)
      expect_equal(stats::AIC(model),stats::AIC(independent),tolerance=1e-12)
      expect_equal(stats::BIC(model),stats::BIC(independent),tolerance=1e-12)
      expect_equal(stats::predict(model,newdata=eligible,type="response"),stats::predict(independent,newdata=eligible,type="response"),tolerance=1e-12)
    }
    independent1 = stats::glm(y~x,data=eligible,family=gaussian())
    critical = stats::qt(.975,stats::df.residual(independent1))
    radius = critical*sqrt(diag(stats::vcov(independent1)))
    expect_equal(analysis$results$coefficients$asDF$lower,unname(stats::coef(independent1)-radius),tolerance=1e-12)
    expect_equal(analysis$results$coefficients$asDF$upper,unname(stats::coef(independent1)+radius),tolerance=1e-12)
    model1 = comparison$model1;model2 = comparison$model2
    expected = model.comparison(model1,model2)$statistics
    expect_identical(comparison$statistics,expected)
    lr = 2*as.numeric(stats::logLik(comparison$model2)-stats::logLik(comparison$model1))
    df = attr(stats::logLik(comparison$model2),"df")-attr(stats::logLik(comparison$model1),"df")
    expect_equal(comparison$lr,lr,tolerance=1e-12)
    expect_equal(comparison$df,df)
    expect_equal(comparison$p,stats::pchisq(lr,df,lower.tail=FALSE),tolerance=1e-12)
    note = analysis$results$coefficients$notes$missing
    if (any(!mask)) expect_identical(note$note,paste(sum(!mask),"observations omitted because model variables were missing.")) else expect_null(note)
  }
})

test_that("omission notes follow comparison toggles and preserve standalone behavior", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(753)
  data = data.frame(x=rnorm(120),z=rnorm(120),a=factor(rep(c("A","B"),60)))
  data$y = .4+.2*data$x+.1*data$z+rnorm(120)
  data$z[1:30] = NA_real_
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","a"),family="gaussian",
    modelTerms=list("x"),comparisonTerms=list("x","z"),comparisonX="x",compare=FALSE,graphic=FALSE),data)
  expect_true(analysis$run())
  expect_equal(analysis$results$plot$state$omitted,0)
  expect_equal(stats::nobs(analysis$results$plot$state$model),120)
  expect_null(analysis$results$coefficients$notes$missing)
  for (enabled in c(TRUE,FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(compare=enabled)))
    expect_equal(analysis$results$plot$state$omitted,if(enabled)30 else 0)
    expect_equal(stats::nobs(analysis$results$plot$state$model),if(enabled)90 else 120)
    if (enabled) expect_identical(analysis$results$coefficients$notes$missing$note,
      "30 observations omitted because model variables were missing.") else expect_null(analysis$results$coefficients$notes$missing)
  }
  # Model 1 now needs z, an interaction and a square; Model 2 needs different predictors.
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x","z",c("x","z")),quadratic="x",
    polynomials=TRUE,interactions=TRUE,comparisonTerms=list("x","a"))))
  expect_equal(analysis$results$plot$state$omitted,30)
  expect_identical(analysis$results$coefficients$notes$missing$note,
    "30 observations omitted because model variables were missing.")
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_equal(analysis$results$plot$state$omitted,30)
  expect_equal(stats::coef(analysis$results$plot$state$model),stats::coef(stats::glm(y~x+z+x:z+I(x^2),data=data)),tolerance=1e-12)
})

test_that("common-sample omission notes and numerical results are correct for every GLM family", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(754)
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    data = jamovi_generalized_comparison_data(family)
    data$x[1:10] = NA_real_;data$z[6:20] = NA_real_;data$y[16:25] = NA_real_
    analysis = jamovi_generalized_comparison_analysis(data,family,graphic=FALSE,
      modelTerms=list("x","a"),comparisonTerms=list("x","z","a"))
    expect_true(analysis$run())
    state = analysis$results$plot$state
    expect_equal(state$omitted,25)
    expect_identical(analysis$results$coefficients$notes$missing$note,
      "25 observations omitted because model variables were missing.")
    independent = jamovi_generalized_independent(y~x+a,state$data,family)
    expect_equal(stats::coef(state$model),stats::coef(independent),tolerance=1e-12)
    expect_equal(stats::vcov(state$model),stats::vcov(independent),tolerance=1e-12)
    expect_equal(stats::predict(state$model,type="response"),stats::predict(independent,type="response"),tolerance=1e-12)
  }
})
