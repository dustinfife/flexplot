test_that("jamovi generalized models reproduce glm coefficients, uncertainty and likelihood statistics", {
  skip_if_not_installed("jmvcore")
  set.seed(892)
  data = data.frame(x=rep(seq(-1, 1, length.out=80), 2), a=factor(rep(c("a", "b"), each=80)))
  eta = .2 + .5*data$x + .3*(data$a == "b") + .4*data$x*(data$a == "b")
  for (family in c("gaussian", "binomial", "poisson")) {
    data$y = switch(family, gaussian=eta+rnorm(nrow(data)),
      binomial=rbinom(nrow(data), 1, plogis(eta)), poisson=rpois(nrow(data), exp(eta)))
    analysis = jamovi_generalized_analysis(data, family, list("x", "a", c("x", "a")))
    expect_true(analysis$run())
    expected = glm(y ~ x*a, data=data, family=family)
    state = analysis$results$plot$state
    expect_equal(coef(state$model), coef(expected), tolerance=1e-12)
    table = analysis$results$coefficients$asDF
    coefficients = summary(expected)$coefficients
    expect_identical(table$term, rownames(coefficients))
    expect_equal(table$estimate, unname(coefficients[, 1]), tolerance=1e-12)
    expect_equal(table$se, unname(coefficients[, 2]), tolerance=1e-12)
    expect_equal(table$statistic, unname(coefficients[, 3]), tolerance=1e-12)
    expect_equal(table$p, unname(coefficients[, 4]), tolerance=1e-12)
    critical = if (family == "gaussian") qt(.975, df.residual(expected)) else qnorm(.975)
    expect_equal(table$lower, unname(coefficients[, 1]-critical*coefficients[, 2]), tolerance=1e-12)
    expect_equal(table$upper, unname(coefficients[, 1]+critical*coefficients[, 2]), tolerance=1e-12)
    fit = rbind(analysis$results$fit$asDF, analysis$results$likelihood$asDF)
    values = c(nobs(expected), df.residual(expected), deviance(expected), expected$null.deviance,
      as.numeric(logLik(expected)), AIC(expected), BIC(expected), summary(expected)$dispersion)
    expect_equal(fit$value[match(c("N", "Residual df", "Deviance", "Null deviance", "Log likelihood", "AIC", "BIC", "Dispersion"), fit$measure)], values, tolerance=1e-12)
    expect_identical(state$model$family$link, switch(family, gaussian="identity", binomial="logit", poisson="log"))
    expect_setequal(analysis$results$coefficients$getRefs(), c("flexplot", "graphicalAnalysis"))
  }
})

test_that("generalized model callbacks plot saved-model response predictions with panels and categorical x", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_generalized_plot=jamovi_generalized_recorder(capture))
  set.seed(971)
  data = data.frame(x=rep(seq(-1, 1, length.out=60), 2), a=factor(rep(c("a", "b"), each=60)))
  for (family in c("gaussian", "binomial", "poisson")) {
    eta = .2 + .4*data$x + .3*(data$a == "b") + .6*data$x*(data$a == "b")
    data$y = switch(family, gaussian=eta+rnorm(nrow(data)),
      binomial=rbinom(nrow(data), 1, plogis(eta)), poisson=rpois(nrow(data), exp(eta)))
    for (categorical in c(FALSE, TRUE)) {
      predictors = if (categorical) c("a", "x") else c("x", "a")
      analysis = jamovi_generalized_analysis(data, family,
        list("x", "a", c("x", "a")), predictors, panels=predictors[2])
      expect_true(analysis$run())
      image = analysis$results$plot
      saved = image$state
      expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(image)))
      expect_equal(image$state, saved)
      plot = capture$plot
      expect_no_warning({ built = ggplot2::ggplot_build(plot) })
      layers = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
        "prediction" %in% names(layer$data) &&
        inherits(layer$geom, c("GeomLine", "GeomPoint", "GeomSmooth")), logical(1)))
      expect_gt(length(layers), 0L)
      expected = glm(y ~ x*a, data=data, family=family)
      for (i in layers) {
        source = plot$layers[[i]]$data
        profile = source
        if (is.factor(profile$x)) profile$x = as.numeric(as.character(profile$x))
        predictions = predict(expected, newdata=profile, type="response")
        expect_equal(source$prediction, unname(predictions), tolerance=1e-10)
        expect_equal(sort(built$data[[i]]$y), sort(unname(predictions)), tolerance=1e-10)
        if (family == "binomial") expect_true(all(source$prediction >= 0 & source$prediction <= 1))
      }
      expect_gt(nrow(built$layout$layout), 1L)
    }
  }
})

test_that("generalized models handle eligible rows and binary coding consistently", {
  skip_if_not_installed("jmvcore")
  data = small
  data$y = data$y_bin
  data$y[1] = NA
  data$x[2] = NA
  data$b[3] = NA # Selected but absent from the fitted formula.
  for (categorical in c(FALSE, TRUE)) {
    if (categorical) data$y = ordered(data$y, levels=c(1, 0, 2))
    analysis = jamovi_generalized_analysis(data, "binomial", list("x", "a"), c("x", "a", "b"))
    expect_true(analysis$run())
    eligible = data[complete.cases(data[c("y", "x", "a")]), ]
    if (categorical) eligible$y = droplevels(eligible$y)
    expected = glm(y ~ x+a, eligible, family=binomial())
    model = analysis$results$plot$state$model
    expect_equal(nobs(model), nrow(small)-2)
    expect_equal(coef(model), coef(expected), tolerance=1e-12)
    expect_equal(fitted(model), fitted(expected), tolerance=1e-12)
    if (categorical) {
      expect_identical(levels(model$model$y), c("1", "0"))
      expect_identical(analysis$results$coefficients$notes$success$note, "Success event: 0")
    }
  }
})

test_that("invalid generalized models produce clear analysis errors", {
  skip_if_not_installed("jmvcore")
  cases = list(
    list(family="gaussian", data=transform(small, y=factor(a)), message="numeric outcome"),
    list(family="gaussian", data=transform(small, y=1), message="varies"),
    list(family="binomial", data=transform(small, y=2), message="0/1"),
    list(family="binomial", data=transform(small, y=factor(b)), message="two observed"),
    list(family="poisson", data=transform(small, y=-1), message="nonnegative integer"),
    list(family="poisson", data=transform(small, y=.5), message="nonnegative integer"),
    list(family="poisson", data=transform(small, y=0), message="positive count"),
    list(family="gaussian", data=transform(small, y=NA_real_), message="No complete"),
    list(family="gaussian", data=transform(small, z=x), message="not estimable", alias=TRUE))
  for (case in cases) {
    analysis = jamovi_generalized_analysis(case$data, case$family,
      predictors=if (isTRUE(case$alias)) c("x", "z") else c("x", "a"))
    expect_true(analysis$run())
    for (name in c("coefficients", "fit", "plot")) {
      expect_identical(analysis$results[[name]]$status, "error")
      expect_match(analysis$results[[name]]$.__enclos_env__$private$.error, case$message)
    }
    expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  }
})

test_that("categorical interactions preserve ordered contrasts and supplied-model probabilities", {
  skip_if_not_installed("jmvcore")
  set.seed(146)
  data = expand.grid(a=factor(c("a", "b")), b=ordered(c("low", "middle", "high")), repeat_id=1:25)
  data$y = rbinom(nrow(data), 1, plogis(-.3 + .4*(data$a == "b") + .2*as.integer(data$b)))
  analysis = jamovi_generalized_analysis(data, "binomial", list("a", "b", c("a", "b")), c("a", "b"))
  expect_true(analysis$run())
  state = analysis$results$plot$state
  expected = glm(y ~ a*b, data, family=binomial())
  expect_equal(coef(state$model), coef(expected), tolerance=1e-12)
  expect_equal(analysis$results$coefficients$asDF$se, unname(summary(expected)$coefficients[, 2]), tolerance=1e-12)
  plot = flexplot:::jamovi_glm_generalized_plot(state, "a", "b")
  for (layer in plot$layers) {
    if (!is.data.frame(layer$data) || !"prediction" %in% names(layer$data)) next
    expect_equal(layer$data$prediction, unname(predict(expected, layer$data, type="response")), tolerance=1e-10)
  }
  expect_no_warning(ggplot2::ggplot_build(plot))
})

test_that("new generalized UI and headers survive compilation and initialize the original additive model", {
  skip_if_not_installed("jmvcore")
  node = Sys.which("node")
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  skip_if(!nzchar(node) || !nzchar(compiler), "Optional node/jamovi UI compiler unavailable")
  root = normalizePath(test_path("..", ".."))
  skip_if(!file.exists(file.path(root, "jamovi", "glmgeneralized.u.yaml")), "UI source is not installed with the R package")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(c(output, paste0(output, ".header.R"))))
  args = c(test_path("js", "jamovi-glm-ui.cjs"), root, compiler, output,
    if (length(runtime)) runtime[1] else "", "glmgeneralized")
  log = system2(node, shQuote(args), stdout=TRUE, stderr=TRUE)
  expect_null(attr(log, "status"), info=paste(log, collapse="\n"))
  if (!file.exists(output)) return(invisible(NULL))
  states = read.delim(output, stringsAsFactors=FALSE)
  generated = new.env(parent=asNamespace("flexplot"))
  sys.source(paste0(output, ".header.R"), generated)
  sys.source(file.path(root, "R", "glmgeneralized.b.R"), generated)
  row = states[states$stage == "add-dose", ]
  terms = strsplit(strsplit(row$primary, ";", fixed=TRUE)[[1]], ":", fixed=TRUE)
  options = generated$glmgeneralizedOptions$new(out="len", preds=c("supp", "dose"), modelTerms=terms)
  analysis = generated$glmgeneralizedClass$new(options=options, data=ToothGrowth)
  expect_true(analysis$run())
  expect_equal(coef(analysis$results$plot$state$model), coef(glm(len ~ supp+dose, data=ToothGrowth)), tolerance=1e-12)
})

test_that("analysis-specific published references are attached to both existing analyses", {
  skip_if_not_installed("jmvcore")
  general = flexplot:::glmbasicResults$new(flexplot:::glmbasicOptions$new())
  descriptive = flexplot:::flexplotaResults$new(flexplot:::flexplotaOptions$new())
  for (results in list(general, descriptive)) {
    expect_setequal(results$getRefs(), c("flexplot", "graphicalAnalysis"))
  }
  refs = flexplot:::.jmvrefs
  expect_equal(refs$flexplot$year, 2022)
  expect_equal(refs$flexplot$pages, "477-496")
  expect_identical(refs$flexplot$doi, "10.1037/met0000424")
  expect_identical(refs$graphicalAnalysis$doi, "10.3758/s13428-020-01520-2")
})

test_that("saturated Poisson models retain fixed-dispersion inference", {
  skip_if_not_installed("jmvcore")
  data = data.frame(y=c(1, 3), x=c(0, 1))
  analysis = jamovi_generalized_analysis(data, "poisson", predictors="x")
  expect_true(analysis$run())
  expected = glm(y ~ x, data=data, family=poisson())
  table = analysis$results$coefficients$asDF
  expect_equal(table$estimate, unname(coef(expected)), tolerance=1e-12)
  expect_equal(table$se, unname(summary(expected)$coefficients[, 2]), tolerance=1e-12)
  expect_equal(table$lower, unname(coef(expected)-qnorm(.975)*summary(expected)$coefficients[, 2]), tolerance=1e-12)
  fit = rbind(analysis$results$fit$asDF, analysis$results$likelihood$asDF)
  expect_equal(fit$value[fit$measure == "Residual df"], 0)
  expect_equal(fit$value[fit$measure == "Dispersion"], 1)
})
