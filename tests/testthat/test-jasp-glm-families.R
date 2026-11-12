# Stand-ins for JASP host objects only; fitting, tables and plots use production code.
jasp_glm_result = R6::R6Class("JaspGlmTestResult", public=list(
  object=NULL, plotObject=NULL, data=NULL, footnotes=character(),
  initialize=function(...) invisible(NULL),
  dependOn=function(...) invisible(NULL),
  addCitation=function(...) invisible(NULL),
  addColumnInfo=function(...) invisible(NULL),
  addFootnote=function(note) { self$footnotes = c(self$footnotes, note) },
  setData=function(data) { self$data = data }
))

jasp_glm_environment = function() {
  namespace = asNamespace("flexplot")
  host = new.env(parent=namespace)
  functions = c("glinmod_jasp", ".read_glinmod_data", ".glinmod_compute",
    ".glinmod_model_plot", ".create_flexplot_glinmod", ".create_glinmod_coefs",
    ".fill_glinmod_table_fixed", "fancifyMyLabels")
  for (name in functions) {
    implementation = get(name, namespace)
    environment(implementation) = host
    assign(name, implementation, host)
  }
  host$createJaspState = host$createJaspPlot = host$createJaspTable = jasp_glm_result$new
  host$encodeColNames = host$decodeColNames = identity
  host
}

jasp_glm_options = function(family, plot=TRUE) {
  list(dependent="y", variables=c("x", "g"),
       interactions=list(list(components=c("x", "g"))), family=family,
       model=plot, ests=TRUE, univariates=FALSE, ghost=FALSE,
       alpha=.4, jitx=0, jity=0, theme="Classic")
}

jasp_glm_counts = function() {
  data = expand.grid(x=seq(-1, 1, length.out=15), g=factor(c("A", "B")),
                       replicate=seq_len(12))
  mean = exp(.7 + .5*data$x + .3*(data$g == "B") + .4*data$x*(data$g == "B"))
  data$y = qnbinom((data$replicate-.5)/12, size=1.7, mu=mean)
  data
}

jasp_glm_unexpected_refit = function(...) stop("Unexpected model refit")

test_that("the active JASP family selector exposes Logistic without changing other options", {
  qml = readLines(system.file("qml", "glinmod.qml", package="flexplot", mustWork=TRUE))
  start = grep("id: family", qml, fixed=TRUE)
  end = grep('label: qsTr("Distribution family")', qml, fixed=TRUE)
  selector = qml[seq.int(start, end)]
  entries = grep('value: "', selector, value=TRUE, fixed=TRUE)
  values = sub('.*value: "([^"]+)".*', '\\1', entries)
  expect_identical(values, c("Normal", "Logistic", "Poisson", "negativebinomial", "Gamma"))
  expect_true(any(grepl('label: qsTr("Logistic"), value: "Logistic"', entries, fixed=TRUE)))
})

test_that("JASP Logistic handles numeric and factor success coding in tables and plots", {
  host = jasp_glm_environment()
  binary = as.numeric((jasp_glm_counts()$replicate-.5)/12 <
                        plogis(.3+.5*jasp_glm_counts()$x))
  for (coding in c("numeric", "factor", "ordered")) {
    data = jasp_glm_counts()
    data$y = binary
    if (coding != "numeric") {
      # The fitted second level is success, even with explicitly reversed labels.
      data$y = factor(binary, levels=c(1, 0), labels=c("yes", "no"),
                        ordered=coding == "ordered")
    }
    independent = glm(y ~ x*g, data=data, family=binomial())
    results = new.env(parent=emptyenv())
    expect_no_warning(host$glinmod_jasp(results, data, jasp_glm_options("Logistic")))
    fit = results$glinmod_results$object
    expect_identical(fit$family$family, "binomial")
    expect_identical(fit$family$link, "logit")
    expect_equal(coef(fit), coef(independent), tolerance=1e-10)
    table = results$glinmod_table_fixed
    expect_equal(table$data$est, summary(independent)$coefficients[, 1], tolerance=1e-10)
    expect_equal(table$data$sterr, summary(independent)$coefficients[, 2], tolerance=1e-10)
    expect_equal(table$data$z, summary(independent)$coefficients[, 3], tolerance=1e-10)
    expect_match(table$footnotes, "the logit function", fixed=TRUE)
    plot = results$modelplot$plotObject
    built = ggplot2::ggplot_build(plot)
    overlays = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
      all(c("prediction", "model") %in% names(layer$data)), logical(1)))
    expect_length(overlays, 1)
    for (i in overlays) {
      source = plot$layers[[i]]$data
      expected = unname(predict(independent, source, type="response"))
      expect_equal(source$prediction, expected, tolerance=1e-10)
      expect_equal(sort(built$data[[i]]$y), sort(expected), tolerance=1e-10)
      expect_true(all(expected >= 0 & expected <= 1))
    }
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("invalid JASP Logistic outcomes are rejected before a model is fitted", {
  host = jasp_glm_environment()
  invalid = list(negative=c(-1, 0), counts=c(0, 2), fractions=c(.2, .8),
                 three_levels=factor(c("A", "B", "C")),
                 one_category=factor("A"), missing=c(NA_real_, NA_real_))
  local_mocked_bindings(glm.fit=jasp_glm_unexpected_refit, .package="stats")
  for (response in invalid) {
    data = jasp_glm_counts()
    data$y = rep(response, length.out=nrow(data))
    results = new.env(parent=emptyenv())
    expect_error(host$glinmod_jasp(results, data, jasp_glm_options("Logistic", plot=FALSE)),
                   "Logistic regression requires a numeric 0/1 outcome or a categorical outcome with exactly two observed levels.",
                   fixed=TRUE)
    expect_null(results$glinmod_results)
  }
})

test_that("JASP negative binomial fits, tables and plots use the actual NB model", {
  data = jasp_glm_counts()
  independent = MASS::glm.nb(y ~ x*g, data=data)
  host = jasp_glm_environment()
  results = new.env(parent=emptyenv())
  options = jasp_glm_options("negativebinomial")
  expect_no_warning(host$glinmod_jasp(results, data, options))
  fit = results$glinmod_results$object
  expect_s3_class(fit, "negbin")
  expect_identical(fit$family$link, "log")
  expect_equal(coef(fit), coef(independent), tolerance=1e-10)
  expect_equal(fit$theta, independent$theta, tolerance=1e-10)
  expect_equal(fit$SE.theta, independent$SE.theta, tolerance=1e-10)
  table = results$glinmod_table_fixed
  expected = summary(independent)$coefficients
  expect_equal(table$data$est, expected[, 1], tolerance=1e-10)
  expect_equal(table$data$sterr, expected[, 2], tolerance=1e-10)
  expect_equal(table$data$z, expected[, 3], tolerance=1e-10)
  expect_match(table$footnotes, "the log function", fixed=TRUE)
  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(independent)), tolerance=1e-10)
  expect_equal(AIC(fit), AIC(independent), tolerance=1e-10)
  expect_equal(BIC(fit), BIC(independent), tolerance=1e-10)

  local_mocked_bindings(glm.nb=jasp_glm_unexpected_refit, .package="MASS")
  local_mocked_bindings(glm.fit=jasp_glm_unexpected_refit, .package="stats")
  plot = jasp_glm_result$new()
  host$.create_flexplot_glinmod(results, plot, options, data)
  built = ggplot2::ggplot_build(plot$plotObject)
  overlays = which(vapply(plot$plotObject$layers,
    function(layer) is.data.frame(layer$data) &&
      all(c("prediction", "model") %in% names(layer$data)), logical(1)))
  expect_length(overlays, 1)
  for (i in overlays) {
    source = plot$plotObject$layers[[i]]$data
    expected = unname(predict(independent, newdata=source, type="response"))
    expect_equal(source$prediction, expected, tolerance=1e-10)
    expect_equal(sort(built$data[[i]]$y), sort(expected), tolerance=1e-10)
    layout = built$layout$layout
    for (panel in layout$PANEL) {
      actual = built$data[[i]][built$data[[i]]$PANEL == panel, ]
      profile = data.frame(x=actual$x, g=layout$g[layout$PANEL == panel])
      expect_equal(actual$y, unname(predict(independent, profile, type="response")), tolerance=1e-10)
    }
  }
  expect_no_warning(ggplot2::ggplotGrob(plot$plotObject))
  expect_equal(coef(fit), coef(independent), tolerance=1e-10)
})

test_that("NB prediction uncertainty retains existing coefficient delta intervals", {
  data = jasp_glm_counts()
  host = jasp_glm_environment()
  results = new.env(parent=emptyenv())
  host$glinmod_jasp(results, data, jasp_glm_options("negativebinomial", plot=FALSE))
  fit = results$glinmod_results$object
  independent = MASS::glm.nb(y ~ x*g, data=data)
  for (method in c("marginal", "observed", "reference")) {
    actual = compare_fits(y ~ g, data, fit, return.preds=TRUE, report.se=TRUE,
                            prediction_method=method)
    for (i in seq_len(nrow(actual))) {
      profile = if (method == "observed") data[data$g == actual$g[i], ] else data
      if (method == "reference") profile = actual[i, , drop=FALSE]
      profile$g = factor(rep(as.character(actual$g[i]), nrow(profile)), levels=levels(data$g))
      mean = unname(predict(independent, profile, type="response"))
      matrix = model.matrix(~ x*g, profile)
      gradient = colMeans(matrix * mean)
      se = sqrt(drop(t(gradient) %*% vcov(independent) %*% gradient))
      expect_equal(actual$prediction[i], mean(mean), tolerance=1e-10)
      expect_equal(actual$se[i], se, tolerance=1e-10)
      expect_equal(actual$prediction.lwr[i], mean(mean)-qnorm(.975)*se, tolerance=1e-10)
      expect_equal(actual$prediction.upr[i], mean(mean)+qnorm(.975)*se, tolerance=1e-10)
    }
  }
  reduced = MASS::glm.nb(y ~ x + g, data=data)
  comparison = model.comparison(fit, reduced)$statistics
  expect_equal(comparison$aic, round(c(AIC(independent), AIC(reduced)), 3))
  expect_equal(comparison$bic, round(c(BIC(independent), BIC(reduced)), 3))
})

test_that("other JASP GLM families retain fits, tables and model predictions", {
  host = jasp_glm_environment()
  families = c(Normal="gaussian", Poisson="poisson", Gamma="Gamma", Logistic="binomial")
  for (name in names(families)) {
    data = jasp_glm_counts()
    if (name == "Normal") data$y = .7 + .3*data$x + .2*(data$g == "B") + .1*sin(seq_len(nrow(data)))
    if (name == "Gamma") data$y = exp(data$y/10)
    if (name == "Logistic") data$y = as.numeric((data$replicate-.5)/12 < plogis(.3+.5*data$x))
    independent = glm(y ~ x*g, data=data, family=families[[name]])
    results = new.env(parent=emptyenv())
    expect_no_warning(host$glinmod_jasp(results, data, jasp_glm_options(name)))
    fit = results$glinmod_results$object
    expect_identical(class(fit), c("glm", "lm"))
    expect_identical(fit$family$family, independent$family$family)
    expect_equal(coef(fit), coef(independent), tolerance=1e-10)
    table = results$glinmod_table_fixed$data
    expect_equal(table$est, summary(independent)$coefficients[, 1], tolerance=1e-10)
    expect_equal(table$sterr, summary(independent)$coefficients[, 2], tolerance=1e-10)
    plot = results$modelplot$plotObject
    overlays = Filter(function(layer) is.data.frame(layer$data) &&
      all(c("prediction", "model") %in% names(layer$data)), plot$layers)
    expect_length(overlays, 1)
    for (layer in overlays) {
      expect_equal(layer$data$prediction,
        unname(predict(independent, layer$data, type="response")), tolerance=1e-10)
    }
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})
