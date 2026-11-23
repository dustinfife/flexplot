# Presentation must follow fitted objects and remain independent of display options.
test_that("fitted specification text preserves literal formulas and GLM context", {
  skip_if_not_installed("jmvcore")
  data = small
  names(data)[match(c("y", "x"), names(data))] = c("out <tag>", "dose + square")
  formula = `out <tag>` ~ `dose + square` * a + I(`dose + square`^2)
  model = lm(formula, data)
  result = lm_quadratic_analysis(graphic=FALSE)$results$specification
  flexplot:::jamovi_model_specification(result, list(model, lm(`out <tag>` ~ 1, data)))
  expect_true(result$visible)
  expect_match(result$content, "`out &lt;tag&gt;`", fixed=TRUE)
  expect_match(result$content, "`dose + square`", fixed=TRUE)
  expect_match(result$content, "I(`dose + square`^2)", fixed=TRUE)
  expect_match(result$content, "~ 1</p>", fixed=TRUE)
  expect_false(grepl("<tag>", result$content, fixed=TRUE))
  event_data = small
  event_data$event = ordered(event_data$y_bin, levels=c(0, 1), labels=c("No", "Yes <event>"))
  flexplot:::jamovi_model_specification(result, list(glm(event ~ x, event_data, family=binomial())))
  expect_match(result$content, "Success event: Yes &lt;event&gt;", fixed=TRUE)
  expect_false(grepl("<event>", result$content, fixed=TRUE))
  for (family in c("gaussian", "binomial", "poisson", "gammaLog", "Gamma", "negativebinomial")) {
    outcome = switch(family, gaussian="y", binomial="y_bin", gammaLog="y_gam", Gamma="y_gam", "y_pois")
    data = small
    if (family=="negativebinomial") {
      data = small[rep(seq_len(nrow(small)), each=10), , drop=FALSE]
      data$y_pois = withr::with_seed(826,
        rnbinom(nrow(data), size=.6, mu=exp(1+.3*as.numeric(scale(data$x)))))
    }
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=outcome,
      preds=c("x", "a"), family=family, graphic=FALSE, compare=TRUE, comparisonX="x",
      modelTerms=list("x", "a", c("x", "a")), interactions=TRUE,
      polynomials=TRUE, quadratic="x", comparisonTerms=list("x")), data)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    text = analysis$results$specification$content
    for (i in 1:2) {
      fit = state[[paste0("model", i)]]
      expect_match(text, flexplot:::jamovi_html_escape(paste(deparse(formula(fit),
        backtick=TRUE, width.cutoff=500L), collapse=" ")), fixed=TRUE)
      expect_match(text, paste0("Family: ", fit$family$family, "; Link: ", fit$family$link), fixed=TRUE)
      independent = if (inherits(fit, "negbin")) MASS::glm.nb(formula(fit), state$data, link=log) else
        glm(formula(fit), state$data, family=fit$family)
      expect_equal(coef(fit), coef(independent), tolerance=1e-12)
    }
    if (family=="binomial") expect_match(text, "Success event: 1", fixed=TRUE)
    expect_null(analysis$results$comparison$notes$models)
  }
})

test_that("LM and Mixed specifications report fitted terms and estimation methods", {
  skip_if_not_installed("jmvcore")
  lm = lm_quadratic_analysis(preds=c("x", "a"), compare=TRUE, graphic=FALSE,
    interactions=TRUE, modelTerms=list("x", "a", c("x", "a")),
    polynomials=TRUE, quadratic="x", comparisonTerms=list())
  expect_true(lm$run())
  state = lm$results$comparisonplot$state
  expect_match(lm$results$specification$content, "I(x^2)", fixed=TRUE)
  expect_match(lm$results$specification$content, "x:a", fixed=TRUE)
  expect_match(lm$results$specification$content, "Model 2:</strong> y ~ 1", fixed=TRUE)
  expect_equal(coef(state$model1), coef(stats::lm(formula(state$model1), state$data)))
  expect_null(lm$results$comparison$notes$models)
  expect_true(jamovi_reporting_update(lm, list(compare=FALSE, modelTerms=list(), quadratic=NULL)))
  expect_match(lm$results$specification$content, "Model 1:</strong> y ~ 1", fixed=TRUE)
  mixed = jamovi_mixed_comparison_analysis(estimation="ML", comparisonEstimation="REML")
  expect_true(mixed$run())
  expect_false(mixed$results$specification$visible)
  expect_true(mixed$results$comparisonSpecification$visible)
  text = mixed$results$comparisonSpecification$content
  expect_match(text, "I(x^2)", fixed=TRUE)
  expect_match(text, "(1 + x | id)", fixed=TRUE)
  expect_match(text, "(ML)", fixed=TRUE)
  expect_match(text, "(REML)", fixed=TRUE)
  expect_true(jamovi_reporting_update(mixed, list(compare=FALSE)))
  expect_true(mixed$results$specification$visible)
  expect_false(mixed$results$comparisonSpecification$visible)
})

test_that("result objects retain order, values and restored content through display edits", {
  skip_if_not_installed("jmvcore")
  analyses = list(
    lm=lm_quadratic_analysis(preds=c("x", "a"), graphic=FALSE, compare=TRUE,
      modelTerms=list("x", "a"), comparisonTerms=list("x")),
    glm=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin", preds=c("x", "a"),
      family="binomial", graphic=FALSE, compare=TRUE, comparisonX="x",
      modelTerms=list("x", "a"), comparisonTerms=list("x")), small),
    mixed=jamovi_mixed_comparison_analysis())
  orders = list(
    lm=c("waiting", "specification", "explanation", "plot", "glmcat", "factorMeans", "diff", "rsq", "comparisonplot", "comparison", "assumpplot"),
    glm=c("waiting", "specification", "explanation", "plot", "plotExplanation", "coefficients", "irt", "probabilities", "comparisonplot", "comparisonPlotExplanation", "comparison"),
    mixed=c("waiting", "specification", "comparisonSpecification", "explanation", "diagnostics", "plot", "plotExplanation", "model1Heading", "fixed", "random", "correlations", "model2Heading", "fixed2", "random2", "correlations2", "comparisonHeading", "comparisonplot", "comparisonPlotNote", "comparison"))
  for (name in names(analyses)) {
    analysis = analyses[[name]]
    expect_true(analysis$run())
    objects = analysis$results$items
    expect_identical(analysis$results$itemNames, orders[[name]])
    tables = function() lapply(Filter(function(item) inherits(item, "Table"), analysis$results$items), function(item) item$asDF)
    expected = tables()
    specification = analysis$results[[if(name=="mixed") "comparisonSpecification" else "specification"]]$content
    # Simulate pre-stage cached records: the new specification is absent and the
    # old records arrive in a different order. Restoration is keyed by name.
    old = list(state=raw(), group=list(elements=lapply(rev(Filter(function(item) inherits(item, "Html"), objects)), function(item) {
      list(name=item$name, state=raw(), html=list(content=item$content))
    })))
    old$group$elements = Filter(function(item) !identical(item$name, "specification"), old$group$elements)
    analysis$results$fromProtoBuf(old, character(), character())
    expect_identical(analysis$results$itemNames, orders[[name]])
    expect_identical(analysis$results$items, objects)
    for (values in list(list(plotX="x", plotPanels="a"), list(plotPanels=NULL, plotGroup="a"),
        list(plotGroup=NULL, plotLabels=list()), list(graphic=TRUE), list(graphic=FALSE))) {
      expect_true(jamovi_reporting_update(analysis, values))
      expect_identical(analysis$results$itemNames, orders[[name]])
      expect_equal(tables(), expected)
      expect_identical(analysis$results[[if(name=="mixed") "comparisonSpecification" else "specification"]]$content, specification)
    }
    expect_true(jamovi_reporting_update(analysis, list(graphic=TRUE)))
    labels = analysis$options$plotLabels
    if (length(labels)) {
      labels[[1]]$label = "Presentation-only edited label"
      expect_true(jamovi_reporting_update(analysis, list(plotLabels=labels)))
      expect_identical(analysis$results$itemNames, orders[[name]])
      expect_equal(tables(), expected)
    }
    expect_true(jamovi_reporting_update(analysis, list(compare=FALSE)))
    expect_identical(analysis$results$itemNames, orders[[name]])
    expect_false(analysis$results$comparisonplot$visible)
    expect_true(analysis$results$specification$visible)
    expect_true(jamovi_reporting_update(analysis, list(out=NULL)))
    expect_false(analysis$results$specification$visible)
    expect_identical(analysis$results$specification$content, "")
  }
})
