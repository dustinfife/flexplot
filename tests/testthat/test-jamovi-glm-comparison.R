jamovi_comparison_analysis = function(data=small, ...) {
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("x", "z", "a", "b"),
    compare=TRUE, graphic=FALSE, ...)
  flexplot:::glmbasicClass$new(options=options, data=data)
}

jamovi_comparison_capture = function(state) {
  capture$plot = original(state)
  capture$plot
}

jamovi_comparison_recorder = function(capture) {
  recorder = jamovi_comparison_capture
  environment(recorder) = list2env(list(capture=capture,
    original=flexplot:::jamovi_glm_comparison_plot), parent=environment(recorder))
  recorder
}

test_that("jamovi compares nonnested interaction models on one complete-case population", {
  skip_if_not_installed("jmvcore")
  data = small
  data$y[1] = NA
  data$x[2] = NA
  data$a[3] = NA
  data$z[4] = NA # Needed only by Model 2.
  data$b[5] = NA # Selected but not in either model; must keep this row.
  eligible = data[complete.cases(data[c("y", "x", "a", "z")]), ]
  analysis = jamovi_comparison_analysis(data,
    modelTerms=list("x", "a", c("x", "a")),
    comparisonTerms=list("x", "a", "z"), comparisonX="x", comparisonPanels="a")
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  independent = list(lm(y ~ x*a, eligible), lm(y ~ x+a+z, eligible))
  expect_equal(nrow(state$data), 23L)
  expect_true("5" %in% rownames(state$data))
  for (i in 1:2) {
    model = state[[paste0("model", i)]]
    expect_equal(coef(model), coef(independent[[i]]), tolerance=1e-12)
    expect_identical(rownames(model.frame(model)), rownames(eligible))
  }
  # These specifications are nonnested despite sharing most main effects.
  table = analysis$results$comparison$asDF
  expect_true(all(is.na(table$p)))
  expect_equal(table$n, rep(nrow(eligible), 2))
  expect_equal(table$aic, round(vapply(independent, AIC, numeric(1)), 3))
  expect_equal(table$bic, round(vapply(independent, BIC, numeric(1)), 3))
  expect_equal(table$rsq, round(vapply(independent, function(m) summary(m)$r.squared, numeric(1)), 3))
  expect_equal(table$adj.rsq, round(vapply(independent, function(m) summary(m)$adj.r.squared, numeric(1)), 3))
  expect_equal(sort(table$bayes.factor), round(sort(c(exp(diff(vapply(independent, BIC, numeric(1)))/2),
                                                  exp(-diff(vapply(independent, BIC, numeric(1)))/2))), 3))
  expect_equal(analysis$results$plot$state$data, state$data)
})

test_that("jamovi reports the existing nested F-test and fits numeric and categorical interactions", {
  skip_if_not_installed("jmvcore")
  cases = list(
    list(full=list("x", "a", c("x", "a")), reduced=list("x", "a"),
         formulas=list(y ~ x*a, y ~ x+a)),
    list(full=list("a", "b", c("a", "b")), reduced=list("a", "b"),
         formulas=list(y ~ a*b, y ~ a+b)),
    list(full=list("x"), reduced=list(), formulas=list(y ~ x, y ~ 1)))
  for (case in cases) {
    analysis = jamovi_comparison_analysis(modelTerms=case$full,
      comparisonTerms=case$reduced, comparisonX=all.vars(case$formulas[[1]])[2])
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    models = lapply(case$formulas, lm, data=small)
    expect_equal(coef(state$model1), coef(models[[1]]), tolerance=1e-12)
    expect_equal(coef(state$model2), coef(models[[2]]), tolerance=1e-12)
    expected = anova(models[[1]], models[[2]])$`Pr(>F)`[2]
    expect_equal(analysis$results$comparison$asDF$p[1], expected, tolerance=1e-12)
    expect_true(is.na(analysis$results$comparison$asDF$p[2]))
    expect_identical(attr(terms(state$model1), "term.labels"),
                     attr(terms(models[[1]]), "term.labels"))
  }
})

test_that("actual jamovi comparison callbacks preserve models, panel membership and marginal predictions", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_comparison_plot=jamovi_comparison_recorder(capture))
  cases = list(
    list(x="x", panels="a", full=list("x", "a", c("x", "a")), reduced=list("x", "a")),
    list(x="a", panels="b", full=list("a", "b", c("a", "b")), reduced=list("a", "b")),
    list(x="x", panels=c("a", "z"), full=list("x", "a", "z", c("x", "a")), reduced=list("x", "z")))
  for (case in cases) {
    analysis = jamovi_comparison_analysis(modelTerms=case$full,
      comparisonTerms=case$reduced, comparisonX=case$x, comparisonPanels=case$panels)
    expect_true(analysis$run())
    image = analysis$results$comparisonplot
    state = image$state
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.comparisonplot(image)))
    expect_equal(image$state, state) # Rendering does not refit or mutate models.
    plot = capture$plot
    expect_no_warning({ built = ggplot2::ggplot_build(plot) })
    layers = which(vapply(plot$layers, function(layer)
      is.data.frame(layer$data) && "prediction" %in% names(layer$data), logical(1)))
    expect_true(length(layers) > 0L)
    for (i in layers) {
      predictions = plot$layers[[i]]$data
      expect_setequal(unique(predictions$model), c("model1 (lm)", "model2 (lm)"))
      # Independent full-population standardization at each displayed coordinate.
      expected = vapply(seq_len(nrow(predictions)), function(j) {
        profile = state$data
        for (variable in c(case$x, case$panels)) {
          # Numeric conditioning is represented by bin medians in prediction rows.
          profile[[variable]] = predictions[[variable]][j]
        }
        model = if (predictions$model[j] == "model1 (lm)") state$model1 else state$model2
        mean(predict(model, newdata=profile))
      }, numeric(1))
      expect_equal(predictions$prediction, expected, tolerance=1e-10)
      expect_equal(sort(built$data[[i]]$y), sort(expected), tolerance=1e-10)
      layout = built$layout$layout
      facets = setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))
      for (panel in seq_len(nrow(layout))) {
        keep = rep(TRUE, nrow(predictions))
        for (variable in facets) {
          keep = keep & as.character(predictions[[variable]]) == as.character(layout[[variable]][panel])
        }
        expect_equal(sort(built$data[[i]]$y[built$data[[i]]$PANEL == layout$PANEL[panel]]),
                     sort(expected[keep]), tolerance=1e-10)
      }
    }
    expect_true(all(vapply(case$panels, function(variable)
      any(c(variable, paste0(variable, "_binned")) %in% names(built$layout$layout)), logical(1))))
    raw = which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint") &&
      !(is.data.frame(layer$data) && "prediction" %in% names(layer$data)), logical(1)))
    if (case$x == "x") expect_equal(nrow(built$data[[raw]]), nrow(small))
    expect_equal(length(unique(unlist(lapply(built$data[layers], function(d) d$colour)))), 2L)
  }
})

test_that("comparison-disabled jamovi analyses preserve the additive single-model outputs", {
  skip_if_not_installed("jmvcore")
  for (preds in list("x", "a", c("x", "a"))) {
    old = flexplot:::glmbasicClass$new(data=small,
      options=flexplot:::glmbasicOptions$new(out="y", preds=preds, graphic=FALSE))
    explicit = flexplot:::glmbasicClass$new(data=small,
      options=flexplot:::glmbasicOptions$new(out="y", preds=preds, graphic=FALSE,
        modelTerms=as.list(preds), compare=FALSE, comparisonTerms=list()))
    expect_true(old$run())
    expect_true(explicit$run())
    for (table in c("glmcat", "diff", "rsq")) {
      expect_equal(explicit$results[[table]]$asDF, old$results[[table]]$asDF)
    }
    expect_equal(old$results$plot$state, explicit$results$plot$state)
    expect_null(explicit$results$comparisonplot$state)
    expect_equal(nrow(explicit$results$comparison$asDF), 0L)
  }
})

test_that("invalid jamovi comparisons report actionable errors instead of incomplete results", {
  skip_if_not_installed("jmvcore")
  cases = list(
    list(terms=list("missing"), data=small, message="selected predictor"),
    list(terms=list("x", "z"), data=transform(small, z=x), message="not estimable"),
    list(terms=list("x"), data=transform(small, y=NA_real_), message="No complete"))
  for (case in cases) {
    analysis = jamovi_comparison_analysis(case$data, modelTerms=case$terms,
      comparisonTerms=list("x"), comparisonX="x")
    expect_true(analysis$run())
    expect_identical(analysis$results$comparison$status, "error")
    expect_match(analysis$results$comparison$.__enclos_env__$private$.error, case$message)
    expect_null(analysis$results$comparisonplot$state)
    expect_false(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  }
})

test_that("jamovi model-term options support formula input without interpreting column names", {
  skip_if_not_installed("jmvcore")
  expect_error(flexplot:::jamovi_glm_terms(~x-1), "no-intercept formulas")
  expect_error(flexplot:::jamovi_glm_terms(~factor(x)), "predictor names and interactions")
  result = glmbasic(small, out="y", preds=c("x", "a"), graphic=FALSE,
    compare=TRUE, modelTerms=~x*a, comparisonTerms=~x+a, comparisonX="x")
  expect_equal(result$comparison$asDF$p[1], anova(lm(y~x*a, small), lm(y~x+a, small))$`Pr(>F)`[2])
  data = small
  names(data)[names(data) == "x"] = "x + 1"
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("x + 1", "a"),
    compare=TRUE, modelTerms=list("x + 1", "a", c("x + 1", "a")),
    comparisonTerms=list("x + 1", "a"), comparisonX="x + 1")
  analysis = flexplot:::glmbasicClass$new(options=options, data=data)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_identical(all.vars(formula(state$model1)), c("y", "x + 1", "a"))
  expect_equal(unname(coef(state$model1)), unname(coef(lm(y ~ x*a, small))), tolerance=1e-12)
  # The existing estimates helper cannot summarize this name, but a valid
  # comparison remains available and the summary table explains the failure.
  expect_identical(analysis$results$glmcat$status, "error")
  expect_match(analysis$results$glmcat$.__enclos_env__$private$.error, "summaries are unavailable")
})

test_that("interaction-only and intercept-only primary models retain valid comparison results", {
  skip_if_not_installed("jmvcore")
  for (terms in list(list(c("x", "a")), list())) {
    analysis = jamovi_comparison_analysis(modelTerms=terms, comparisonTerms=list("x", "a"),
      comparisonX="x", comparisonPanels="a")
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    formula = if (length(terms)) y ~ x:a else y ~ 1
    expect_equal(coef(state$model1), coef(lm(formula, small)), tolerance=1e-12)
    expect_equal(nrow(analysis$results$comparison$asDF), 2L)
    expect_no_warning(ggplot2::ggplot_build(flexplot:::jamovi_glm_comparison_plot(state)))
    if (length(terms)) {
      expect_identical(analysis$results$glmcat$status, "error")
    } else {
      expect_equal(analysis$results$glmcat$asDF$means, mean(small$y), tolerance=1e-12)
    }
  }
})
