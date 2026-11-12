jamovi_ui_terms = function(value) {
  if (value == "__NULL__") return(NULL)
  if (value == "__EMPTY__") return(list())
  strsplit(strsplit(value, ";", fixed=TRUE)[[1]], ":", fixed=TRUE)
}

test_that("compiled jamovi controls initialize main effects and keep model terms independent", {
  skip_if_not_installed("jmvcore")
  node = Sys.which("node")
  skip_if(!nzchar(node), "Node is unavailable for the compiled UI contract test")
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  skip_if(!nzchar(compiler), "The optional jamovi UI compiler is unavailable")
  root = normalizePath(test_path("..", ".."))
  skip_if(!file.exists(file.path(root, "jamovi", "glmbasic.u.yaml")),
          "UI sources are excluded from the built R package")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(c(output, paste0(output, ".header.R"))))
  args = c(test_path("js", "jamovi-glm-ui.cjs"), root, compiler, output,
           if (length(runtime)) runtime[1])
  log = system2(node, shQuote(args), stdout=TRUE, stderr=TRUE)
  expect_null(attr(log, "status"), info=paste(log, collapse="\n"))
  expect_true(file.exists(output))
  if (!file.exists(output)) return(invisible(NULL))
  generated = new.env(parent=asNamespace("flexplot"))
  sys.source(paste0(output, ".header.R"), envir=generated)
  expect_false(exists("glmbasic", envir=generated, inherits=FALSE))
  sys.source(file.path(root, "R", "glmbasic.b.R"), envir=generated)
  states = read.delim(output, stringsAsFactors=FALSE)
  formulas = list(supp=len ~ supp, `add-dose`=len ~ supp+dose,
    interaction=len ~ supp*dose, `remove-interaction`=len ~ supp+dose,
    `remove-dose`=len ~ supp, reopen=len ~ dose)
  for (i in seq_len(nrow(states))) {
    predictors = strsplit(states$preds[i], ";", fixed=TRUE)[[1]]
    options = generated$glmbasicOptions$new(out="len", preds=predictors,
      modelTerms=jamovi_ui_terms(states$primary[i]),
      comparisonTerms=jamovi_ui_terms(states$secondary[i]),
      compare=states$compare[i] == "true", graphic=FALSE)
    analysis = generated$glmbasicClass$new(options=options, data=ToothGrowth)
    expect_true(analysis$run())
    expected = lm(formulas[[states$stage[i]]], ToothGrowth)
    actual = lm(analysis$results$plot$state$formula, analysis$results$plot$state$data)
    expect_equal(coef(actual), coef(expected), tolerance=1e-12)
    expect_identical(attr(terms(actual), "term.labels"), attr(terms(expected), "term.labels"))
    expect_equal(analysis$results$rsq$asDF$Estimate[1], round(summary(expected)$r.squared, 4))
    expect_gt(nrow(analysis$results$glmcat$asDF), 0L)
    if (states$compare[i] == "true") {
      models = analysis$results$comparisonplot$state
      expect_equal(coef(models$model2), coef(lm(len ~ supp, ToothGrowth)), tolerance=1e-12)
      expect_equal(nrow(analysis$results$comparison$asDF), 2L)
    }
  }
})

test_that("ToothGrowth default and UI-initialized analyses populate the original results and plot", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  expected = lm(len ~ supp+dose, ToothGrowth)
  saved = NULL
  for (terms in list(NULL, list("supp", "dose"))) {
    options = flexplot:::glmbasicOptions$new(out="len", preds=c("supp", "dose"),
      modelTerms=terms, compare=FALSE)
    analysis = flexplot:::glmbasicClass$new(options=options, data=ToothGrowth)
    expect_true(analysis$run())
    state = analysis$results$plot$state
    expect_identical(attr(stats::terms(state$formula), "term.labels"), c("supp", "dose"))
    expect_equal(coef(lm(state$formula, state$data)), coef(expected), tolerance=1e-12)
    table = analysis$results$glmcat$asDF
    rows = match(c("Intercept", "Slope: dose"), table$var)
    coefficients = summary(expected)$coefficients[c("(Intercept)", "dose"), ]
    expect_equal(as.numeric(table$means[rows]), unname(coefficients[, 1]), tolerance=1e-12)
    expect_equal(as.numeric(table$lower[rows]), unname(round(coefficients[, 1]-1.96*coefficients[, 2], 2)))
    expect_equal(as.numeric(table$upper[rows]), unname(round(coefficients[, 1]+1.96*coefficients[, 2], 2)))
    profile = data.frame(supp=factor(levels(ToothGrowth$supp), levels=levels(ToothGrowth$supp)),
                         dose=mean(ToothGrowth$dose))
    intervals = predict(expected, profile, interval="confidence")
    rows = match(levels(ToothGrowth$supp), table$levels)
    expect_equal(as.numeric(table$means[rows]), unname(round(intervals[, "fit"], 2)))
    expect_equal(as.numeric(table$lower[rows]), unname(round(intervals[, "lwr"], 2)))
    expect_equal(as.numeric(table$upper[rows]), unname(round(intervals[, "upr"], 2)))
    differences = analysis$results$diff$asDF
    difference = unname(diff(intervals[, "fit"]))
    expect_equal(differences$diff, round(difference, 2))
    expect_equal(differences$cohensd, round(difference/sigma(expected), 2))
    rsq = analysis$results$rsq$asDF
    expect_equal(rsq$Estimate[rsq$var == "model"], round(summary(expected)$r.squared, 4))
    sums = anova(expected)$`Sum Sq`
    expect_equal(rsq$Estimate[match(c("supp", "dose"), rsq$var)], sums[1:2]/sum(sums), tolerance=1e-12)
    if (!is.null(saved)) expect_equal(list(table, differences, rsq), saved)
    saved = list(table, differences, rsq)
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    plot = capture$plot
    expect_no_warning({ built = ggplot2::ggplot_build(plot) })
    layers = which(vapply(plot$layers, function(layer) is.data.frame(layer$data) &&
      "prediction" %in% names(layer$data) && inherits(layer$geom, c("GeomPoint", "GeomLine")), logical(1)))
    expect_gt(length(layers), 0L)
    for (i in layers) {
      profile = plot$layers[[i]]$data
      # Low-cardinality numeric predictors retain numeric model values but are
      # ordered categories in the display data. Restore the fitted input class.
      profile$dose = as.numeric(as.character(profile$dose))
      predictions = predict(expected, newdata=profile)
      expect_equal(plot$layers[[i]]$data$prediction, unname(predictions), tolerance=1e-10)
      expect_equal(sort(built$data[[i]]$y), sort(unname(predictions)), tolerance=1e-10)
    }
  }
})
