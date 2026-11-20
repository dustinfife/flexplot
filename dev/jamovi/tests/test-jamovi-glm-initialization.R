jamovi_ui_terms = function(value) {
  if (value == "__NULL__") return(NULL)
  if (value == "__EMPTY__") return(list())
  strsplit(strsplit(value, ";", fixed=TRUE)[[1]], ":", fixed=TRUE)
}

test_that("compiled jamovi controls initialize main effects and keep model terms independent", {
  node = Sys.which("node")
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  root = normalizePath(test_path("..", "..", ".."))
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
