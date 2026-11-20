test_that("model specifications become separate paragraphs in the installed jamovi renderer", {
  node = Sys.which("node")
  assets = "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets"
  analysis = jamovi_comparison_analysis(modelTerms=list("x", "a"), comparisonTerms=list("x"))
  expect_true(analysis$run())
  file = tempfile(fileext=".txt")
  withr::defer(unlink(file))
  writeLines(analysis$results$comparison$notes$models$note, file)
  log = system2(node, shQuote(c(test_path("js", "jamovi-note-layout.cjs"), assets, file)), stdout=TRUE, stderr=TRUE)
  expect_null(attr(log, "status"), info=paste(log, collapse="\n"))
})
