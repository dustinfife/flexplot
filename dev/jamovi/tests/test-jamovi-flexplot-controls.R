test_that("ordinary compiled UI honors binning roles and keeps suppression available", {
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  expect_true(nzchar(compiler))
  expect_gt(length(runtime),0L)
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-flexplot-controls.cjs"),
    root,compiler,runtime[1])),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(any(grepl("PASS: compiled ordinary Flexplot",log,fixed=TRUE)))
})
