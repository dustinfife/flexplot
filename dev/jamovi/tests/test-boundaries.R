test_that("packaged tests have no source-only or host-runtime file dependencies", {
  found = jamovi_test_source_dependencies(root)
  expect_equal(nrow(found),0L,info=paste(capture.output(print(found)),collapse="\n"))
})

test_that("the test boundary guard rejects excluded resources without evaluating tests", {
  directory = withr::local_tempdir()
  cases = c('readLines(file.path(root,"jamovi","new.u.yaml"))',
    'source(test_path("..","..","dev","fixture.R"))',
    'load("data/create_flexplot_linmod.Rdata")',
    'readLines(test_path("..","figs","deps.txt"))',
    'readLines("/Users/someone/checkout/config")',
    'Sys.glob("/Applications/jamovi.app/runtime.js")',
    'system.file("node_modules","jamovi-compiler",package="jmvtools")',
    'sys.source(file.path("R","adapter.R"),environment())',
    'system2("Rscript","dev/fixture.R")', 'devtools::load_all(root)', 'skip_if_not_installed("unlistedTestBackend")')
  for (code in cases) {
    writeLines(code,file.path(directory,"helper-new.R"))
    expect_true(nrow(jamovi_test_source_dependencies(root,directory)) > 0L,info=code)
  }
  writeLines(c('readLines(system.file("qml","glinmod.qml",package="flexplot"))',
    'readLines(system.file("jamovi","fixture.yaml",package="flexplot"))',
    'readLines(test_path("_snaps","plot.svg"))','readRDS(output)','stop("must not execute")'),
    file.path(directory,"helper-new.R"))
  expect_equal(nrow(jamovi_test_source_dependencies(root,directory)),0L)
  writeLines('module.exports = value => value + 1',file.path(directory,"helper-new.cjs"))
  expect_equal(nrow(jamovi_test_source_dependencies(root,directory)),0L)
  writeLines('require("../../jamovi/js/new")',file.path(directory,"helper-new.cjs"))
  expect_gt(nrow(jamovi_test_source_dependencies(root,directory)),0L)
})
