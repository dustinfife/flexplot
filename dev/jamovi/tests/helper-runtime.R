root = normalizePath(testthat::test_path("..","..",".."))
# Numerical fixtures are shared with installed-package tests; test files are not sourced.
for (file in list.files(file.path(root,"tests/testthat"),pattern="^helper.*\\.R$",full.names=TRUE))
  sys.source(file,envir=environment())
sys.source(file.path(root,"dev/jamovi/test-boundaries.R"),envir=environment())
