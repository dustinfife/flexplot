test_that("GLM notes follow the relevant plot and legacy main effects stay hidden", {
  schema = yaml::read_yaml(file.path(root,"jamovi/glmgeneralized.r.yaml"))
  names = vapply(schema$items,`[[`,character(1),"name")
  expect_equal(match("plotExplanation",names),match("plot",names)+1L)
  expect_equal(match("comparisonPlotExplanation",names),match("comparisonplot",names)+1L)
  options = yaml::read_yaml(file.path(root,"jamovi/glmgeneralized.a.yaml"))$options
  main = Filter(function(option)option$name=="mainEffects",options)[[1]]
  expect_true(main$hidden)
  # The installed compiler and R API reject column-level tooltip metadata.
  compiler = system.file("node_modules","jamovi-compiler",package="jmvtools")
  columns = yaml::read_yaml(file.path(compiler,"schemas/resultelementschemas.yaml"))$Table$properties$columns$items
  expect_false(columns$additionalProperties)
  expect_false(any(c("description","tooltip") %in% names(columns$properties)))
  expect_false(any(c("description","tooltip") %in% names(jmvcore::Column$active_bindings)))
  generated = new.env(parent=asNamespace("flexplot"))
  sys.source(file.path(root,"R/glmgeneralized.h.R"),generated)
  sys.source(file.path(root,"R/glmgeneralized.b.R"),generated)
  analysis = generated$glmgeneralizedClass$new(options=generated$glmgeneralizedOptions$new(), data=NULL)
  analysis$.__enclos_env__$private$.init()
  results = analysis$results
  expect_equal(results$itemNames,names)
  for (name in c("plotExplanation","comparisonPlotExplanation")) expect_false(results[[name]]$visible)
})
