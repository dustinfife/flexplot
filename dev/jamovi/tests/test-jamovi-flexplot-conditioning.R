test_that("real serialized options restore both conditioning variables before rendering", {
  jmvcore:::initProtoBuf()
  group = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  cell = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  string = function(value) {item = cell$clone();item$s=value;item}
  variables = function(value) {items=group$clone();items$options=lapply(value,string);item=cell$clone();item$c=items;item}
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  for (given in list(c("a","b"),c("z","a"))) {
    pb = group$clone()
    suppress = cell$clone();suppress$o=1L
    ghost = cell$clone();ghost$o=0L
    pb$names = c("out","preds","given","suppr","ghost")
    pb$options = list(string("y"),variables("x"),variables(given),suppress,ghost)
    options = flexplot:::flexplotaOptions$new()
    options$fromProtoBuf(pb)
    restored = flexplot:::flexplotaOptions$new()
    restored$fromProtoBuf(options$asProtoBuf())
    expect_identical(restored$given,given)
    analysis = flexplot:::flexplotaClass$new(options=restored,data=small)
    expect_true(analysis$run())
    plot = jamovi_flexplot_render(analysis,capture)
    jamovi_flexplot_panel_rows(plot,small,given)
  }
})
