test_that("Mean + sterr reaches the actual jamovi callback and survives option restoration", {
  jmvcore:::initProtoBuf()
  group = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions", asNamespace("jmvcore")))
  cell = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption", asNamespace("jmvcore")))
  string = function(value) {item = cell$clone(); item$s=value; item}
  variables = function(value) {
    items = group$clone(); items$options=lapply(value, string)
    item = cell$clone(); item$c=items; item
  }
  capture = new.env(parent=emptyenv())
  original = flexplot:::jamovi_plots
  local_mocked_bindings(jamovi_plots=function(...) {
    capture$plot = original(...)
    capture$plot
  })
  grDevices::pdf(file=NULL)
  on.exit(grDevices::dev.off(), add=TRUE)
  for (given in list(NULL, "b")) {
    options = flexplot:::flexplotaOptions$new(out="y", preds="a", given=given,
      center="Mean + sterr", ghost=FALSE, jittx=0, jitty=0)
    pb = group$clone()
    pb$names = c("out", "preds", "center", "given")
    pb$options = list(string("y"), variables("a"), string("Mean + sterr"), variables(given))
    options$fromProtoBuf(pb)
    restored = flexplot:::flexplotaOptions$new()
    restored$fromProtoBuf(options$asProtoBuf())
    analysis = flexplot:::flexplotaClass$new(options=restored, data=small)
    expect_true(analysis$run())
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    plot = capture$plot
    built = ggplot2::ggplot_build(plot)
    expected = aggregate(small$y, small[c("a", given)],
      function(y) c(mean=mean(y), se=sd(y)/sqrt(length(y))))
    errors = built$data[[3]]
    panels = built$layout$layout
    panel = if (is.null(given)) rep(1L, nrow(expected)) else
      panels$PANEL[match(expected[[given]], panels[[given]])]
    key = paste(panel, as.integer(expected$a))
    rows = match(paste(errors$PANEL, round(errors$x)), key)
    expect_false(anyNA(rows))
    expect_equal(errors$ymin, expected$x[rows,"mean"]-expected$x[rows,"se"])
    expect_equal(errors$ymax, expected$x[rows,"mean"]+expected$x[rows,"se"])
    expect_identical(restored$center, "Mean + sterr")
  }
})
