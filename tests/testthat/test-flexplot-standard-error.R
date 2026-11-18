test_that("standard-error whiskers use nonmissing group counts, including sparse cells", {
  values = list(singleton=7, small=c(2, 4), constant=rep(8, 4),
                large=seq(-5, 5, length.out=101))
  data = data.frame(y=unlist(values, use.names=FALSE),
    x=factor(rep(names(values), lengths(values)), levels=rev(names(values))))
  data = rbind(data, data.frame(y=NA_real_, x="small"),
    data.frame(y=99, x=NA_character_))
  expected = lapply(values, function(y) c(mean=mean(y), se=sd(y)/sqrt(length(y))))
  for (sample in c(Inf, 1)) {
    plot = withr::with_seed(64, flexplot(y ~ x, data, spread="sterr",
      jitter=FALSE, sample=sample))
    built = ggplot2::ggplot_build(plot)
    categories = built$layout$panel_scales_x[[1]]$get_limits()
    points = built$data[[2]]
    errors = built$data[[3]]
    rows = categories[round(points$x)]
    means = vapply(expected[rows], function(z) z[["mean"]], numeric(1))
    ses = vapply(expected[rows], function(z) z[["se"]], numeric(1))
    expect_equal(points$y, unname(means))
    expect_equal(errors$ymin, unname(means-ses))
    expect_equal(errors$ymax, unname(means+ses))
    expect_true(is.na(errors$ymin[rows=="singleton"]))
    expect_equal(errors$ymin[rows=="constant"], 8)
    expect_equal(errors$ymax[rows=="constant"], 8)
    # Undefined whiskers are omitted at drawing time; singleton means remain.
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("Mean + sterr reaches the actual jamovi callback and survives option restoration", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(), "/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
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
