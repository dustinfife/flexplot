# Observe the real adapter result while the actual R6 callback still renders it.




test_that("actual Flexplot callbacks preserve zero, one and two conditioning roles", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  data = small
  data$w = seq_len(nrow(data))
  for (given in list(NULL,"a","z",c("a","b"),c("a","z"),c("z","a"),c("z","w"),c("b","a"))) {
    analysis = jamovi_flexplot_analysis(data,given)
    expect_true(analysis$run())
    expect_identical(flexplot_axis_given(analysis$results$plot$state$formula)$axis,"x")
    expect_identical(all.vars(analysis$results$plot$state$formula),c("y","x",given))
    expect_no_warning({plot = jamovi_flexplot_render(analysis,capture)})
    jamovi_flexplot_panel_rows(plot,data,given)
  }
})

test_that("two conditioning variables retain missing-data eligibility and group colors", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  data = small
  data$y[2] = NA
  data$x[4] = NA
  data$a[6] = NA
  data$z[8] = NA
  data$unused = NA_real_
  analysis = jamovi_flexplot_analysis(data,c("a","z"),preds=c("x","b"))
  expect_true(analysis$run())
  expect_no_warning({plot = jamovi_flexplot_render(analysis,capture)})
  built = jamovi_flexplot_panel_rows(plot,data,c("a","z"),preds=c("x","b"))
  source = plot$layers[[1]]$data
  expect_equal(built$data[[1]]$colour,built$plot$scales$get_scales("colour")$map(source$b))
  expect_setequal(unique(source$b),levels(droplevels(data$b)))
  expect_identical(ggplot2::get_labs(plot)$colour,"b")
  grob = ggplot2::ggplotGrob(plot)
  guides = grob$grobs[grepl("^guide-box",grob$layout$name)]
  expect_true(any(vapply(guides,inherits,logical(1),"gtable")))
})

jamovi_flexplot_strip_text = function(grob) {
  if (inherits(grob,"text")) return(as.character(grob$label))
  unlist(lapply(c(grob$children,grob$grobs),jamovi_flexplot_strip_text),use.names=FALSE)
}

test_that("actual callback preserves custom names and ordered facet labels", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  data = small
  data$a = ordered(data$a,levels=c("b","a"),labels=c("First custom","Second custom"))
  data$b = ordered(data$b,levels=c("z","x","y"),labels=c("Group Z","Group X","Group Y"))
  names(data)[match(c("y","x","a","b"),names(data))] = c("Observed response","Numeric predictor","Column panel","Row panel")
  options = flexplot:::flexplotaOptions$new(out="Observed response",preds="Numeric predictor",
    given=c("Column panel","Row panel"),suppr=TRUE,ghost=FALSE,jittx=0,jitty=0)
  analysis = flexplot:::flexplotaClass$new(options=options,data=data)
  expect_true(analysis$run())
  plot = jamovi_flexplot_render(analysis,capture)
  built = jamovi_flexplot_panel_rows(plot,data,c("Column panel","Row panel"),
    preds="Numeric predictor",outcome="Observed response")
  expect_identical(levels(built$layout$layout[["Column panel"]]),levels(data[["Column panel"]]))
  expect_identical(levels(built$layout$layout[["Row panel"]]),levels(data[["Row panel"]]))
  grob = ggplot2::ggplotGrob(plot)
  labels = unlist(lapply(grob$grobs[grepl("^strip",grob$layout$name)],jamovi_flexplot_strip_text))
  for (label in c("Column panel","Row panel","First custom","Second custom","Group Z","Group X","Group Y")) {
    expect_true(any(grepl(label,labels,fixed=TRUE)),info=label)
  }
})

test_that("live Flexplot option changes replace facets without stale panel variables", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  data = small
  options = flexplot:::flexplotaOptions$new(out="y",preds="x",given=c("a","b"),
    suppr=TRUE,ghost=FALSE,jittx=0,jitty=0)
  analysis = flexplot:::flexplotaClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  for (given in list(c("a","b"),c("b","a"),"a",c("a","z"),NULL)) {
    expect_true(jamovi_reporting_update(analysis,list(given=given)))
    plot = jamovi_flexplot_render(analysis,capture)
    jamovi_flexplot_panel_rows(plot,data,given)
    expect_identical(all.vars(analysis$results$plot$state$formula),c("y","x",given))
  }
})

test_that("the callback keeps the existing two-panel ghost route", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_plots=jamovi_flexplot_recorder(capture))
  options = flexplot:::flexplotaOptions$new(out="y",preds="x",given=c("a","b"),
    line="Regression",se=FALSE,ghost=TRUE,suppr=TRUE,jittx=0,jitty=0)
  analysis = flexplot:::flexplotaClass$new(options=options,data=small)
  expect_true(analysis$run())
  expect_no_warning({plot = jamovi_flexplot_render(analysis,capture)})
  jamovi_flexplot_panel_rows(plot,small,c("a","b"))
  ghosts = Filter(function(layer) inherits(layer$geom,"GeomLine") &&
    identical(layer$aes_params$colour,"black"),plot$layers)
  expect_gt(length(ghosts),0L)
})
