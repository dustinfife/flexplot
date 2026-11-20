# Shared pure R6 fixtures and numerical assertions; no host file access.

jamovi_flexplot_recorder = function(capture) {
  original = flexplot:::jamovi_plots
  function(...) {
    capture$plot = original(...)
    capture$plot
  }
}

jamovi_flexplot_analysis = function(data=small, given=NULL, preds="x", ...) {
  options = flexplot:::flexplotaOptions$new(out="y",preds=preds,given=given,
    suppr=TRUE,ghost=FALSE,jittx=0,jitty=0,...)
  flexplot:::flexplotaClass$new(options=options,data=data)
}

jamovi_flexplot_render = function(analysis,capture) {
  grDevices::pdf(file=NULL)
  on.exit(grDevices::dev.off(),add=TRUE)
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  capture$plot
}

jamovi_flexplot_panel_rows = function(plot,data,given,preds="x",outcome="y") {
  built = ggplot2::ggplot_build(plot)
  displayed = vapply(given,function(name)
    if (is.numeric(data[[name]])) paste0(name,"_binned") else name,character(1))
  expect_identical(as.character(names(plot$facet$params$cols)),unname(displayed[seq_len(min(1L,length(displayed)))]))
  expect_identical(as.character(names(plot$facet$params$rows)),unname(displayed[seq_along(displayed)>1L]))
  eligible = data[complete.cases(data[c(outcome,preds,given)]),,drop=FALSE]
  raw = built$data[[1]]
  source = plot$layers[[1]]$data
  expect_equal(nrow(raw),nrow(eligible))
  expect_equal(sort(raw$y),sort(eligible[[outcome]]))
  for (i in seq_len(nrow(built$layout$layout))) {
    layout = built$layout$layout[i,,drop=FALSE]
    selected = rep(TRUE,nrow(source))
    for (name in displayed) selected = selected & as.character(source[[name]]) == as.character(layout[[name]])
    expect_equal(sort(raw$y[as.character(raw$PANEL)==as.character(layout$PANEL)]),sort(source[[outcome]][selected]))
  }
  expect_s3_class(ggplot2::ggplotGrob(plot),"gtable")
  invisible(built)
}

jamovi_mixed_cas_data = function() {
  data = jamovi_mixed_test_data()[seq_len(120),]
  data$continuous = data$x
  data$group = data$a
  data$mixed = data$y
  data$count = rep(0:5,length.out=nrow(data))
  data
}

jamovi_mixed_cas_resource = function(name, extension) {
  list(rootPath=output_dir,relPath=paste0(name,".",extension))
}

expect_mixed_tables = function(results, model, second=FALSE, correlations=TRUE) {
  fixed = results[[if(second) "fixed2" else "fixed"]]
  random = results[[if(second) "random2" else "random"]]
  correlation = results[[if(second) "correlations2" else "correlations"]]
  expected = summary(model)$coefficients
  values = fixed$asDF
  expect_true(fixed$visible)
  expect_identical(values$term,rownames(expected))
  expect_equal(values$estimate,unname(lme4::fixef(model)),tolerance=1e-10)
  se = sqrt(diag(as.matrix(vcov(model))))
  expect_equal(values$se,unname(se),tolerance=1e-10)
  if (fixed$getColumn("lower")$visible) expect_equal(values$lower,unname(lme4::fixef(model)-qnorm(.975)*se),tolerance=1e-10)
  if (fixed$getColumn("upper")$visible) expect_equal(values$upper,unname(lme4::fixef(model)+qnorm(.975)*se),tolerance=1e-10)
  expect_true(all(is.na(values$p)))
  components = as.data.frame(lme4::VarCorr(model))
  components = components[is.na(components$var2),]
  expected_terms = components$var1
  expected_terms[components$grp=="Residual"] = "Residual"
  expect_true(random$visible)
  expect_identical(random$asDF$group,components$grp)
  expect_identical(random$asDF$term,expected_terms)
  expect_equal(random$asDF$variance,components$vcov,tolerance=1e-10)
  expect_equal(random$asDF$sd,components$sdcor,tolerance=1e-10)
  expected = attr(lme4::VarCorr(model)[[1]],"correlation")
  expect_identical(correlation$visible,correlations && nrow(expected)>1L)
  if (correlation$visible) {
    expect_identical(correlation$asDF$term,rownames(expected))
    visible = Filter(function(column)isTRUE(column$visible),correlation$columns)
    expect_identical(unname(vapply(visible[-1],function(column)column$title,character(1))),colnames(expected))
    expect_equal(unname(as.matrix(correlation$asDF[paste0("c",seq_len(ncol(expected)))])),
      unname(expected),tolerance=1e-10)
  }
}
