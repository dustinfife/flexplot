test_that("source metadata reports actual prepared reference labels without changing plot data", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  model = lm(y~x*a+z+I(x^2),data)
  for (panels in list("a",c("a","z"))) {
    options = flexplot:::glmbasicOptions$new(plotX="x",plotPanels=panels)
    base = flexplot:::jamovi_lm_display(model,data,options)
    formula = flexplot:::jamovi_model_layout(list(model),data,"x",panels)
    references = list(NULL,list(a="high"))
    if(length(panels)==2L) references = c(references,list(list(a="high",z=levels(base$data$z_binned)[1])))
    for (reference in references) {
      plot = suppressMessages(flexplot:::jamovi_model_reference_ghosts(base,formula,TRUE,reference))
      metadata = attr(plot,"ghost_reference")
      expect_true(metadata$drawable)
      expect_identical(metadata$variables,panels)
      expect_identical(metadata$style,list(colour="#707070",linewidth=1.2,alpha=.85,linetype="dashed"))
      resolved = suppressMessages(flexplot:::create_ghost_reference(data=base$data,given=panels,axis="x",ghost.reference=reference))
      curve = Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),base$layers)[[1]]$data
      source = flexplot:::flexplot_ghost_reference_data(curve,panels,resolved,"x")
      expect_identical(metadata$levels,lapply(metadata$columns,function(name)unique(as.character(source[[name]]))))
      note = flexplot:::jamovi_model_ghost_note(plot)
      expect_match(note,"from the panel where ",fixed=TRUE)
      if(length(panels)==2L) expect_match(note," and ",fixed=TRUE)
      for (value in unlist(metadata$levels)) expect_true(grepl(flexplot:::jamovi_html_escape(value),note,fixed=TRUE))
      edits = list(list(role="columnTitle",variable="a",value="",label="Treatment <arm>"),
        list(role="facetLevel",variable="a",value=metadata$levels[[1]],label="Control & reference"))
      if(length(panels)==2L) edits = c(edits,list(
        list(role="rowTitle",variable=metadata$columns[2],value="",label="Age"),
        list(role="facetLevel",variable=metadata$columns[2],value=metadata$levels[[2]],label="Middle")))
      labeled = flexplot:::jamovi_plot_apply_labels(plot,edits)
      if(length(panels)==2L) expect_match(flexplot:::jamovi_model_ghost_note(labeled),"Age = Middle",fixed=TRUE)
      expect_match(flexplot:::jamovi_model_ghost_note(labeled),"Treatment &lt;arm&gt; = Control &amp; reference",fixed=TRUE)
      expect_identical(lapply(labeled$layers,function(layer)layer$data),lapply(plot$layers,function(layer)layer$data))
      expect_match(note,'style="margin:0 0 4px 0;"',fixed=TRUE)
    }
    expect_identical(flexplot:::jamovi_model_ghost_note(base),"")
    invalid = flexplot:::jamovi_model_reference_ghosts(base,formula,TRUE,list(a="absent"))
    expect_match(flexplot:::jamovi_model_ghost_note(invalid),"Ghost lines unavailable",fixed=TRUE)
    expect_false(grepl("Dashed gray",flexplot:::jamovi_model_ghost_note(invalid),fixed=TRUE))
    # Reusing the unchanged base must not retain an earlier failure or metadata.
    valid = flexplot:::jamovi_model_reference_ghosts(base,formula,TRUE)
    expect_match(flexplot:::jamovi_model_ghost_note(valid),"Dashed gray",fixed=TRUE)
    expect_null(attr(base,"ghost_reference"))
  }
})

test_that("drawable success requires two finite coordinates in the same path", {
  data = data.frame(x=c(1,2),y=c(2,3),panel=c("a","b"),group=c(1,2))
  attr(data,"ghost_axes") = c("x","y")
  attr(data,"ghost_group") = "group"
  expect_false(flexplot:::flexplot_ghost_drawable(data,"panel"))
  data = rbind(data,transform(data,x=x+1))
  expect_true(flexplot:::flexplot_ghost_drawable(data,"panel"))
  data$y = NA_real_
  expect_false(flexplot:::flexplot_ghost_drawable(data,"panel"))
})

test_that("GLM family and logit paths carry the same resolved source metadata", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$binary = as.numeric(data$y>median(data$y))
  withr::local_seed(917)
  for (family in c("binomial","gaussian","poisson")) {
    data$count = rpois(nrow(data),exp(.2+.3*data$x))
    options = flexplot:::glmgeneralizedOptions$new(out=switch(family,binomial="binary",gaussian="y","count"),
      preds=c("x","a","z"),family=family)
    state = flexplot:::jamovi_glm_generalized_fit(options,data)
    for (scale in if(family=="binomial")c("probability","logit") else "probability") {
      plot = suppressMessages(flexplot:::jamovi_glm_generalized_plot(state,x="x",panels=c("a","z"),
        ghostLines=TRUE,scale=scale,ghost.reference=list(a="high")))
      metadata = attr(plot,"ghost_reference")
      expect_true(metadata$drawable)
      expect_identical(metadata$levels[[1]],"high")
      expect_match(flexplot:::jamovi_model_ghost_note(plot),"a = high",fixed=TRUE)
    }
  }
})
