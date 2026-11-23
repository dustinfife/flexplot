test_that("stale binomial display combinations complete real images without changing inference", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(862)
  d = data.frame(x=seq(-2,2,length.out=240),z=rnorm(240),a=factor(rep(c("A","B"),120)))
  d$y = rbinom(240,1,plogis(.1+.7*d$x+.3*d$z+.4*(d$a=="B")+.2*d$x*d$z))
  d$z[1:5] = NA_real_
  options = flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z","a"),family="binomial",
    modelTerms=list("x","z","a",c("x","z")),comparisonTerms=list("x","a"),
    quadratic="x",comparisonQuadratic=NULL,plotX="x",comparisonX="x",compare=TRUE,plotScale="logit")
  analysis = jamovi_reporting_host(options,d)
  expect_true(analysis$run())
  original = analysis$results$plot$state
  comparison = analysis$results$comparison$asDF
  independent = glm(y~x*z+a+I(x^2),data=original$data,family=binomial())
  expect_equal(coef(original$model),coef(independent),tolerance=1e-12)
  output_dir = withr::local_tempdir()
  resource = function(name,ext) list(rootPath=output_dir,relPath=paste0(name,"-",sample.int(1e8,1),".",ext))
  analysis$.setResourcesPathSource(resource)
  for (settings in list(list(plotStyle="raw",plotScale="logit"),
      list(plotStyle="auto",plotX="a",comparisonX="a",plotScale="logit"),
      list(plotX="x",comparisonX="x",plotScale="logit"),list(se=FALSE),list(se=TRUE))) {
    expect_true(jamovi_reporting_update(analysis,settings))
    expect_equal(coef(analysis$results$plot$state$model),coef(original$model),tolerance=1e-12)
    expect_equal(analysis$results$comparison$asDF,comparison)
    for (name in c("plot","comparisonplot")) {
      image = analysis$results[[name]]
      expect_true(analysis$.createImage(if (name=="plot") ".plot" else ".comparisonplot",image,noThrow=TRUE))
      expect_identical(image$status,"complete")
      expect_false(analysis$results$waiting$visible)
      path = file.path(output_dir,image$filePath)
      expect_gt(file.info(path)$size,2000)
      expect_identical(as.integer(readBin(path,"raw",8L)),c(137L,80L,78L,71L,13L,10L,26L,10L))
    }
  }
  # Compare the resolved categorical plot with the supported Probability magnet plot.
  state = analysis$results$plot$state
  probability = withr::with_seed(873, ggplot2::ggplot_build(flexplot:::jamovi_glm_generalized_plot(state,x="a",scale="probability")))
  resolved = withr::with_seed(873, ggplot2::ggplot_build(flexplot:::jamovi_glm_generalized_plot(state,x="a",scale="logit")))
  expect_equal(resolved$data,probability$data)
  for (scale in c("probability","logit")) {
    p = flexplot:::jamovi_glm_generalized_plot(state,x="x",scale=scale)
    curve = Filter(function(layer)inherits(layer$geom,"GeomSmooth"),p$layers)[[1]]$data
    independent_curve = t(vapply(curve$x,function(x) {
      profile=state$data;profile$x=x
      probabilities=predict(independent,profile,type="response")
      design=model.matrix(delete.response(terms(independent)),profile,contrasts.arg=independent$contrasts)
      gradient=colMeans(design*as.numeric(probabilities*(1-probabilities)))
      se=sqrt(drop(gradient%*%vcov(independent)%*%gradient))
      mean=mean(probabilities);eta=qlogis(mean)
      radius=qnorm(.975)*se/(mean*(1-mean))
      values=c(eta,eta-radius,eta+radius)
      if(scale=="probability") plogis(values) else values
    },numeric(3)))
    expect_equal(unname(as.matrix(curve[,c("prediction","ymin","ymax")])),unname(independent_curve),tolerance=1e-10)
  }
})

test_that("plot errors render a diagnostic with an error state and can recover", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),family="binomial",plotX="x"),small)
  expect_true(analysis$run())
  output_dir = withr::local_tempdir()
  analysis$.setResourcesPathSource(function(name,ext) list(rootPath=output_dir,relPath=paste0(name,".",ext)))
  original = flexplot:::jamovi_glm_generalized_plot
  fail = TRUE
  local_mocked_bindings(jamovi_glm_generalized_plot=function(...) {
    if (fail) stop("Diagnostic probe")
    original(...)
  },.package="flexplot")
  expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
  expect_identical(analysis$results$plot$status,"error")
  expect_match(analysis$results$plot$.__enclos_env__$private$.error,"Diagnostic probe")
  expect_gt(file.info(file.path(output_dir,analysis$results$plot$filePath))$size,1000)
  fail = FALSE
  expect_true(jamovi_reporting_update(analysis,list(se=FALSE)))
  expect_null(analysis$results$plot$filePath)
  expect_true(analysis$.createImage(".plot",analysis$results$plot,noThrow=TRUE))
  expect_identical(analysis$results$plot$status,"complete")
})

test_that("GLM table help and plot explanations are concise and mode-specific", {
  skip_if_not_installed("jmvcore")
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome = switch(family,gaussian="y",binomial="y_bin",Gamma="y_gam",gammaLog="y_gam","y_pois")
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=outcome,preds=c("x","a"),family=family,plotX="x",plotPanels="a"),small)
    expect_true(analysis$run())
    model = analysis$results$plot$state$model
    notes = analysis$results$coefficients$notes
    expect_identical(notes$model$note,paste0("Family: ",model$family$family,"; Link: ",model$family$link,if (family=="binomial") ". Success event: 1" else ""))
    expect_false(grepl("estimates.glm",paste(vapply(notes,function(note)note$note,character(1)),collapse=" "),fixed=TRUE))
    if (family %in% c("binomial","poisson","negativebinomial")) expect_null(notes$intervals)
    if (family=="gaussian") expect_null(notes$uncertainty) else expect_match(notes$uncertainty$note,"SE")
    state = analysis$results$plot$state
    if (family=="binomial") {
      expect_match(flexplot:::jamovi_glm_plot_guide(state,"x",panels="a"),"success proportions")
      expect_match(flexplot:::jamovi_glm_plot_guide(state,"x",panels="a",scale="logit"),"observed-bin logits")
      expect_match(flexplot:::jamovi_glm_plot_guide(state,"x",panels="a",style="raw",scale="logit"),"Probability scale is used")
      expect_false(grepl("observed-bin",flexplot:::jamovi_glm_plot_guide(state,"x",panels="a",style="raw")))
      expect_match(notes$interpretation$note,"reciprocal")
      expect_match(notes$model$note,"Success event")
    } else expect_identical(flexplot:::jamovi_glm_plot_guide(state,"x",panels="a"),"")
  }
})


test_that("comparison explanations disappear on live removal", {
  skip_if_not_installed("jmvcore")
  options = flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","z","a"),family="binomial",
    compare=TRUE,plotX="x",comparisonX="x",plotScale="logit",comparisonPlotScale="logit",se=TRUE)
  analysis = jamovi_reporting_host(options,small)
  expect_true(analysis$run())
  expect_match(analysis$results$comparisonPlotExplanation$content,"logits")
  expect_true(analysis$results$comparisonPlotExplanation$visible)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_false(analysis$results$comparisonPlotExplanation$visible)
  expect_identical(analysis$results$comparisonPlotExplanation$content,"")
  expect_true(jamovi_reporting_update(analysis,list(graphic=FALSE)))
  expect_false(analysis$results$plotExplanation$visible)
  expect_identical(analysis$results$plotExplanation$content,"")
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_null(analysis$results$plot$filePath)
  expect_null(analysis$results$plot$state)
})
