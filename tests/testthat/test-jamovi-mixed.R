jamovi_mixed_test_data = function() {
  set.seed(925)
  data = data.frame(id=factor(rep(seq_len(16), each=15)),
    x=rep(seq(-1.5,1.5,length.out=15),16), z=rnorm(240),
    a=ordered(rep(c("low","middle","high"),80),levels=c("middle","low","high")))
  intercept = rnorm(16,sd=.8)
  slope = .25*intercept+rnorm(16,sd=.45)
  data$y = .3+.6*data$x+.2*data$z+.4*(data$a=="high")+.3*data$x*(data$a=="high")+
    .15*data$x^2+intercept[data$id]+slope[data$id]*data$x+rnorm(240,sd=.4)
  data
}

jamovi_mixed_analysis = function(data=jamovi_mixed_test_data(), ...) {
  options = do.call(flexplot:::mixedmodelOptions$new, utils::modifyList(
    list(out="y",preds=c("x","z","a"),cluster="id",graphic=FALSE),list(...),keep.null=TRUE))
  flexplot:::mixedmodelClass$new(options=options,data=data)
}

jamovi_mixed_capture = function(...) {
  capture$plot = original(...)
  capture$plot
}

jamovi_mixed_recorder = function(capture) {
  recorder = jamovi_mixed_capture
  environment(recorder) = list2env(list(capture=capture,original=flexplot:::jamovi_mixed_plot),
    parent=environment(recorder))
  recorder
}

test_that("Mixed Model estimates preserve the requested random structure and estimation method", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  specifications = list(
    list(terms=list("x","z","a"),slopes=NULL,intercept=TRUE,quadratic=NULL,formula=y~x+z+a+(1|id)),
    list(terms=list("x","a",c("x","a")),slopes="x",intercept=TRUE,quadratic=NULL,formula=y~x*a+(1+x|id)),
    list(terms=list("x","z"),slopes="x",intercept=FALSE,quadratic=NULL,formula=y~x+z+(0+x|id)),
    list(terms=list("x","a"),slopes="x",intercept=TRUE,quadratic="x",formula=y~x+a+I(x^2)+(1+x|id)))
  for (specification in specifications) for (estimation in c("REML","ML")) {
    analysis = jamovi_mixed_analysis(data,modelTerms=specification$terms,randomSlopes=specification$slopes,
      randomIntercept=specification$intercept,quadratic=specification$quadratic,estimation=estimation)
    expect_true(analysis$run())
    state = analysis$results$plot$state
    expect_false(is.null(state))
    independent = lme4::lmer(specification$formula,data=data,REML=estimation=="REML")
    expect_identical(lme4::isREML(state$model), estimation=="REML")
    expect_equal(lme4::fixef(state$model),lme4::fixef(independent),tolerance=1e-10)
    expect_equal(as.data.frame(lme4::VarCorr(state$model)),as.data.frame(lme4::VarCorr(independent)),tolerance=1e-10)
    expected = summary(independent)$coefficients
    table = analysis$results$fixed$asDF
    expect_identical(table$term,rownames(expected))
    expect_equal(table$estimate,unname(expected[,1]),tolerance=1e-10)
    expect_equal(table$se,unname(expected[,2]),tolerance=1e-10)
    ci = suppressMessages(stats::confint(independent,method="Wald",parm="beta_"))
    expect_equal(table$lower,unname(ci[,1]),tolerance=1e-10)
    expect_equal(table$upper,unname(ci[,2]),tolerance=1e-10)
    expect_match(analysis$results$fixed$notes$intervals$note,"normal-Wald")
    expect_false("p" %in% names(table))
    random = analysis$results$random$asDF
    components = as.data.frame(lme4::VarCorr(independent))
    expect_equal(random$variance[is.na(components$var2)],components$vcov[is.na(components$var2)])
    expect_equal(random$sd[is.na(components$var2)],components$sdcor[is.na(components$var2)])
    expect_equal(as.numeric(random$correlation[!is.na(components$var2)]),components$sdcor[!is.na(components$var2)])
    expect_true("Residual" %in% random$group)
    expect_match(analysis$results$specification$content,"Fixed:")
    expect_match(analysis$results$specification$content,"Random:")
    expect_identical(state$data$a,data$a)
  }
})

test_that("Mixed Model uses complete cases only for variables needed by its formula", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$y[1] = NA
  data$x[2] = NA
  data$id[3] = NA
  data$z[4] = NA # Selected but unused.
  analysis = jamovi_mixed_analysis(data,modelTerms=list("x","a"),randomSlopes="x",includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$plot$state
  eligible = data[complete.cases(data[c("y","x","a","id")]),]
  independent = lme4::lmer(y~x+a+(1+x|id),eligible)
  expect_identical(rownames(state$data),rownames(eligible))
  expect_identical(state$omitted,3L)
  expect_equal(lme4::fixef(state$model),lme4::fixef(independent))
  expect_true("p" %in% names(analysis$results$fixed$asDF))
  for (row in seq_len(length(lme4::fixef(independent)))) expect_null(analysis$results$fixed$getCell("p",rowNo=row)$value)
  expect_match(analysis$results$fixed$notes$p$note,"unavailable")
})

test_that("Mixed Model rejects invalid specifications and reports singular fits without simplification", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  cases = list(
    list(options=list(randomIntercept=FALSE,randomSlopes=NULL),data=data,error="random intercept or"),
    list(options=list(randomSlopes="z",preds="x"),data=data,error="selected predictor"),
    list(options=list(cluster="y"),data=data,error="different from"),
    list(options=list(),data=transform(data,id=factor("one")),error="at least two"),
    list(options=list(modelTerms=list("x","z")),data=transform(data,z=x),error="rank deficient"),
    list(options=list(quadratic="a"),data=data,error="numeric predictors"))
  for (case in cases) {
    analysis = do.call(jamovi_mixed_analysis,c(list(data=case$data),case$options))
    expect_true(analysis$run())
    expect_null(analysis$results$plot$state)
    expect_match(analysis$results$explanation$.__enclos_env__$private$.error,case$error)
    expect_false(analysis$results$fixed$visible)
    expect_false(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  }
  # Identical within-cluster outcomes leave exactly no between-cluster intercept variation.
  data$y = rep(seq(-1,1,length.out=15)+sin(seq_len(15)),16)
  analysis = jamovi_mixed_analysis(data,modelTerms=list("x"))
  expect_true(analysis$run())
  state = analysis$results$plot$state
  expect_true(lme4::isSingular(state$model))
  expect_true(any(grepl("Singular fit",state$warnings)))
  expect_length(reformulas::findbars(formula(state$model)),1L)
  expect_match(analysis$results$explanation$content,"not been simplified")
})

test_that("actual Mixed Model plot callbacks use only the requested original-model prediction stream", {
  skip_if_not_installed("jmvcore")
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_mixed_plot=jamovi_mixed_recorder(capture))
  data = jamovi_mixed_test_data()
  cases = list(list(x="x",panels="id",group="a",quadratic=NULL),
    list(x="x",panels="a",group="id",quadratic=NULL,intercept=FALSE),
    list(x="a",panels=c("id","z"),group=NULL,quadratic="x"))
  for (case in cases) for (type in c("population","cluster")) {
    analysis = jamovi_mixed_analysis(data,modelTerms=list("x","z","a",c("x","a")),
      quadratic=case$quadratic,randomSlopes="x",randomIntercept=!identical(case$intercept,FALSE),graphic=TRUE,plotX=case$x,
      plotPanels=case$panels,plotGroup=case$group,predictionType=type)
    expect_true(analysis$run())
    image = analysis$results$plot
    state = image$state
    independent = lme4::lmer(formula(state$model),data=state$data)
    set.seed(234)
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(image)))
    plot = capture$plot
    expect_no_warning({built=ggplot2::ggplot_build(plot)})
    expect_match(plot$labels$subtitle,if (type=="population") "Population-level" else "Cluster-specific")
    expect_false(any(vapply(plot$layers,function(layer) inherits(layer$geom,c("GeomSmooth","GeomRibbon")),logical(1))))
    prediction_layers = which(vapply(plot$layers,function(layer) is.data.frame(layer$data) &&
      "prediction" %in% names(layer$data),logical(1)))
    expect_length(prediction_layers,if (case$x == "a") 2L else 1L)
    rows = plot$layers[[prediction_layers[1]]]$data
    expect_equal(length(unique(rows$id)),3L)
    if (type == "cluster") expect_identical(unique(as.character(rows$model)),"random effects")
    expected = vapply(seq_len(nrow(rows)),function(i) {
      profile = state$data
      for (variable in c(case$x,case$panels,case$group)) profile[[variable]] = rows[[variable]][i]
      mean(predict(independent,newdata=profile,re.form=if(type=="population") NA else NULL))
    },numeric(1))
    expect_equal(rows$prediction,expected,tolerance=1e-10)
    for (index in prediction_layers) expect_equal(sort(built$data[[index]]$y),sort(expected),tolerance=1e-10)
    raw = which(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomPoint") &&
      !(is.data.frame(layer$data) && "prediction" %in% names(layer$data)),logical(1)))
    if (length(raw)) expect_equal(sum(vapply(built$data[raw],nrow,integer(1))),45L)
    expect_equal(image$state,state) # No model refit or selection mutation during rendering.
  }
})

test_that("Mixed Model visualization switching and unsupported layouts are explicit", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x","a"),randomSlopes="x")
  expect_true(analysis$run())
  state = analysis$results$plot$state
  plots = list()
  for (type in c("population","cluster")) {
    set.seed(15)
    plots[[type]] = flexplot:::jamovi_mixed_plot(state,"x","id",type=type)
  }
  prediction = lapply(plots,function(plot) Filter(function(layer) is.data.frame(layer$data) &&
    "prediction" %in% names(layer$data),plot$layers)[[1]]$data)
  expect_equal(unname(as.matrix(prediction[[1]][c("x","id")])),unname(as.matrix(prediction[[2]][c("x","id")])))
  expect_true(any(abs(prediction[[1]]$prediction-prediction[[2]]$prediction)>1e-4))
  expect_error(flexplot:::jamovi_mixed_plot(state,"x","a",type="cluster"),"place the grouping variable")
  expect_error(flexplot:::jamovi_mixed_plot(state,"z","id"),"predictors in the fitted model")
  expect_error(flexplot:::jamovi_mixed_plot(state,"id"),"model predictor on x")
  # No fixed terms is a valid random-intercept model; it has no predictor plot.
  analysis = jamovi_mixed_analysis(modelTerms=list())
  expect_true(analysis$run())
  expect_error(flexplot:::jamovi_mixed_plot(analysis$results$plot$state),"intercept-only")
})

test_that("Mixed Model UI compiles and installed automatic handlers preserve native selections", {
  skip_if_not_installed("jmvcore")
  node = Sys.which("node")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  root = normalizePath(test_path("..",".."))
  skip_if(!nzchar(node)||!nzchar(compiler)||!dir.exists(file.path(root,"jamovi")),"UI tools/source unavailable")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  output = tempfile()
  withr::defer(unlink(c(output,paste0(output,".header.R"))))
  log = system2(node,shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,
    if(length(runtime)) runtime[1] else "","mixedmodel")),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  if (!file.exists(paste0(output,".header.R"))) return(invisible(NULL))
  generated = new.env(parent=asNamespace("flexplot"))
  sys.source(paste0(output,".header.R"),generated)
  sys.source(file.path(root,"R","mixedmodel.b.R"),generated)
  options = generated$mixedmodelOptions$new(out="y",preds="x",cluster="id",randomSlopes="x",
    randomIntercept=FALSE,estimation="ML",graphic=FALSE)
  analysis = generated$mixedmodelClass$new(options=options,data=jamovi_mixed_test_data())
  expect_true(analysis$run())
  expect_false(lme4::isREML(analysis$results$plot$state$model))
})

test_that("Mixed Model results return to the waiting state when required selections are removed", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  options = flexplot:::mixedmodelOptions$new(graphic=FALSE)
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$fixed$visible)
  expect_false(analysis$results$random$visible)
  expect_true(jamovi_reporting_update(analysis,list(out="y",preds=c("x","a"),cluster="id")))
  expect_false(analysis$results$waiting$visible)
  expect_true(analysis$results$fixed$visible)
  expect_true(analysis$results$random$visible)
  expect_equal(lme4::fixef(analysis$results$plot$state$model),
    lme4::fixef(lme4::lmer(y~x+a+(1|id),data=data)))
  expect_true(jamovi_reporting_update(analysis,list(cluster=NULL)))
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$fixed$visible)
  expect_null(analysis$results$plot$state)
  expect_equal(analysis$results$fixed$rowCount,0L)
  expect_true(jamovi_reporting_update(analysis,list(cluster="id",includeP=TRUE)))
  expect_true("p" %in% names(analysis$results$fixed$asDF))
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$fixed$visible)
})

test_that("native UI server option batches rerun Mixed Model and clear stale results", {
  skip_if_not_installed("jmvcore")
  node = Sys.which("node")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  root = normalizePath(test_path("..",".."))
  skip_if(!nzchar(node)||!nzchar(compiler)||!length(runtime)||!dir.exists(file.path(root,"jamovi")),
    "Installed UI runtime/compiler unavailable")
  output = tempfile()
  withr::defer(unlink(c(output,paste0(output,".header.R"))))
  log = system2(node,shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime[1],"mixedmodel")),
    stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  if (!file.exists(output)) return(invisible(NULL))
  packets = read.delim(output,stringsAsFactors=FALSE,check.names=FALSE)
  data = jamovi_mixed_test_data()
  data$school = factor(rep(seq_len(4),length.out=nrow(data)))
  options = flexplot:::mixedmodelOptions$new()
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  formulas = list(group=y~1+(1|id),predictors=y~x+a+(1|id),interaction=y~x*a+(1|id),
    slope=y~x*a+(1+x|id),`no-random-intercept`=y~x*a+(0+x|id),ML=y~x*a+(0+x|id),
    squared=y~x*a+I(x^2)+(0+x|id),`restore-intercept`=y~a+(1|id),
    `change-group`=y~a+(1|school),`restore-outcome`=y~a+(1|school),`restore-group`=y~a+(1|id))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_mixed_plot=jamovi_mixed_recorder(capture))
  for (i in seq_len(nrow(packets))) {
    changed = strsplit(packets$changed[i],";",fixed=TRUE)[[1]]
    for (name in changed) {
      value = as.character(packets[[name]][i])
      if (value == "__NULL__") value = NULL else if (name == "modelTerms") {
        value = if (value == "__EMPTY__") list() else strsplit(strsplit(value,";",fixed=TRUE)[[1]],":",fixed=TRUE)
      } else if (name %in% c("preds","quadratic","randomSlopes","plotPanels")) {
        value = if (value == "__EMPTY__") character() else strsplit(value,";",fixed=TRUE)[[1]]
      } else if (name %in% c("randomIntercept","includeP","graphic")) value = value == "true" else
        if (name == "clusters") value = as.integer(value)
      option = options$option(name)
      option$value = value
    }
    analysis$optionsChangedHandler(changed)
    expect_true(analysis$run())
    state = analysis$results$plot$state
    stage = packets$stage[i]
    if (!stage %in% names(formulas)) {
      expect_null(state)
      expect_false(analysis$results$fixed$visible)
      if (stage == "remove-predictor") expect_match(analysis$results$explanation$.__enclos_env__$private$.error,
        "random intercept or at least one random slope") else expect_true(analysis$results$waiting$visible)
      next
    }
    expect_false(is.null(state),info=stage)
    if (is.null(state)) next
    independent = suppressMessages(lme4::lmer(formulas[[stage]],data=data,REML=options$estimation=="REML"))
    expect_equal(lme4::fixef(state$model),lme4::fixef(independent),tolerance=1e-10)
    expect_equal(as.data.frame(lme4::VarCorr(state$model)),as.data.frame(lme4::VarCorr(independent)),tolerance=1e-10)
    expect_identical(lme4::isREML(state$model),options$estimation=="REML")
    if (!stage %in% c("slope","ML","squared","restore-group")) next
    set.seed(103)
    expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
    plot = capture$plot
    curve = Filter(function(layer) is.data.frame(layer$data)&&"prediction" %in% names(layer$data),plot$layers)[[1]]$data
    expected = vapply(seq_len(nrow(curve)),function(row) {
      profile = state$data
      for (variable in c(options$plotX,options$plotPanels,options$plotGroup)) profile[[variable]] = curve[[variable]][row]
      mean(predict(independent,newdata=profile,re.form=NULL))
    },numeric(1))
    expect_equal(curve$prediction,expected,tolerance=1e-10)
  }
})
