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
      randomIntercept=specification$intercept,quadratic=specification$quadratic,estimation=estimation,
      interactions=any(lengths(specification$terms)>1L),polynomials=length(specification$quadratic)>0L)
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
    expect_named(random,c("group","term","variance","sd"))
    expect_equal(random$variance,components$vcov[is.na(components$var2)])
    expect_equal(random$sd,components$sdcor[is.na(components$var2)])
    expect_false(analysis$results$correlations$visible)
    expect_true("Residual" %in% random$group)
    content = analysis$results$specification$content
    expect_match(content,flexplot:::jamovi_mixed_escape(paste(deparse(state$specification$formula),collapse=" ")),fixed=TRUE)
    expect_match(content,paste0("(",estimation,")"),fixed=TRUE)
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
    list(options=list(quadratic="a",polynomials=TRUE),data=data,error="numeric predictors"))
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
  expect_true(state$singular)
  expect_false(any(grepl("boundary.*singular",state$warnings)))
  expect_length(reformulas::findbars(formula(state$model)),1L)
  expect_false(grepl("not been automatically simplified",analysis$results$diagnostics$content))
  expect_false(grepl("singular",analysis$results$explanation$content,ignore.case=TRUE))
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
    analysis = jamovi_mixed_analysis(data,modelTerms=list("x","z","a",c("x","a")),interactions=TRUE,polynomials=TRUE,
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
  formulas = list(group=y~1+(1|id),predictors=y~x+a+(1|id),`allow-interactions`=y~x+a+(1|id),interaction=y~x*a+(1|id),
    slope=y~x*a+(1+x|id),`no-random-intercept`=y~x*a+(0+x|id),ML=y~x*a+(0+x|id),
    `allow-polynomials`=y~x*a+(0+x|id),squared=y~x*a+I(x^2)+(0+x|id),
    `disable-interactions`=y~x+a+I(x^2)+(0+x|id),`enable-interactions`=y~x+a+I(x^2)+(0+x|id),
    `disable-polynomials`=y~x+a+(0+x|id),`enable-polynomials`=y~x+a+(0+x|id),`restore-intercept`=y~a+(1|id),
    `change-group`=y~a+(1|school),`restore-outcome`=y~a+(1|school),`restore-group`=y~a+(1|id))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_mixed_plot=jamovi_mixed_recorder(capture))
  for (i in seq_len(nrow(packets))) {
    changed = strsplit(packets$changed[i],";",fixed=TRUE)[[1]]
    for (name in changed) {
      value = as.character(packets[[name]][i])
      if (value == "__NULL__") value = NULL else if (name %in% c("modelTerms","comparisonTerms")) {
        value = if (value == "__EMPTY__") list() else strsplit(strsplit(value,";",fixed=TRUE)[[1]],":",fixed=TRUE)
      } else if (name %in% c("preds","quadratic","randomSlopes","plotPanels","comparisonQuadratic","comparisonRandomSlopes")) {
        value = if (value == "__EMPTY__") character() else strsplit(value,";",fixed=TRUE)[[1]]
      } else if (name %in% c("randomIntercept","includeP","graphic","interactions","polynomials","compare","comparisonInteractions","comparisonPolynomials","comparisonRandomIntercept")) value = value == "true" else
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

test_that("Mixed Model term switches remove terms in R and never recreate them on enabling", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a"),cluster="id",
    modelTerms=list("x","a",c("x","a")),quadratic="x",graphic=FALSE,interactions=TRUE,polynomials=TRUE)
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_true(jamovi_reporting_update(analysis,list(interactions=FALSE)))
  independent = lme4::lmer(y~x+a+I(x^2)+(1|id),data)
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(independent),tolerance=1e-10)
  expect_true(jamovi_reporting_update(analysis,list(polynomials=FALSE)))
  independent = lme4::lmer(y~x+a+(1|id),data)
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(independent),tolerance=1e-10)
  # Synchronized saved options have removed the selections, not merely hidden them.
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x","a"),quadratic=character(),
    interactions=TRUE,polynomials=TRUE)))
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(independent),tolerance=1e-10)
})

test_that("Mixed Model CAS delegates adjustment and model curves to the fitted-model CAS implementation", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x","a",c("x","a")),randomSlopes="x",interactions=TRUE,
    graphic=TRUE,predictionType="cas",plotX="x",plotPanels="a")
  expect_true(analysis$run())
  state = analysis$results$plot$state
  original = state$model
  plot = flexplot:::jamovi_mixed_plot(state,"x","a",type="cas")
  expected = cluster_adjusted_scatter(y~x|a,object=original,adjust_re="all",smooth=FALSE)
  expect_equal(plot$data,expected$data)
  expected_y = model.response(model.frame(original)) -
    (predict(original,re.form=NULL)-predict(original,re.form=NA))
  expect_equal(plot$data$y_adj,unname(expected_y),ignore_attr=TRUE)
  set.seed(945)
  actual_layers = ggplot2::ggplot_build(plot)$data
  set.seed(945)
  expect_equal(actual_layers,ggplot2::ggplot_build(expected)$data)
  expect_match(plot$labels$subtitle,"Cluster-adjusted")
  expect_false(any(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomSmooth"),logical(1))))
  expect_match(analysis$results$plotExplanation$content,"All eligible fitted observations")
  expect_identical(state$model,original)
  # Exercise the actual rendering callback in the CAS mode.
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_mixed_plot=jamovi_mixed_recorder(capture))
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_equal(capture$plot$data$y_adj,plot$data$y_adj)
  # Existing CAS errors are propagated, rather than replaced by a smoother.
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","z"),cluster="id",quadratic="x",polynomials=TRUE)
  quadratic = flexplot:::jamovi_mixed_fit(options,jamovi_mixed_test_data())
  quadratic_plot = flexplot:::jamovi_mixed_plot(quadratic,"x",type="cas")
  direct = cluster_adjusted_scatter(y~x,object=quadratic$model,adjust_re="all")
  expect_equal(quadratic_plot$data,direct$data)
  quadratic_curve = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),quadratic_plot$layers)[[1]]$data
  expected_curve = vapply(quadratic_curve$x,function(x) {
    profile = quadratic$data
    profile$x = x
    mean(predict(quadratic$model,newdata=profile,re.form=NA))
  },numeric(1))
  expect_equal(quadratic_curve$prediction,expected_curve,tolerance=1e-10)
  offset = quadratic
  offset$model = lme4::lmer(y~x+offset(z)+(1|id),data=quadratic$data)
  expect_error(flexplot:::jamovi_mixed_plot(offset,"x",type="cas"),"original fixed-effect variables")
})

test_that("Mixed Model tables separate correlations and keep citations out of table footnotes", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x","a"),randomSlopes=c("x","a"),showCorrelations=TRUE)
  expect_true(analysis$run())
  results = analysis$results
  expect_length(results$fixed$getRefs(),0)
  expect_length(results$random$getRefs(),0)
  expect_length(results$correlations$getRefs(),0)
  expect_true(all(c("flexplot","graphicalAnalysis") %in% results$getRefs(recurse=TRUE)))
  expect_named(results$random$asDF,c("group","term","variance","sd"))
  expected = attr(lme4::VarCorr(results$plot$state$model)[[1]],"correlation")
  actual = results$correlations$asDF
  expect_true(results$correlations$visible)
  expect_identical(actual$term,rownames(expected))
  expect_equal(unname(as.matrix(actual[-1])),unname(expected),tolerance=1e-12)
  expect_equal(diag(as.matrix(actual[-1])),rep(1,nrow(expected)))
  expect_equal(unname(as.matrix(actual[-1])),unname(t(as.matrix(actual[-1]))))
  expect_identical(vapply(results$correlations$columns[-1],function(column) column$title,character(1)),
    stats::setNames(colnames(expected),names(results$correlations$columns)[-1]))
  # Switching structures cannot leave old matrix columns or residual rows as correlations.
  options = analysis$options
  option = options$option("randomSlopes"); option$value = "x"
  analysis$optionsChangedHandler("randomSlopes")
  expect_true(analysis$run())
  expect_equal(ncol(results$correlations$asDF),3L)
  option$value = character()
  analysis$optionsChangedHandler("randomSlopes")
  expect_true(analysis$run())
  expect_false(results$correlations$visible)
  expect_equal(results$correlations$rowCount,0L)
  expect_true("Residual" %in% results$random$asDF$group)
  slope_only = jamovi_mixed_analysis(modelTerms=list("x"),randomSlopes="x",randomIntercept=FALSE,showCorrelations=TRUE)
  expect_true(slope_only$run())
  expect_false(slope_only$results$correlations$visible)
})

test_that("Mixed Model diagnostics precede estimates and distinguish singularity from convergence", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x"))
  expect_true(analysis$run())
  order = vapply(analysis$results$items,function(item) item$name,character(1))
  expect_true(match("specification",order)<match("diagnostics",order))
  expect_true(match("diagnostics",order)<match("fixed",order))
  state = analysis$results$plot$state
  state$singular = TRUE
  state$warnings = "Model failed to converge with max|grad| = 0.03"
  flexplot:::jamovi_mixed_reset(analysis$results)
  flexplot:::jamovi_mixed_report(analysis$results,state,FALSE)
  message = analysis$results$diagnostics$content
  expect_true(analysis$results$diagnostics$visible)
  expect_match(message,"rank-deficient")
  expect_false(grepl("not been automatically simplified",message))
  expect_match(message,"failed to converge")
  expect_length(gregexpr("Singular fit detected",message,fixed=TRUE)[[1]],1)
  expect_false(grepl("boundary.*singular",message))
  expect_false(grepl("failed to converge",analysis$results$explanation$content))
})

jamovi_mixed_grob_text = function(grob) {
  text = if (is.character(grob$label)) grob$label else character()
  children = c(grob$grobs,as.list(grob$children))
  for (child in children) text = c(text,jamovi_mixed_grob_text(child))
  text
}

test_that("Mixed Model plot-label edits preserve coordinates, fitted model and inactive labels", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  for (type in c("population","cluster","cas")) {
    set.seed(153)
    options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",
      randomSlopes="x",graphic=TRUE,predictionType=type,plotX="x",plotPanels=c("id","z"),plotGroup="a")
    analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader)
    analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run())
    state = analysis$results$plot$state
    original = state$displayPlot
    catalog = options$plotLabels
    roles = vapply(catalog,`[[`,character(1),"role")
    expect_true(all(c("x","y","legend","legendLevel","rowTitle","columnTitle","facetLevel") %in% roles))
    # Edit each title and one level of each kind, leaving remaining levels automatic.
    for (role in unique(roles)) {
      index = match(role,roles)
      catalog[[index]]$label = paste0("Custom ",role)
    }
    option = options$option("plotLabels");option$value = catalog
    analysis$optionsChangedHandler("plotLabels")
    before_rng = .Random.seed
    expect_true(analysis$run())
    expect_identical(.Random.seed,before_rng)
    expect_identical(analysis$results$plot$state$model,state$model)
    expect_identical(analysis$results$plot$state$displayPlot,original)
    labeled = flexplot:::jamovi_mixed_apply_labels(original,options$plotLabels)
    expect_equal(labeled$labels$x,"Custom x")
    expect_equal(labeled$labels$y,"Custom y")
    expect_equal(labeled$labels$colour,"Custom legend")
    set.seed(39);before = ggplot2::ggplot_build(original)
    set.seed(39);after = ggplot2::ggplot_build(labeled)
    expect_equal(after$data,before$data)
    expect_equal(after$layout$layout,before$layout$layout)
    expect_equal(after$layout$panel_params[[1]]$x.range,before$layout$panel_params[[1]]$x.range)
    scale = after$plot$scales$get_scales("colour")
    expect_true("Custom legendLevel" %in% scale$get_labels())
    labels = labeled$facet$params$labeller(after$layout$layout[names(labeled$facet$params$rows)])
    expect_true(any(grepl("Custom rowTitle",unlist(labels))))
    grob = expect_no_warning(ggplot2::ggplotGrob(labeled))
    rendered = jamovi_mixed_grob_text(grob)
    expect_true(all(c("Custom x","Custom y","Custom legend","Custom legendLevel") %in% rendered))
    expect_true(any(grepl("Custom rowTitle",rendered)))
    expect_true(any(grepl("Custom columnTitle",rendered)))
    # Changes unrelated to plot variables retain custom entries and selected clusters.
    expect_true(jamovi_reporting_update(analysis,list(includeP=TRUE)))
    expect_identical(options$plotLabels,catalog)
    # Removing a role hides its editor rows; its labels survive reintroduction.
    expect_true(jamovi_reporting_update(analysis,list(plotGroup=NULL)))
    expect_false(any(vapply(options$plotLabels,`[[`,character(1),"role") == "legend"))
    expect_true(jamovi_reporting_update(analysis,list(plotGroup="a")))
    restored = Filter(function(entry) entry$role == "legend",options$plotLabels)[[1]]
    expect_equal(restored$label,"Custom legend")
    # Saved-analysis options restored to a new R6 instance retain the label store.
    saved = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a","z"),cluster="id",
      randomSlopes="x",graphic=TRUE,predictionType=type,plotX="x",plotPanels=c("id","z"),plotGroup="a",
      plotLabels=options$plotLabels,plotLabelStore=options$plotLabelStore)
    reopened = flexplot:::mixedmodelClass$new(options=saved,data=data)
    expect_true(reopened$run())
    expect_equal(Filter(function(entry) entry$role == "legend",saved$plotLabels)[[1]]$label,"Custom legend")
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  }
})

test_that("Mixed Model dynamic label rows serialize back through jamovi's options response", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  # Minimal real host request, using the installed protocol definitions.
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  empty = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  empty$o = 2L
  pb$names = c("plotLabels","plotLabelStore")
  pb$options = list(empty,empty)
  options = flexplot:::mixedmodelOptions$new()
  options$fromProtoBuf(pb)
  row = flexplot:::jamovi_mixed_label_row("x","x",display="X-axis label")
  row$label = "Length (cm)"
  flexplot:::jamovi_mixed_publish_labels(options,"plotLabels",list(row))
  restored = flexplot:::mixedmodelOptions$new()
  restored$fromProtoBuf(options$asProtoBuf())
  expect_equal(restored$plotLabels,list(row))
  expect_null(restored$plotLabelStore)
  legacy = pb$clone(); legacy$names = "out"
  scalar = empty$clone(); scalar$clear(); scalar$s = "y"
  legacy$options = list(scalar)
  options$fromProtoBuf(legacy)
  flexplot:::jamovi_mixed_publish_labels(options,"plotLabels",list(row))
  restored$fromProtoBuf(options$asProtoBuf())
  expect_equal(restored$plotLabels,list(row))
  expect_equal(restored$out,"y")
})

test_that("Mixed Model categorical-axis labels keep automatic defaults and only applicable controls", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("a"),graphic=TRUE,predictionType="population",plotX="a")
  expect_true(analysis$run())
  original = analysis$results$plot$state$displayPlot
  catalog = analysis$options$plotLabels
  expect_identical(vapply(catalog,`[[`,character(1),"role"),c("x","y"))
  expect_true(nzchar(catalog[[2]]$variable))
  expect_identical(flexplot:::jamovi_mixed_apply_labels(original,catalog),original)
  catalog[[1]]$label = "Treatment category"
  option = analysis$options$option("plotLabels");option$value = catalog
  analysis$optionsChangedHandler("plotLabels")
  expect_true(analysis$run())
  labeled = flexplot:::jamovi_mixed_apply_labels(analysis$results$plot$state$displayPlot,analysis$options$plotLabels)
  set.seed(90); before = ggplot2::ggplot_build(original)$data
  set.seed(90); after = ggplot2::ggplot_build(labeled)$data
  expect_equal(after,before)
  expect_true("Treatment category" %in% jamovi_mixed_grob_text(ggplot2::ggplotGrob(labeled)))
  catalog[[1]]$label = ""
  option$value = catalog
  analysis$optionsChangedHandler("plotLabels")
  expect_true(analysis$run())
  expect_identical(flexplot:::jamovi_mixed_apply_labels(analysis$results$plot$state$displayPlot,analysis$options$plotLabels),
    analysis$results$plot$state$displayPlot)
})

test_that("new Mixed Model term builders default off while explicit saved settings survive", {
  skip_if_not_installed("jmvcore")
  fresh = flexplot:::mixedmodelOptions$new()
  expect_false(fresh$interactions)
  expect_false(fresh$polynomials)
  data = jamovi_mixed_test_data()
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  # Restore the explicit saved settings through jamovi's actual options protocol.
  for (enabled in c(TRUE,FALSE)) {
    options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","a"),cluster="id",
      modelTerms=list("x","a",c("x","a")),quadratic="x",graphic=FALSE)
    pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    flag = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    flag$o = as.integer(enabled)
    pb$names = c("interactions","polynomials")
    pb$options = list(flag,flag)
    options$fromProtoBuf(pb)
    analysis = flexplot:::mixedmodelClass$new(options=options,data=data)
    expect_true(analysis$run())
    formula = if (enabled) y~x*a+I(x^2)+(1|id) else y~x+a+(1|id)
    independent = lme4::lmer(formula,data)
    expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(independent),tolerance=1e-10)
    expect_identical(options$interactions,enabled)
    expect_identical(options$polynomials,enabled)
  }
})
