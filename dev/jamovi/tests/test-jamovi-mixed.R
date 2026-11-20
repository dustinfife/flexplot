test_that("Mixed Model UI compiles and installed automatic handlers preserve native selections", {
  node = Sys.which("node")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  root = normalizePath(test_path("..","..",".."))
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


test_that("native UI server option batches rerun Mixed Model and clear stale results", {
  node = Sys.which("node")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  root = normalizePath(test_path("..","..",".."))
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
      } else if (name %in% c("preds","quadratic","randomSlopes","plotPanels","comparisonFixed","comparisonQuadratic","comparisonRandomSlopes")) {
        value = if (value == "__EMPTY__") character() else strsplit(value,";",fixed=TRUE)[[1]]
      } else if (name %in% c("randomIntercept","includeP","graphic","interactions","polynomials","compare","comparisonInteractions","comparisonPolynomials","comparisonRandomIntercept")) value = value == "true" else
        if (name == "clusters") value = as.integer(value)
      option = options$option(name)
      option$value = value
    }
    analysis$optionsChangedHandler(changed)
    expect_true(analysis$run())
    state = analysis$.__enclos_env__$private$.modelState
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
    if (stage == "group") {
      # The intercept-only fit is valid before a plot predictor is selected.
      expect_null(analysis$results$plot$state)
      expect_identical(analysis$results$plot$status,"error")
      expect_identical(analysis$results$plot$asProtoBuf(status=3L)$status,4L)
    }
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

test_that("Mixed Model dynamic label rows serialize back through jamovi's options response", {
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

test_that("new Mixed Model term builders default off while explicit saved settings survive", {
  fresh = flexplot:::mixedmodelOptions$new()
  expect_false(fresh$interactions)
  expect_false(fresh$polynomials)
  data = jamovi_mixed_test_data()
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
