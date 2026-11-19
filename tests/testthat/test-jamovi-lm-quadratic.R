lm_quadratic_analysis = function(data=small, ...) {
  options = do.call(flexplot:::glmbasicOptions$new,utils::modifyList(
    list(out="y",preds=c("x","z","a"),graphic=FALSE),list(...),keep.null=TRUE))
  flexplot:::glmbasicClass$new(options=options,data=data)
}

test_that("LM quadratic formulas fit their exact model spaces and preserve supplementary statistics", {
  skip_if_not_installed("jmvcore")
  cases = list(
    list(terms=list("x"),squares="x",formula=y~x+I(x^2)),
    list(terms=list(),squares="x",formula=y~I(x^2)),
    list(terms=list("x","z"),squares=c("x","z"),formula=y~x+z+I(x^2)+I(z^2)),
    list(terms=list("x","a",c("x","a")),squares="x",formula=y~x*a+I(x^2)))
  for (case in cases) {
    analysis = lm_quadratic_analysis(modelTerms=case$terms,polynomials=TRUE,quadratic=case$squares)
    expect_true(analysis$run())
    expected = lm(case$formula,small)
    jamovi_expect_lm_coefficients(analysis,expected)
    actual = lm(analysis$results$plot$state$formula,analysis$results$plot$state$data)
    expect_identical(colnames(model.matrix(actual)),colnames(model.matrix(expected)))
    expect_equal(predict(actual),predict(expected),tolerance=1e-12)
    expect_null(analysis$results$glmcat$notes$summaries)
    contributions = compute_semi_partial(expected)
    table = analysis$results$rsq$asDF
    expect_equal(table$Estimate[match(names(contributions),table$var)],as.numeric(contributions),tolerance=1e-12)
  }
  analysis = lm_quadratic_analysis(modelTerms=list("x","z"),polynomials=TRUE,quadratic="x")
  expect_true(analysis$run())
  full = lm(y~x+z+I(x^2),small)
  table = analysis$results$rsq$asDF
  expect_equal(table$Estimate[table$var=="x (combined polynomial)"],
    summary(full)$r.squared-summary(lm(y~z,small))$r.squared,tolerance=1e-12)
  expect_equal(table$Estimate[table$var=="I(x^2) (incremental curvature)"],
    summary(full)$r.squared-summary(lm(y~x+z,small))$r.squared,tolerance=1e-12)
})

test_that("LM quadratic prediction callbacks draw the supplied lm relationship", {
  skip_if_not_installed("jmvcore")
  withr::local_options(device=function(...)grDevices::pdf(file=NULL))
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  capture = new.env(parent=emptyenv())
  local_mocked_bindings(jamovi_glm_model_plot=jamovi_model_plot_recorder(capture))
  for (x in list(NULL,"x")) for (terms in list(list("x"),list())) {
    analysis = lm_quadratic_analysis(preds="x",modelTerms=terms,polynomials=TRUE,quadratic="x",plotX=x,se=FALSE)
    expect_true(analysis$run())
    expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
    plot = capture$plot
    curve = Filter(function(layer)is.data.frame(layer$data)&&"prediction"%in%names(layer$data),plot$layers)[[1]]$data
    model = lm(if(length(terms)) y~x+I(x^2) else y~I(x^2),small)
    expect_equal(curve$prediction,unname(predict(model,newdata=data.frame(x=curve$x))),tolerance=1e-10)
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})

test_that("LM comparisons keep independent quadratic models on identical estimation rows", {
  skip_if_not_installed("jmvcore")
  data = small;data$z[1:3]=NA_real_
  analysis = lm_quadratic_analysis(data,modelTerms=list("x"),polynomials=TRUE,quadratic="x",compare=TRUE,
    comparisonTerms=list("x","z"),comparisonPolynomials=TRUE,comparisonQuadratic="z",comparisonX="x")
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  eligible = data[complete.cases(data[c("y","x","z")]),]
  models = list(lm(y~x+I(x^2),eligible),lm(y~x+z+I(z^2),eligible))
  for(i in 1:2) {
    model = state[[paste0("model",i)]]
    expect_equal(coef(model),coef(models[[i]]),tolerance=1e-12)
    expect_identical(rownames(model.frame(model)),rownames(eligible))
  }
  expect_equal(state$statistics$aic,round(vapply(models,AIC,numeric(1)),3))
  plot = flexplot:::jamovi_glm_comparison_plot(state)
  curve = Filter(function(layer)is.data.frame(layer$data)&&"prediction"%in%names(layer$data),plot$layers)[[1]]$data
  for(label in unique(as.character(curve$model))) {
    model = models[[if(grepl("model1",label))1 else 2]]
    rows = curve[as.character(curve$model)==label,]
    values = vapply(rows$x,function(x){profile=eligible;profile$x=x;mean(predict(model,profile))},numeric(1))
    expect_equal(rows$prediction,values,tolerance=1e-10)
  }
  expect_no_warning(ggplot2::ggplotGrob(plot))
})

test_that("literal quadratic names are safe and checkbox-off models ignore stale squares", {
  skip_if_not_installed("jmvcore")
  probe = new.env(parent=emptyenv());probe$executed=FALSE
  names = c("has space","a+b","tick`name","back\\slash","local({probe$executed = TRUE; 1})")
  for(name in names) {
    data=small;names(data)[names(data)=="x"]=name
    analysis=lm_quadratic_analysis(data,preds=name,modelTerms=list(name),polynomials=TRUE,quadratic=name)
    expect_true(analysis$run())
    expected=lm(flexplot:::jamovi_lm_formula(analysis$options,data),data)
    # Compare against an equivalent syntactic-name model, independent of the formula builder.
    equivalent=lm(y~x+I(x^2),small)
    expect_equal(unname(coef(expected)),unname(coef(equivalent)),tolerance=1e-12)
    jamovi_expect_lm_coefficients(analysis,expected)
  }
  expect_false(probe$executed)
  analysis=lm_quadratic_analysis(modelTerms=list("x"),polynomials=FALSE,quadratic="x")
  expect_true(analysis$run())
  jamovi_expect_lm_coefficients(analysis,lm(y~x,small))
  expect_false(analysis$options$polynomials)
  expect_false(flexplot:::glmbasicOptions$new()$comparisonPolynomials)
})

test_that("compiled LM polynomial handlers drive the actual fitted formulas", {
  skip_if_not_installed("jmvcore")
  node = Sys.which("node")
  skip_if(!nzchar(node),"Node unavailable for installed polynomial handlers")
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  skip_if(!nzchar(compiler),"UI compiler unavailable")
  root = normalizePath(test_path("..",".."))
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")
  skip_if(!length(runtime),"Installed UI runtime unavailable")
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(c(output,paste0(output,".header.R"),paste0(output,".quadratic.json"))))
  log = system2(node,shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime[1])),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  records = jsonlite::fromJSON(paste0(output,".quadratic.json"),simplifyVector=FALSE)
  primary = list(default=y~z,`enabled-empty`=y~z,squared=y~z+x+I(x^2),`square-only-x`=y~z+I(x^2),
    `two-squares`=y~z+x+I(x^2)+I(z^2),comparison=y~z+x+I(x^2)+I(z^2),
    `disabled-primary`=y~z+x,`disabled-second`=y~z+x,restored=y~z+I(x^2),`removed-predictor`=y~z)
  secondary = list(comparison=y~x+z+I(z^2),`disabled-primary`=y~x+z+I(z^2),
    `disabled-second`=y~x+z,restored=y~x+I(z^2),`removed-predictor`=y~I(z^2))
  for(record in records) {
    values = record$options
    values$preds = unlist(values$preds)
    values$quadratic = unlist(values$quadratic)
    values$comparisonQuadratic = unlist(values$comparisonQuadratic)
    values$modelTerms = lapply(values$modelTerms,unlist)
    values$comparisonTerms = lapply(values$comparisonTerms,unlist)
    analysis = do.call(lm_quadratic_analysis,values)
    expect_true(analysis$run(),info=record$stage)
    expected = lm(primary[[record$stage]],small)
    jamovi_expect_lm_coefficients(analysis,expected)
    if(isTRUE(values$compare)) {
      expected2 = lm(secondary[[record$stage]],small)
      expect_equal(coef(analysis$results$comparisonplot$state$model2),coef(expected2),tolerance=1e-12)
    }
  }
})

test_that("saved polynomial options and public LM calls retain independent specifications", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  for(polynomial in c(FALSE,TRUE)) {
    saved = list(out="y",preds=c("x","z"),graphic=FALSE,modelTerms=list("z"),compare=TRUE,
      comparisonTerms=list("x"),comparisonX="x")
    if(polynomial) saved = c(saved,list(polynomials=TRUE,quadratic="x",
      comparisonPolynomials=TRUE,comparisonQuadratic="z"))
    message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::glmbasicOptions$new();options$fromProtoBuf(message)
    analysis = flexplot:::glmbasicClass$new(options=options,data=small)
    expect_true(analysis$run())
    jamovi_expect_lm_coefficients(analysis,lm(if(polynomial)y~z+I(x^2) else y~z,small))
    expect_equal(coef(analysis$results$comparisonplot$state$model2),
      coef(lm(if(polynomial)y~x+I(z^2) else y~x,small)),tolerance=1e-12)
    expect_identical(options$polynomials,polynomial)
    expect_identical(options$comparisonPolynomials,polynomial)
  }
  results = glmbasic(small,out="y",preds=c("x","z"),modelTerms=~x+z,
    polynomials=TRUE,quadratic="x",graphic=FALSE)
  expect_equal(results$glmcat$asDF$means,unname(coef(lm(y~x+z+I(x^2),small))),tolerance=1e-12)
})

test_that("live polynomial and comparison changes replace fitted formulas and coefficient rows", {
  skip_if_not_installed("jmvcore")
  analysis = lm_quadratic_analysis(modelTerms=list("x","z"),quadratic="x",
    comparisonTerms=list("x"),comparisonQuadratic="z")
  expect_true(analysis$run())
  changes = list(list(polynomials=TRUE),list(polynomials=FALSE),
    list(compare=TRUE,comparisonPolynomials=TRUE),list(polynomials=TRUE),
    list(comparisonPolynomials=FALSE),list(compare=FALSE))
  for(change in changes) {
    expect_true(jamovi_reporting_update(analysis,change))
    expected = lm(if(analysis$options$polynomials)y~x+z+I(x^2) else y~x+z,small)
    jamovi_expect_lm_coefficients(analysis,expected)
    expect_identical(attr(terms(analysis$results$plot$state$formula),"term.labels"),attr(terms(expected),"term.labels"))
    if(analysis$options$compare) {
      expected2=lm(if(analysis$options$comparisonPolynomials)y~x+I(z^2) else y~x,small)
      expect_equal(coef(analysis$results$comparisonplot$state$model2),coef(expected2),tolerance=1e-12)
    }
  }
})
