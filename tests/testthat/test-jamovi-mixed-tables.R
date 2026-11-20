test_that("both model tables use requested-method common-sample fits without extra fitting", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data(); data$z[1:60] = NA_real_
  common = data[61:240,]
  capture = new.env(parent=emptyenv());capture$count=0L
  original = flexplot:::jamovi_mixed_fit
  local_mocked_bindings(jamovi_mixed_fit=function(options,data) {
    capture$count = capture$count+1L
    original(options,data)
  })
  for (first in c("ML","REML")) for (second in c("ML","REML")) {
    before = capture$count
    analysis = jamovi_mixed_analysis(data,preds=c("x","z"),modelTerms=list("x"),
      randomSlopes="x",compare=TRUE,comparisonTerms=list("x","z"),
      comparisonRandomSlopes=NULL,estimation=first,comparisonEstimation=second,showCorrelations=TRUE)
    expect_true(analysis$run())
    expect_equal(capture$count-before,2L)
    models = list(lme4::lmer(y~x+(1+x|id),common,REML=first=="REML"),
      lme4::lmer(y~x+z+(1|id),common,REML=second=="REML"))
    for(i in 1:2) expect_mixed_tables(analysis$results,models[[i]],second=i==2)
    expect_match(analysis$results$comparisonSpecification$content,"180 complete observations; 12 groups; 60 observations omitted.",fixed=TRUE)
    for(name in c("model1Heading","model2Heading","comparisonHeading")) expect_true(analysis$results[[name]]$visible)
    expect_identical(analysis$results$model2Heading$content,"<h3>Model 2</h3>")
    expect_length(analysis$results$fixed2$notes,0)
  }
})

test_that("Model 2 tables retain factor contrasts, interactions and squared coefficients", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  contrasts(data$a) = contr.sum(3)
  analysis = jamovi_mixed_analysis(data,modelTerms=list("x"),compare=TRUE,
    comparisonTerms=list("x","a",c("x","a")),comparisonInteractions=TRUE,
    comparisonQuadratic="x",comparisonPolynomials=TRUE,comparisonRandomSlopes="x",showCorrelations=TRUE)
  expect_true(analysis$run())
  expected = lme4::lmer(y~x+a+x:a+I(x^2)+(1+x|id),data)
  expect_mixed_tables(analysis$results,expected,second=TRUE)
  expect_setequal(analysis$results$fixed2$asDF$term,names(lme4::fixef(expected)))
  # Rank-deficient structures remain rejected by the existing fitter.
  data$duplicate = data$x
  invalid = jamovi_mixed_analysis(data,preds=c("x","duplicate"),modelTerms=list("x"),compare=TRUE,
    comparisonTerms=list("x","duplicate"))
  expect_true(invalid$run())
  expect_false(invalid$results$fixed2$visible)
  expect_match(invalid$results$comparison$.__enclos_env__$private$.error,"rank deficient")
})

test_that("shared output options and live Model 2 changes clear stale tables and columns", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x"),randomSlopes="x",compare=TRUE,
    comparisonTerms=list("x","z"),comparisonRandomSlopes="x",showCorrelations=TRUE)
  expect_true(analysis$run())
  original = analysis$results$comparisonplot$state
  for (enabled in c(FALSE,TRUE,FALSE)) {
    expect_true(jamovi_reporting_update(analysis,list(includeCI=enabled,includeP=enabled,showCorrelations=enabled)))
    for(name in c("fixed","fixed2")) for(column in c("lower","upper","p"))
      expect_identical(analysis$results[[name]]$getColumn(column)$visible,enabled)
    for(name in c("correlations","correlations2")) expect_identical(analysis$results[[name]]$visible,enabled)
    expect_identical(analysis$results$comparisonplot$state$model2,original$model2)
  }
  for (slopes in list(NULL,c("x","z"),"z","x")) {
    expect_true(jamovi_reporting_update(analysis,list(comparisonRandomSlopes=slopes,showCorrelations=TRUE)))
    expected = analysis$results$comparisonplot$state$model2
    expect_mixed_tables(analysis$results,expected,second=TRUE)
    if(length(slopes)) expect_equal(sum(vapply(analysis$results$correlations2$columns,
      function(column)isTRUE(column$visible),logical(1))),length(lme4::getME(expected,"cnms")[[1]])+1L)
  }
  expect_true(jamovi_reporting_update(analysis,list(comparisonTerms=list("x"))))
  expect_identical(analysis$results$fixed2$asDF$term,c("(Intercept)","x"))
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  for(name in c("fixed2","random2","correlations2","model1Heading","model2Heading","comparisonHeading"))
    expect_false(analysis$results[[name]]$visible)
  expect_equal(analysis$results$fixed2$rowCount,0)
  expect_true(analysis$results$fixed$visible)
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
  expect_identical(analysis$results$fixed2$asDF$term,c("(Intercept)","x"))
})

test_that("model-specific singular and convergence diagnostics appear once above both table sections", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(modelTerms=list("x"),compare=TRUE,comparisonTerms=list("x"))
  # Isolate reporting of diagnostics from optimizer-dependent singularity decisions.
  original = flexplot:::jamovi_mixed_comparison
  local_mocked_bindings(jamovi_mixed_comparison=function(...) {
    state = original(...)
    state$first$singular = FALSE
    state$second$singular = TRUE
    state$second$warnings = "Model 2 diagnostic probe"
    state
  })
  expect_true(analysis$run())
  expect_match(analysis$results$diagnostics$content,"Model 2: Singular fit detected",fixed=TRUE)
  expect_false(grepl("Model 1: Singular",analysis$results$diagnostics$content,fixed=TRUE))
  expect_match(analysis$results$diagnostics$content,"Model 2: Convergence / fitting diagnostic: Model 2 diagnostic probe",fixed=TRUE)
  expect_false(any(grepl("Model 2 diagnostic probe",unlist(analysis$results$comparison$notes),fixed=TRUE)))
  items = vapply(analysis$results$items,function(item)item$name,character(1))
  expect_lt(match("diagnostics",items),match("model1Heading",items))
  expect_lt(match("correlations",items),match("model2Heading",items))
  expect_lt(match("correlations2",items),match("comparisonHeading",items))
  # Real singular fits still report their estimated variance components.
  data = jamovi_mixed_test_data(); data$y = rep(seq(-1,1,length.out=15)+sin(seq_len(15)),16)
  state = original(analysis$options,data)
  expect_true(state$first$singular)
  expect_true(state$second$singular)
  results = flexplot:::mixedmodelResults$new(options=analysis$options)
  flexplot:::jamovi_mixed_tables(list(fixed=results$fixed2,random=results$random2,correlations=results$correlations2),state$second,FALSE)
  expect_equal(results$random2$asDF$variance,as.data.frame(lme4::VarCorr(state$model2))$vcov)
})



test_that("identical models agree and real Model 2-only singular fits are labeled correctly", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(compare=TRUE,modelTerms=list("x","z"),
    comparisonTerms=list("x","z"),showCorrelations=TRUE)
  expect_true(analysis$run())
  expect_equal(analysis$results$fixed2$asDF,analysis$results$fixed$asDF)
  expect_equal(analysis$results$random2$asDF,analysis$results$random$asDF)
  expect_true(jamovi_reporting_update(analysis,list(comparisonRandomSlopes=c("x","z"))))
  state = analysis$results$comparisonplot$state
  expect_false(state$first$singular)
  expect_true(state$second$singular)
  expect_match(analysis$results$diagnostics$content,"Model 2: Singular fit detected",fixed=TRUE)
  expect_false(grepl("Model 1: Singular",analysis$results$diagnostics$content,fixed=TRUE))
  expected = suppressMessages(lme4::lmer(y~x+z+(1+x+z|id),jamovi_mixed_test_data()))
  expect_mixed_tables(analysis$results,expected,second=TRUE)
})
