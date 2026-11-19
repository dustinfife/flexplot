jamovi_mixed_comparison_analysis = function(data=jamovi_mixed_test_data(), ...) {
  values = list(out="y",preds=c("x","a","z"),cluster="id",compare=TRUE,graphic=FALSE,
    modelTerms=list("x","a","z",c("x","a")),interactions=TRUE,polynomials=TRUE,quadratic="x",
    randomSlopes="x",comparisonTerms=list("x","a","z"),comparisonRandomSlopes="x")
  supplied = list(...)
  for (name in names(supplied)) values[name] = supplied[name]
  options = do.call(flexplot:::mixedmodelOptions$new,values)
  flexplot:::mixedmodelClass$new(options=options,data=data)
}

jamovi_mixed_comparison_capture = function(plot, entries) {
  capture$plot = original(plot,entries)
  capture$plot
}

test_that("Mixed comparisons use ML copies for nested fixed effects and preserve original REML estimates", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  analysis = jamovi_mixed_comparison_analysis(data,includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_false(is.null(state))
  original1 = lme4::lmer(y~x*a+z+I(x^2)+(1+x|id),data,REML=TRUE)
  original2 = lme4::lmer(y~x+a+z+(1+x|id),data,REML=TRUE)
  expect_true(lme4::isREML(state$model1))
  expect_true(lme4::isREML(state$model2))
  expect_true(lme4::isREML(analysis$results$plot$state$model))
  expect_equal(lme4::fixef(state$model1),lme4::fixef(original1),tolerance=1e-10)
  expect_equal(lme4::fixef(state$model2),lme4::fixef(original2),tolerance=1e-10)
  expected = list(lme4::refitML(original1),lme4::refitML(original2))
  expect_false(any(vapply(state$statistics$models,lme4::isREML,logical(1))))
  table = analysis$results$comparison$asDF
  expect_equal(table$aic,vapply(expected,AIC,numeric(1)),tolerance=1e-8)
  expect_equal(table$bic,vapply(expected,BIC,numeric(1)),tolerance=1e-8)
  bayes = exp((BIC(expected[[2]])-BIC(expected[[1]]))/2)
  expect_equal(table$bayes.factor,c(bayes,1/bayes),tolerance=1e-8)
  expect_equal(prod(table$bayes.factor),1)
  independent = anova(expected[[1]],expected[[2]],refit=FALSE)
  expect_equal(state$statistics$lr,independent$Chisq[2],tolerance=1e-8)
  expect_equal(state$statistics$df,independent$Df[2])
  expect_equal(table$p[1],independent$`Pr(>Chisq)`[2],tolerance=1e-8)
  expect_null(analysis$results$comparison$getCell("p",rowNo=2)$value)
  expect_match(analysis$results$comparison$notes$estimation$note,"Model comparisons use ML")
  expect_match(analysis$results$comparison$notes$bayes$note,"Approximate BIC.*each row.*reciprocal")
  expect_match(analysis$results$comparisonSpecification$content,"Model 2")
})

test_that("Mixed comparisons use the common sample for primary estimates and comparison fits", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$z[c(1,21,41)] = NA
  data$a[2] = NA
  analysis = jamovi_mixed_comparison_analysis(data,modelTerms=list("x"),quadratic=NULL,polynomials=FALSE,
    comparisonTerms=list("x","z"),comparisonEstimation="ML")
  expect_true(analysis$run())
  primary = analysis$results$plot$state
  state = analysis$results$comparisonplot$state
  expect_identical(rownames(model.frame(state$model1)),rownames(model.frame(state$model2)))
  eligible = data[complete.cases(data[c("y","x","z","id")]),]
  expect_identical(rownames(state$data),rownames(eligible))
  expect_equal(nobs(primary$model),nrow(eligible))
  expect_equal(nobs(state$model1),nrow(data)-3L)
  expect_true(lme4::isREML(state$model1))
  expect_false(lme4::isREML(state$model2))
  expected1 = lme4::lmer(y~x+(1+x|id),eligible,REML=TRUE)
  expected2 = lme4::lmer(y~x+z+(1+x|id),eligible,REML=FALSE)
  expect_equal(lme4::fixef(state$model1),lme4::fixef(expected1),tolerance=1e-10)
  expect_equal(lme4::fixef(state$model2),lme4::fixef(expected2),tolerance=1e-10)
  expect_equal(lme4::fixef(primary$model),lme4::fixef(expected1),tolerance=1e-10)
})

test_that("Mixed comparisons with random boundary changes or nonnested fixed effects omit ordinary LRT p-values", {
  skip_if_not_installed("jmvcore")
  for (slopes in list(NULL,"x")) {
    analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x"),quadratic=NULL,polynomials=FALSE,
      comparisonTerms=list("x"),comparisonRandomSlopes=slopes,includeP=TRUE)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    expect_true(all(is.finite(state$statistics$aic)))
    expect_true(all(is.finite(state$statistics$bic)))
    expect_true(is.na(state$statistics$p))
    expect_null(analysis$results$comparison$getCell("p",rowNo=1)$value)
    if (is.null(slopes)) {
      independent = anova(state$statistics$models[[1]],state$statistics$models[[2]],refit=FALSE)
      expect_equal(state$statistics$lr,independent$Chisq[2],tolerance=1e-8)
      expect_equal(state$statistics$df,independent$Df[2])
      expect_match(analysis$results$comparison$notes$randomP$note,
        "^P-value unavailable.*random-effects comparisons.*chi-square")
    } else expect_match(state$statistics$reason,"equivalent")
  }
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x"),quadratic=NULL,polynomials=FALSE,
    comparisonTerms=list("z"),estimation="ML",comparisonEstimation="ML")
  expect_true(analysis$run())
  expect_true(is.na(analysis$results$comparisonplot$state$statistics$p))
  expect_match(analysis$results$comparison$notes$test$note,"nonnested")
  expect_null(analysis$results$comparison$notes$randomP)
  expect_true(is.na(analysis$results$comparisonplot$state$statistics$lr))
  expect_false("p" %in% names(analysis$results$comparison$asDF))
  # Intercept-free structures are fitted as requested and do not receive a naive boundary p-value.
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x"),quadratic=NULL,polynomials=FALSE,
    comparisonTerms=list("x"),comparisonRandomIntercept=FALSE,includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_identical(lme4::getME(state$model2,"cnms")[[1]],"x")
  expect_true(is.na(state$statistics$p))
})

test_that("Mixed comparisons retain singular estimates and clear results when a common fit is invalid", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$y = rep(seq(-1,1,length.out=15)+sin(seq_len(15)),16)
  analysis = jamovi_mixed_comparison_analysis(data,modelTerms=list("x"),quadratic=NULL,
    comparisonTerms=list("x","z"),randomSlopes=NULL,comparisonRandomSlopes=NULL,includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_true(lme4::isSingular(state$model1))
  expect_true(all(is.na(state$statistics$bayes)))
  expect_true(all(vapply(state$statistics$models,flexplot:::jamovi_mixed_converged,logical(1))))
  independent = anova(state$statistics$models[[1]],state$statistics$models[[2]],refit=FALSE)
  expect_equal(state$statistics$lr,independent$Chisq[2],tolerance=1e-8)
  expect_equal(state$statistics$df,independent$Df[2])
  expect_true(is.na(state$statistics$p))
  expect_match(analysis$results$comparison$notes$singular$note,"Singular ML comparison fit")
  expect_null(analysis$results$comparison$notes$randomP)
  expect_true(jamovi_reporting_update(analysis,list(comparisonTerms=list("x"),comparisonRandomSlopes="x")))
  random_state = analysis$results$comparisonplot$state
  expect_true(any(vapply(random_state$statistics$models,lme4::isSingular,logical(1))))
  expect_true(all(vapply(random_state$statistics$models,flexplot:::jamovi_mixed_converged,logical(1))))
  expect_true(is.finite(random_state$statistics$lr))
  expect_gt(random_state$statistics$df,0)
  expect_true(is.na(random_state$statistics$p))
  expect_match(analysis$results$comparison$notes$randomP$note,"P-value unavailable")
  expect_match(analysis$results$diagnostics$content,"rank-deficient")
  expect_false(grepl("not been automatically simplified",analysis$results$diagnostics$content))
  invalid = jamovi_mixed_comparison_analysis(comparisonRandomIntercept=FALSE,comparisonRandomSlopes=NULL)
  expect_true(invalid$run())
  expect_false(invalid$results$fixed$visible)
  expect_true(invalid$results$comparison$visible)
  expect_null(invalid$results$comparisonplot$state)
  expect_match(invalid$results$comparison$.__enclos_env__$private$.error,"random intercept or")
  options = invalid$options
  empty = jamovi_mixed_test_data(); empty$z = NA_real_
  expect_error(flexplot:::jamovi_mixed_comparison(options,empty),"random intercept or")
  option = options$option("comparisonRandomIntercept");option$value=TRUE
  expect_error(flexplot:::jamovi_mixed_comparison(options,empty),"No common complete")
})

test_that("Mixed comparison plots use actual original-model predictions and preserve custom labels", {
  skip_if_not_installed("jmvcore")
  for (type in c("population","cluster")) {
    set.seed(151)
    analysis = jamovi_mixed_comparison_analysis(graphic=TRUE,plotX="x",plotPanels="id",predictionType=type)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    expect_true(analysis$results$comparisonplot$visible)
    expect_false(analysis$results$plot$visible)
    plot = state$displayPlot
    expect_false(is.null(plot))
    curve = Filter(function(layer) is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    expect_setequal(as.character(unique(curve$model)),c("model1 (lmerMod)","model2 (lmerMod)"))
    for (label in unique(as.character(curve$model))) {
      model = if (label=="model1 (lmerMod)") state$model1 else state$model2
      rows = curve[as.character(curve$model)==label,,drop=FALSE]
      expected = vapply(seq_len(nrow(rows)),function(i) {
        profile = state$data
        profile$x = rows$x[i];profile$id = rows$id[i]
        mean(predict(model,newdata=profile,re.form=if (type=="cluster") NULL else NA))
      },numeric(1))
      expect_equal(rows$prediction,expected,tolerance=1e-10)
    }
    built = ggplot2::ggplot_build(plot)
    expect_setequal(built$plot$scales$get_scales("colour")$get_labels(),c("Model 1","Model 2"))
    expect_false(any(vapply(plot$layers,function(layer) inherits(layer$geom,"GeomRibbon"),logical(1))))
    expect_no_warning(ggplot2::ggplotGrob(plot))
    entries = analysis$options$plotLabels
    for (i in seq_along(entries)) if (entries[[i]]$role=="x") entries[[i]]$label="Time"
    option = analysis$options$option("plotLabels");option$value=entries
    analysis$optionsChangedHandler("plotLabels")
    expect_true(analysis$run())
    expect_identical(analysis$results$comparisonplot$state$model1,state$model1)
    capture = new.env(parent=emptyenv())
    recorder = jamovi_mixed_comparison_capture
    environment(recorder) = list2env(list(capture=capture,original=flexplot:::jamovi_mixed_apply_labels),parent=environment(recorder))
    local_mocked_bindings(jamovi_mixed_apply_labels=recorder)
    expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
    expect_equal(capture$plot$labels$x,"Time")
    cas = flexplot:::jamovi_mixed_comparison_plot(state,"x","id",type="cas")
    expect_identical(vapply(attr(cas,"jamovi_cas_children"),function(p) p$labels$title,character(1)),c("Model 1","Model 2"))
    expect_error(flexplot:::jamovi_mixed_comparison_plot(state,"x","id",group="a",type=type),"panels instead")
  }
})

test_that("Mixed comparisons restore independent second-model specifications and switch modes without stale output", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  saved = list(out="y",preds=c("x","a","z"),cluster="id",modelTerms=list("x","a"),
    compare=TRUE,comparisonFixed=c("x","a"),comparisonTerms=list("x","a",c("x","a")),comparisonInteractions=TRUE,
    comparisonPolynomials=TRUE,comparisonQuadratic="x",randomSlopes="x",
    comparisonRandomIntercept=FALSE,comparisonRandomSlopes="x",comparisonEstimation="ML",
    includeP=TRUE,casGhost=TRUE,graphic=TRUE,plotX="x",plotPanels="id",predictionType="population")
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  message$names=names(saved)
  message$options=lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options=flexplot:::mixedmodelOptions$new()
  options$fromProtoBuf(message)
  data=jamovi_mixed_test_data()
  analysis=flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader=jamovi_reporting_read
  environment(reader)=list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_true(options$compare)
  expect_true(options$includeP)
  expect_true(options$casGhost)
  expect_true(analysis$results$comparison$getColumn("p")$visible)
  state=analysis$results$comparisonplot$state
  independent=lme4::lmer(y~x*a+I(x^2)+(0+x|id),data,REML=FALSE)
  expect_equal(lme4::fixef(state$model2),lme4::fixef(independent),tolerance=1e-10)
  expect_identical(lme4::getME(state$model2,"cnms")[[1]],"x")
  expect_true(lme4::isREML(state$model1))
  expect_false(lme4::isREML(state$model2))
  original_primary=analysis$results$plot$state$model
  expect_true(jamovi_reporting_update(analysis,list(comparisonEstimation="REML")))
  expect_true(lme4::isREML(analysis$results$comparisonplot$state$model2))
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(original_primary),tolerance=1e-10)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_false(analysis$results$comparison$visible)
  expect_false(analysis$results$comparisonSpecification$visible)
  expect_false(analysis$results$comparisonplot$visible)
  expect_null(analysis$results$comparisonplot$state)
  expect_true(analysis$results$plot$visible)
  expect_identical(options$comparisonTerms,list("x","a",c("x","a")))
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
  expect_true(analysis$results$comparisonplot$visible)
  expect_true(analysis$results$comparison$visible)
  expect_equal(lme4::fixef(analysis$results$plot$state$model),lme4::fixef(original_primary),tolerance=1e-10)
  expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
  expect_true(analysis$results$waiting$visible)
  expect_false(analysis$results$comparison$visible)
  expect_null(analysis$results$comparisonplot$state)
})

test_that("Mixed comparisons recognize actual random designs and reject unreliable likelihood tests", {
  skip_if_not_installed("jmvcore")
  data=jamovi_mixed_test_data()
  first=lme4::lmer(y~x+z+(1+x+z|id),data)
  reordered=lme4::lmer(y~x+z+(1+z+x|id),data)
  expect_true(flexplot:::jamovi_mixed_same_random_design(first,reordered))
  different=lme4::lmer(y~x+z+(1+x|id),data)
  expect_false(flexplot:::jamovi_mixed_same_random_design(first,different))
  # An ML fit must retain its convergence diagnostic; it is not refitted again.
  unreliable=lme4::refitML(different)
  unreliable@optinfo$conv$lme4$messages="Model failed to converge"
  state=flexplot:::jamovi_mixed_comparison_statistics(list(lme4::refitML(first),unreliable))
  expect_match(state$reason,"convergence diagnostics")
  expect_true(all(is.na(state$aic)))
  expect_true(all(is.na(state$bayes)))
  expect_true(is.na(state$p))
  expect_true(is.na(state$lr))
  expect_true(is.na(state$df))
  expect_false(state$random_p_unavailable)
})


test_that("Mixed categorical comparison layouts build and intercept-only visualizations are explicit", {
  skip_if_not_installed("jmvcore")
  analysis=jamovi_mixed_comparison_analysis(graphic=TRUE,plotX="a",plotPanels=c("id","z"),predictionType="cluster")
  expect_true(analysis$run())
  plot=analysis$results$comparisonplot$state$displayPlot
  expect_false(is.null(plot))
  expect_no_warning(ggplot2::ggplotGrob(plot))
  expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  analysis=jamovi_mixed_comparison_analysis(modelTerms=list(),comparisonTerms=list(),randomSlopes=NULL,
    comparisonRandomSlopes=NULL,polynomials=FALSE,quadratic=NULL)
  expect_true(analysis$run())
  expect_true(analysis$results$comparison$visible)
  expect_error(flexplot:::jamovi_mixed_comparison_plot(analysis$results$comparisonplot$state,type="population"),"intercept-only")
})


test_that("Mixed comparison p columns and random-test notes follow options in serialized results", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x"),polynomials=FALSE,quadratic=NULL,
    comparisonTerms=list("x"),comparisonRandomSlopes=NULL,includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_true(state$statistics$random_p_unavailable)
  expect_true(is.finite(state$statistics$lr))
  expect_gt(state$statistics$df,0)
  for (enabled in c(TRUE,FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(includeP=enabled)))
    table = analysis$results$comparison
    expect_identical(table$getColumn("p")$visible,enabled)
    serialized = table$asProtoBuf()$table$columns
    column = Filter(function(column) column$name == "p",serialized)[[1]]
    visibility = get("jamovi.coms.Visible",asNamespace("jmvcore"))
    expect_equal(column$visible,if (enabled) visibility$YES else visibility$NO)
    if (enabled) expect_match(table$notes$randomP$note,"^P-value unavailable") else
      expect_null(table$notes$randomP)
    expect_identical(analysis$results$comparisonplot$state$model1,state$model1)
    expect_equal(table$asDF$lr[1],state$statistics$lr)
    expect_equal(table$asDF$df[1],state$statistics$df)
  }
  # A nonnested random structure does not receive the boundary-test note.
  expect_true(jamovi_reporting_update(analysis,list(randomSlopes="x",comparisonRandomSlopes="z",includeP=TRUE)))
  expect_match(analysis$results$comparison$notes$test$note,"nonnested")
  expect_null(analysis$results$comparison$notes$randomP)
  expect_true(is.na(analysis$results$comparisonplot$state$statistics$p))
})


test_that("Mixed comparison presentation is compact without losing model specifications", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_comparison_analysis(includeP=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  notes = vapply(analysis$results$comparison$notes,`[[`,character(1),"note")
  expect_length(notes,3L)
  expect_lt(sum(nchar(notes)),350)
  expect_match(notes[["estimation"]],"Model comparisons use ML on identical observations; plots use the original fitted models")
  expect_match(notes[["bayes"]],"Approximate BIC.*reciprocal")
  expect_false(analysis$results$specification$visible)
  content = analysis$results$comparisonSpecification$content
  expect_match(content,"</strong>.*<br><strong>Model 2:")
  for (model in list(state$model1,state$model2))
    expect_match(content,flexplot:::jamovi_mixed_escape(paste(deparse(formula(model)),collapse=" ")),fixed=TRUE)
  first = lme4::fixef(state$model1)
  expect_true(jamovi_reporting_update(analysis,list(comparisonInteractions=TRUE,
    comparisonTerms=list("x","z",c("x","z")),comparisonPolynomials=TRUE,comparisonQuadratic="z")))
  updated = analysis$results$comparisonplot$state
  expect_identical(lme4::fixef(updated$model1),first)
  expect_match(paste(deparse(formula(updated$model2)),collapse=" "),"x:z",fixed=TRUE)
  expect_match(paste(deparse(formula(updated$model2)),collapse=" "),"I(z^2)",fixed=TRUE)
  expect_match(analysis$results$comparison$notes$test$note,"nonnested")
})


test_that("Mixed Model 2 fixed selections constrain terms without selecting fixed effects from slopes", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x","a"),polynomials=FALSE,quadratic=NULL,
    comparisonFixed="a",comparisonTerms=list("x","a"),comparisonRandomSlopes="x",graphic=TRUE,plotX="x",plotPanels="id")
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_identical(colnames(lme4::getME(state$model1,"X")),c("(Intercept)","x","a.L","a.Q"))
  expect_identical(colnames(lme4::getME(state$model2,"X")),c("(Intercept)","a.L","a.Q"))
  expect_identical(lme4::getME(state$model2,"cnms")[[1]],c("(Intercept)","x"))
  expect_false(analysis$results$plotExplanation$visible)
  expect_identical(analysis$results$plotExplanation$content,"")
  expect_match(analysis$results$comparison$notes$estimation$note,
    "Model comparisons use ML on identical observations; plots use the original fitted models",fixed=TRUE)
  expect_true(jamovi_reporting_update(analysis,list(comparisonFixed=c("x","a"),comparisonTerms=list("x","a"))))
  restored = analysis$results$comparisonplot$state
  expect_identical(lme4::fixef(restored$model1),lme4::fixef(state$model1))
  expect_identical(colnames(lme4::getME(restored$model2,"X")),colnames(lme4::getME(state$model1,"X")))
  # Old saved analyses do not have the new option: explicit legacy terms are retained.
  options = analysis$options
  option = options$option("comparisonFixed"); option$value = NULL
  option = options$option("comparisonTerms"); option$value = list("x")
  legacy = flexplot:::jamovi_mixed_comparison(options,jamovi_mixed_test_data())
  expect_identical(colnames(lme4::getME(legacy$model2,"X")),c("(Intercept)","x"))
})


test_that("new backend comparisons initialize from Model 1 fixed predictors rather than unused candidates", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_comparison_analysis(modelTerms=list("x","a"),polynomials=FALSE,quadratic=NULL,
    comparisonFixed=NULL,comparisonTerms=NULL)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  expect_identical(colnames(lme4::getME(state$model1,"X")),colnames(lme4::getME(state$model2,"X")))
  expect_false("z" %in% all.vars(formula(state$model2)))
  expect_equal(lme4::fixef(state$model1),lme4::fixef(state$model2),tolerance=1e-10)
})

test_that("saved CAS ghost settings restore the same note and serialized result order", {
  skip_if_not_installed("jmvcore")
  withr::local_libpaths(c(.libPaths(),"/Applications/jamovi.app/Contents/Resources/modules/base/R"))
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  for (enabled in c(TRUE,FALSE)) {
    saved = list(out="y",preds=c("x","z"),cluster="id",modelTerms=list("x","z"),
      comparisonFixed=c("x","z"),comparisonTerms=list("z"),compare=TRUE,graphic=TRUE,
      predictionType="cas",plotX="x",casGhost=enabled)
    message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=message)
    options = flexplot:::mixedmodelOptions$new()
    options$fromProtoBuf(message)
    restored = flexplot:::mixedmodelClass$new(options=options,data=NULL)
    reader = jamovi_reporting_read
    environment(reader) = list2env(list(data=jamovi_mixed_test_data()),parent=environment(reader))
    restored$.setReadDatasetSource(reader)
    restored$.setReadDatasetHeaderSource(reader)
    expect_true(restored$run())
    expect_identical(restored$results$comparisonPlotNote$visible,enabled)
    note = restored$results$comparisonPlotNote$asProtoBuf()
    expect_identical(note$html$content,if (enabled)
      "<p>Note. Dashed line in Model 1 represents Model 2's fixed-effects predictions.</p>" else "")
    message = restored$results$asProtoBuf(status=3L)
    items = vapply(message$group$elements,function(item)item$name,character(1))
    specification = message$group$elements[[match("comparisonSpecification",items)]]
    expect_match(specification$html$content,"</p><p>240 complete observations; 16 groups; 0 observations omitted.</p>",fixed=TRUE)
    expect_false(restored$results$explanation$visible)
    expect_identical(restored$results$explanation$content,"")
    expect_identical(match("comparisonPlotNote",items),match("comparisonplot",items)+1L)
  }
})
