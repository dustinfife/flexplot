lm_manual_analysis = function(data, includeP=FALSE, terms=list("x"), second=list("x","z"), graphic=FALSE) {
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","z","a"),modelTerms=terms,
    comparisonTerms=second,compare=TRUE,includeP=includeP,graphic=graphic,comparisonGraphic=graphic,
    comparisonX="x",plotX="x")
  analysis = flexplot:::glmbasicClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read;environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  analysis
}

test_that("LM common-sample explanations describe only additional exclusions without changing fits", {
  d = small[complete.cases(small[c("y","x","z")]),]
  cases = list(complete=d,same=d,second=d,both=d)
  cases$same$y[1] = NA_real_
  cases$second$z[1:2] = NA_real_
  cases$both$x[1] = NA_real_;cases$both$z[2] = NA_real_
  for (name in names(cases)) {
    data = cases[[name]]
    first = list("x");second = if(name=="both") list("z") else list("x","z")
    analysis = lm_manual_analysis(data,TRUE,first,second)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    f1 = y~x;f2 = if(name=="both") y~z else y~x+z
    common = complete.cases(data[c("y","x","z")])
    expected = c(sum(complete.cases(data[all.vars(f1)]) & !common),
      sum(complete.cases(data[all.vars(f2)]) & !common))
    expect_equal(state$additionalExcluded,expected)
    expect_identical(is.null(flexplot:::jamovi_lm_sample_note(state)),!any(expected>0))
    independent = list(lm(f1,data[common,]),lm(f2,data[common,]))
    for (i in 1:2) {
      model = state[[paste0("model",i)]]
      expect_equal(coef(model),coef(independent[[i]]),tolerance=1e-12)
      expect_equal(as.numeric(logLik(model)),as.numeric(logLik(independent[[i]])),tolerance=1e-12)
      expect_equal(nobs(model),sum(common))
    }
    if (name=="second") {
      expect_match(analysis$results$comparison$notes$column_n$note,"2 additional observations from Model 1",fixed=TRUE)
      expect_false(grepl("from Model 2",analysis$results$comparison$notes$column_n$note,fixed=TRUE))
      expect_equal(state$p,anova(independent[[1]],independent[[2]])$`Pr(>F)`[2],tolerance=1e-12)
    }
    if (name=="both") expect_match(flexplot:::jamovi_lm_sample_note(state),
      "1 additional observation from Model 1; 1 additional observation from Model 2",fixed=TRUE)
  }
})

test_that("LM comparison header notes renumber conditionally and follow Include p-values", {
  data = small[complete.cases(small[c("y","x","z")]),]
  analysis = lm_manual_analysis(data)
  expect_true(analysis$run())
  table = analysis$results$comparison
  p = table$getCell("p",rowNo=1)$value
  stats = table$asDF
  expect_false(table$getColumn("p")$visible)
  expect_identical(table$getColumn("n")$title,"N")
  expect_identical(table$getColumn("bayes.factor")$title,"BIC Bayes factorᵃ")
  expect_identical(names(table$notes),"column_bayes.factor")
  expect_true(jamovi_reporting_update(analysis,list(includeP=TRUE)))
  expect_identical(table$getColumn("p")$title,"pᵇ")
  expect_match(table$notes$column_p$note,"ᵇ Nested-model F-test.",fixed=TRUE)
  expect_equal(table$getCell("p",rowNo=1)$value,p,tolerance=0)
  expect_equal(table$asDF[names(stats)],stats,tolerance=0)
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_true(analysis$options$includeP)
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
  expect_true(table$getColumn("p")$visible)
  expect_true(jamovi_reporting_update(analysis,list(includeP=FALSE)))
  expect_null(table$notes$column_p)
  expect_identical(table$getColumn("p")$title,"p")
  expect_false(table$getColumn("p")$visible)
  for (column in table$columns) for (i in 1:2) expect_length(column$getCell(i)$footnotes,0L)
  state = analysis$results$comparisonplot$state
  state$additionalExcluded = c(2L,1L)
  flexplot:::jamovi_glm_comparison_table(table,state,TRUE)
  expect_identical(table$getColumn("n")$title,"Nᵃ")
  expect_identical(table$getColumn("bayes.factor")$title,"BIC Bayes factorᵇ")
  expect_identical(table$getColumn("p")$title,"pᶜ")
  expect_identical(substr(vapply(table$notes,`[[`,character(1),"note"),1,1),
    c(column_n="ᵃ",column_bayes.factor="ᵇ",column_p="ᶜ"))
  flexplot:::jamovi_glm_comparison_table(table,analysis$results$comparisonplot$state,FALSE)
  expect_identical(names(table$notes),"column_bayes.factor")
  expect_identical(table$getColumn("n")$title,"N")
  expect_identical(table$getColumn("bayes.factor")$title,"BIC Bayes factorᵃ")
})

test_that("LM default titles and user titles retain stable label keys and independent intervals", {
  data = small[complete.cases(small[c("y","x","z")]),]
  analysis = lm_manual_analysis(data,graphic=TRUE)
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  main = flexplot:::jamovi_lm_display(state$model1,data,analysis$options)
  comparison = flexplot:::jamovi_glm_comparison_plot(state)
  expect_identical(main$labels$title,"Model Visualization")
  expect_identical(comparison$labels$title,"Model Comparison Visualization")
  for (isComparison in c(FALSE,TRUE)) {
    option = if(isComparison) "comparisonPlotLabels" else "plotLabels"
    rows = analysis$options[[option]]
    keys = vapply(rows,`[[`,character(1),"key")
    title = which(vapply(rows,function(row)row$role=="title",logical(1)))
    rows[[title]]$label = if(isComparison) "Custom comparison" else "Custom main"
    expect_true(jamovi_reporting_update(analysis,setNames(list(rows),option)))
    expect_identical(vapply(analysis$options[[option]],`[[`,character(1),"key"),keys)
    plot = if(isComparison) comparison else main
    info = flexplot:::jamovi_lm_label_info(state$model1,analysis$options,isComparison,state$formula)
    labeled = flexplot:::jamovi_plot_apply_labels(plot,analysis$options[[option]],info$context,info$layout)
    expect_identical(labeled$labels$title,rows[[title]]$label)
  }
  original = ggplot2::ggplot_build(comparison)$data
  for (se in c(FALSE,TRUE)) {
    expect_true(jamovi_reporting_update(analysis,list(se=se)))
    expect_equal(ggplot2::ggplot_build(flexplot:::jamovi_glm_comparison_plot(state))$data,original)
    plot = flexplot:::jamovi_lm_display(state$model1,data,analysis$options)
    intervals = Filter(function(layer)inherits(layer$geom,"GeomRibbon"),plot$layers)
    expect_identical(length(intervals)>0L,se)
  }
  expect_length(analysis$results$getRefs(),0L)
  expect_identical(analysis$results$getRefs(recurse=TRUE),c("flexplot","graphicalAnalysis"))
  expect_false("methodReferences" %in% analysis$results$itemNames)
})

test_that("LM serializes bibliography entries without a Method References block or callouts", {
  jmvcore:::initProtoBuf()
  for (empty in c(FALSE,TRUE)) {
    options = if(empty)flexplot:::glmbasicOptions$new() else
      flexplot:::glmbasicOptions$new(out="y",preds="x",graphic=FALSE)
    analysis = flexplot:::glmbasicClass$new(options=options,data=if(empty)NULL else small,
      analysisId=1L,datasetId="lm-citation-review")
    if(empty) {
      analysis$.setReadDatasetSource(function(columns)data.frame())
      analysis$.setReadDatasetHeaderSource(function(columns)data.frame())
      analysis$init()
    } else expect_true(analysis$run())
    response = analysis$asProtoBuf(includeState=FALSE)
    expect_identical(vapply(response$references,function(ref)ref$name,character(1)),
      c("flexplot","graphicalAnalysis"))
    walk = function(element) {
      expect_false(identical(element$name,"methodReferences"))
      expect_length(element$refs,0L)
      if(element$has("group"))for(child in element$group$elements)walk(child)
    }
    walk(response$results)
  }
})
