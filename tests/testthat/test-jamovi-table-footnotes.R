test_that("targeted GLM cell footnotes retain estimands and context across families", {
  skip_if_not_installed("jmvcore")
  for(family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome=switch(family,gaussian="y",binomial="y_bin",gammaLog="y_gam",Gamma="y_gam","y_pois")
    analysis=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=outcome,preds=c("x","a"),
      modelTerms=list("x","a",c("x","a")),interactions=TRUE,family=family,graphic=FALSE),small)
    expect_true(analysis$run());results=analysis$results;table=results$coefficients
    expect_identical(names(table$notes)[1],"model")
    text=paste(jamovi_test_table_notes(table),collapse=" ")
    expect_match(text,"Interaction effects depend",fixed=TRUE)
    expect_match(paste(table$getCell("lower",rowNo=1)$footnotes,collapse=" "),"raw coefficients",fixed=TRUE)
    if(family=="negativebinomial") expect_match(text,"estimated size (theta)",fixed=TRUE)
    if(family=="Gamma") expect_match(text,"not response ratios",fixed=TRUE)
    if(family=="binomial") {
      expect_match(paste(table$getCell("standardized.OR",rowNo=2)$footnotes,collapse=" "),"one-SD",fixed=TRUE)
      expect_match(paste(table$getCell("Prediction Difference (+/- 1 SD)",rowNo=2)$footnotes,collapse=" "),"reference profile",fixed=TRUE)
      expect_length(table$getCell("Prediction Difference (+/- 1 SD)",rowNo=1)$footnotes,0L)
      expect_match(paste(results$irt$getCell("variable",rowNo=1)$footnotes,collapse=" "),"fitted reference levels",fixed=TRUE)
      expect_match(paste(results$probabilities$getCell("Probability",rowNo=1)$footnotes,collapse=" "),"averaged",fixed=TRUE)
    }
    before=table$asDF;notes=jamovi_test_table_notes(table)
    expect_true(analysis$run());expect_equal(table$asDF,before,tolerance=0)
    expect_identical(jamovi_test_table_notes(table),notes)
    expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
    for(name in c("coefficients","irt","probabilities")) expect_length(jamovi_test_table_notes(results[[name]]),0L)
    expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
    for(name in c("coefficients","irt","probabilities")) {
      expect_equal(results[[name]]$rowCount,0L)
      expect_length(jamovi_test_table_notes(results[[name]]),0L)
    }
    expect_true(jamovi_reporting_update(analysis,list(out=outcome,showEstimates=TRUE,showIRT=TRUE,showProbabilities=TRUE)))
    expect_identical(jamovi_test_table_notes(table),notes)
  }
})

test_that("comparison p headings, conditional test reasons and BIC directions match fitted models", {
  skip_if_not_installed("jmvcore")
  for(module in c("glmbasic","glmgeneralized","mixedmodel"))for(second in list(list("x"),list("x","z"),list("z"),list("a"))) {
    values=list(out="y",preds=c("x","z","a"),modelTerms=list("x","z"),comparisonTerms=second,
      compare=TRUE,includeP=TRUE,graphic=FALSE,comparisonGraphic=FALSE,comparisonX="x")
    data=small
    if(module=="mixedmodel"){values$comparisonX=NULL;values$cluster="id";values$comparisonFixed=unique(unlist(second));data=jamovi_mixed_test_data()}
    factory=get(paste0(module,"Options"),asNamespace("flexplot"));class=get(paste0(module,"Class"),asNamespace("flexplot"))
    analysis=class$new(options=do.call(factory$new,values),data=NULL)
    reader=jamovi_reporting_read;environment(reader)=list2env(list(data=data),parent=environment(reader))
    analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
    expect_true(analysis$run());table=analysis$results$comparison
    if(module=="glmbasic") expect_match(table$getColumn("p")$title,"^p[ᵃᵇᶜ]$") else
      expect_identical(table$getColumn("p")$title,"p")
    state=analysis$results$comparisonplot$state
    bic=if(module=="mixedmodel")c(BIC(lme4::refitML(state$model1)),BIC(lme4::refitML(state$model2))) else c(BIC(state$model1),BIC(state$model2));actual=table$asDF$bayes.factor
    expected=exp(c(bic[2]-bic[1],bic[1]-bic[2])/2)
    expect_equal(actual,if(module=="mixedmodel")expected else round(expected,3))
    text=paste(jamovi_test_table_notes(table),collapse=" ")
    expect_match(text,"row model over the other model",fixed=TRUE)
    p=table$getCell("p",rowNo=1)$value
    if(length(p) && is.finite(p))expect_false(grepl("only; unavailable|nonnested/equivalent",text)) else
      expect_match(text,"equivalent|nonnested|unavailable")
  }
})

test_that("LM notes keep interval methods and conditioning without redundant descriptions", {
  skip_if_not_installed("jmvcore")
  analysis=flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),graphic=FALSE),data=NULL)
  reader=jamovi_reporting_read;environment(reader)=list2env(list(data=small),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_match(analysis$results$glmcat$notes$intervals$note,"t confidence intervals",fixed=TRUE)
  note=analysis$results$diff$notes$intervals$note
  expect_match(note,"95% t confidence intervals for individual pairwise contrasts.",fixed=TRUE)
  expect_false(grepl("Bonferroni|Tukey|Adjusted intervals|estimable pairwise family",note))
  expect_match(note,"held at their means",fixed=TRUE)
  note=analysis$results$factorMeans$notes$reference$note
  expect_match(note,"Conditional predictions",fixed=TRUE)
  expect_false(grepl("not model coefficients",note,fixed=TRUE))
})

test_that("unavailable GLM tests identify invalid likelihood fields without calculating substitutes", {
  skip_if_not_installed("jmvcore")
  analysis=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),family="gaussian",
    modelTerms=list("x","z"),comparisonTerms=list("x"),comparisonX="x",compare=TRUE,includeP=TRUE,graphic=FALSE,comparisonGraphic=FALSE),small)
  expect_true(analysis$run());state=analysis$results$comparisonplot$state;state$p=NA_real_
  for(case in list(list(df=0,lr=1,reason="degrees of freedom"),list(df=1,lr=Inf,reason="nonfinite likelihood-ratio"),
    list(df=1,lr=-1,reason="negative likelihood-ratio"))) {
    state$df=case$df;state$lr=case$lr
    flexplot:::jamovi_glm_generalized_comparison_table(analysis$results$comparison,state)
    expect_match(analysis$results$comparison$notes$test$note,case$reason,fixed=TRUE)
    expect_null(analysis$results$comparison$getCell("p",rowNo=1)$value)
  }
})
