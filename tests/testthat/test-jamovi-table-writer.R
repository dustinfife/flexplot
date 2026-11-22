test_that("the shared writer preserves literal text in every text column", {
  skip_if_not_installed("jmvcore")
  text = c("NA","NaN","Inf","-Inf","",NA_character_,"ordinary","0")
  values = data.frame(term=text,var=text,comparison=text,group=text,difference=text,
    category=factor(text,levels=text[!is.na(text)]),
    ordered=ordered(text,levels=rev(text[!is.na(text)])),
    value=c(NA_real_,NaN,Inf,-Inf,0,1.23456789,-2,3))
  columns = lapply(names(values),function(name)
    list(name=name,type=if(name=="value") "number" else "text"))
  table = jmvcore::Table$new(columns=columns)
  table$setNote("probe","Unchanged note")
  flexplot:::jamovi_glm_table_rows(table,values)
  expect_identical(names(table$asDF),names(values))
  for (name in setdiff(names(values),"value")) {
    # jmvcore's convenience data-frame accessor maps empty text to NA.
    expected = text;expected[!is.na(expected) & expected==""] = NA_character_
    expect_identical(table$asDF[[name]],expected)
    for (i in seq_along(text)) {
      cell = table$getCell(rowNo=i,col=name)
      if (is.na(text[i])) expect_true(cell$isNotFilled()) else {
        expect_false(cell$isNotFilled())
        expect_identical(cell$value,text[i])
      }
    }
  }
  for (i in 1:4) expect_true(table$getCell(rowNo=i,col="value")$isNotFilled())
  expect_identical(table$asDF$value[5:8],values$value[5:8])
  expect_identical(table$notes$probe$note,"Unchanged note")
})

test_that("literal names and factor levels survive complete LM, GLM and Mixed Model runs", {
  skip_if_not_installed("jmvcore")
  withr::local_seed(758)
  labels = c("NA","NaN","Inf","-Inf")
  for (i in seq_along(labels)) {
    name = labels[i];cluster = labels[i %% length(labels)+1L]
    data = data.frame(y=rnorm(240),binary=rep(c(0,1,1,0,1,0,0,1),30),x=rnorm(240))
    data[[name]] = factor(rep(labels,60),levels=labels)
    data[[cluster]] = factor(rep(1:16,each=15))
    data$y = data$y + rep(rnorm(16),each=15)
    formula = flexplot:::jamovi_glm_formula("y",c("x",name),NULL)
    lm_model = stats::lm(formula,data)
    lm = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
      out="y",preds=c("x",name),graphic=FALSE,includeP=TRUE),data=data)
    expect_true(lm$run())
    jamovi_expect_lm_coefficients(lm,lm_model,includeP=TRUE)
    expect_identical(lm$results$factorMeans$asDF$var,c(name,rep(NA_character_,3)))
    expect_identical(lm$results$factorMeans$asDF$levels,labels)
    glm = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(
      out="binary",preds=c("x",name),family="binomial",graphic=FALSE),data)
    expect_true(glm$run())
    expected = estimates(glm$results$plot$state$model)
    probabilities = attr(expected,"marginal_probabilities")
    rownames(probabilities) = rownames(glm$results$probabilities$asDF)
    expect_equal(glm$results$probabilities$asDF,probabilities)
    expect_identical(glm$results$probabilities$asDF$Variable,rep(name,4))
    expect_identical(glm$results$probabilities$asDF$Level,labels)
    mixed = jamovi_mixed_analysis(data,preds=c("x",name),cluster=cluster,
      modelTerms=list("x",name))
    expect_true(mixed$run())
    state = mixed$results$plot$state
    expect_identical(mixed$results$fixed$asDF$term,names(lme4::fixef(state$model)))
    expect_equal(mixed$results$fixed$asDF$estimate,unname(lme4::fixef(state$model)))
    expect_identical(mixed$results$random$asDF$group,c(cluster,"Residual"))
    expect_equal(mixed$results$random$asDF$variance,as.data.frame(lme4::VarCorr(state$model))$vcov)
  }
})
