test_that("serialized GLM notes render compactly through the installed native table footer", {
  jmvcore:::initProtoBuf()
  fixtures = new.env(parent=emptyenv()); fixtures$value = list()
  add = function(table,name,expected=character(),forbidden=character()) {
    element = table$asProtoBuf()
    fixtures$value[[length(fixtures$value)+1L]] = list(name=name,columns=length(element$table$columns),
      notes=lapply(element$table$notes,function(note)list(note=note$note)),
      expected=as.list(expected),forbidden=as.list(c("estimates.glm()","95% t confidence intervals",forbidden)))
  }
  for (family in c("gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    outcome = switch(family,gaussian="y",binomial="y_bin",Gamma="y_gam",gammaLog="y_gam","y_pois")
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out=outcome,preds=c("x","a"),
      family=family,graphic=FALSE,compare=TRUE,comparisonX="x",modelTerms=list("x","a"),comparisonTerms=list("x")),small)
    expect_true(analysis$run())
    add(analysis$results$coefficients,family,
      expected=c(paste0("Family: ",analysis$results$plot$state$model$family$family),
        if(family=="binomial") "Success event: 1" else character()),
      forbidden=if(family!="binomial") c("odds ratios","Success event:","Std. OR") else character())
    add(analysis$results$irt,paste(family,"IRT"))
    add(analysis$results$probabilities,paste(family,"probabilities"))
    add(analysis$results$comparison,paste(family,"comparison"),
      c("Model 1:","Model 2:","Approximate BIC Bayes factors","each row over the other model"))
    expect_true(jamovi_reporting_update(analysis,list(showEstimates=FALSE,showIRT=FALSE,showProbabilities=FALSE)))
    for (name in c("coefficients","irt","probabilities")) {
      expect_length(analysis$results[[name]]$notes,0L)
      add(analysis$results[[name]],paste(family,"hidden",name))
    }
  }
  original = withr::with_seed(755, {
    data = data.frame(x=rnorm(120),z=rnorm(120),y=rnorm(120))
    data
  })
  missing_note = "30 observations omitted because model variables were missing."
  for (variable in c("x","z","y")) {
    data = original;data[1:30,variable] = NA_real_
    analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),
      family="gaussian",graphic=FALSE,compare=TRUE,comparisonX="x",modelTerms=list("x"),comparisonTerms=list("x","z")),data)
    expect_true(analysis$run())
    add(analysis$results$coefficients,paste("comparison omissions",variable),missing_note)
    expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
    add(analysis$results$coefficients,paste("standalone omissions",variable),
      expected=if(variable!="z") missing_note else character(),
      forbidden=if(variable=="z") missing_note else character())
  }
  data = original;data$y = rep(0:1,60);data$a = factor(rep(c("A","B","C"),40));data$z[1:30] = NA_real_
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","z"),
    family="binomial",graphic=FALSE,compare=TRUE,comparisonX="x",modelTerms=list("x","a"),
    comparisonTerms=list("x","a","z"),showEstimates=FALSE),data)
  expect_true(analysis$run())
  add(analysis$results$irt,"omissions with estimates hidden",missing_note)
  expect_true(jamovi_reporting_update(analysis,list(showIRT=FALSE)))
  add(analysis$results$probabilities,"omissions with estimates and IRT hidden",missing_note)
  data = small
  event = "<img src=x onerror=probe()> & success"
  data$response = factor(data$y_bin,levels=c(0,1),labels=c("no",event))
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="response",preds="x",family="binomial",graphic=FALSE),data)
  expect_true(analysis$run())
  add(analysis$results$coefficients,"escaped success event",paste0("Success event: ",event))
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(fixtures$value,file,auto_unbox=TRUE,null="null")
  output = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-note-renderer.cjs"),
    "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets",file)),stdout=TRUE,stderr=TRUE)
  expect_null(attr(output,"status"),info=paste(output,collapse="\n"))
  expect_match(paste(output,collapse="\n"),"installed native table-footer code rendered",fixed=TRUE)
})

test_that("saved GLM comparison options recalculate omissions from current analysis data", {
  jmvcore:::initProtoBuf()
  withr::local_seed(756)
  data = data.frame(x=rnorm(120),z=rnorm(120),y=rnorm(120))
  data$z[1:30] = NA_real_
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  values = list(out="y",preds=c("x","z"),family="gaussian",modelTerms=list("x"),
    comparisonTerms=list("x","z"),comparisonX="x",compare=TRUE,graphic=FALSE)
  message$names = names(values)
  message$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=message)
  options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(message)
  reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
  analysis = jamovi_reporting_host(reopened,data)
  expect_true(analysis$run())
  expect_true(reopened$compare)
  expect_identical(reopened$modelTerms,list("x"))
  expect_identical(reopened$comparisonTerms,list("x","z"))
  expect_equal(analysis$results$plot$state$omitted,30)
  expect_identical(analysis$results$coefficients$notes$missing$note,
    "30 observations omitted because model variables were missing.")
  expect_true(jamovi_reporting_update(analysis,list(compare=FALSE)))
  expect_equal(analysis$results$plot$state$omitted,0)
  expect_null(analysis$results$coefficients$notes$missing)
  expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
  expect_equal(analysis$results$plot$state$omitted,30)
  expect_equal(analysis$results$comparison$asDF$n,c(90,90))
})

test_that("native table serialization preserves literal text and blank numeric cells across analyses", {
  jmvcore:::initProtoBuf()
  lm = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(),data=small)
  glm = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(),small)
  mixed = jamovi_mixed_analysis()
  tables = list(list(table=lm$results$factorMeans,text="levels",number="means"),
    list(table=glm$results$probabilities,text="Level",number="Probability"),
    list(table=mixed$results$random,text="group",number="variance"),
    list(table=mixed$results$comparison,text="model",number="p"))
  labels = c("NA","NaN","Inf","-Inf","",NA_character_)
  for (entry in tables) {
    table = entry$table
    before = table$asProtoBuf()
    values = data.frame(text=factor(labels,levels=labels[!is.na(labels)]),
      number=c(NA_real_,NaN,Inf,-Inf,0,.123456789))
    names(values) = c(entry$text,entry$number)
    flexplot:::jamovi_glm_table_rows(table,values)
    output = table$asProtoBuf()
    columns = output$table$columns
    expect_identical(vapply(columns,function(column)column$name,character(1)),
      vapply(before$table$columns,function(column)column$name,character(1)))
    for (i in seq_along(columns)) {
      expect_identical(columns[[i]]$format,before$table$columns[[i]]$format)
      expect_identical(columns[[i]]$type,before$table$columns[[i]]$type)
    }
    text = columns[[match(entry$text,names(table$columns))]]$cells
    number = columns[[match(entry$number,names(table$columns))]]$cells
    for (i in 1:5) {
      expect_true(RProtoBuf::has(text[[i]],"s"))
      expect_identical(text[[i]]$s,labels[i])
    }
    expect_identical(text[[6]]$o,0L) # Native MISSING cell, distinct from literal "NA".
    expect_true(RProtoBuf::has(text[[6]],"o"))
    for (i in 1:4) expect_true(RProtoBuf::has(number[[i]],"o"))
    expect_identical(number[[5]]$d,0)
    expect_identical(number[[6]]$d,.123456789)
  }
})
