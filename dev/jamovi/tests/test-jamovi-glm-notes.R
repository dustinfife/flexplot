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
