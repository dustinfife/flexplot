test_that("real protobuf preserves legacy named families and current Automatic provenance", {
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  data = small;data$y = rep(0:1,length.out=nrow(data));data$other = data$y+3
  for (family in c("auto","gaussian","binomial","poisson","gammaLog","Gamma","negativebinomial")) {
    saved = list(out="y",preds="x",family=family,graphic=FALSE,modelTerms=list("x"))
    pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(pb)
    for (cycle in 1:3) {
      analysis = jamovi_reporting_host(options,data)
      expect_true(analysis$run())
      expect_identical(options$family,family)
      if(family %in% c("auto","binomial","gaussian","poisson")) {
        state = analysis$results$plot$state
        expect_identical(state$model$family$family,if(family=="auto") "binomial" else family)
      }
      expect_true(jamovi_reporting_update(analysis,list(out="other")))
      expect_identical(options$family,family)
      expect_true(jamovi_reporting_update(analysis,list(out="y")))
      reopened = flexplot:::glmgeneralizedOptions$new();reopened$fromProtoBuf(options$asProtoBuf())
      expect_identical(reopened$family,family)
      expect_identical(reopened$automaticFamilyOutcome,"y")
      options = reopened
    }
  }
  # Missing legacy selector means historical Gaussian, not newly assumed Auto.
  saved = list(out="y",preds="x",graphic=FALSE,modelTerms=list("x"))
  pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(pb)
  analysis = jamovi_reporting_host(options,data)
  expect_true(analysis$run());expect_identical(options$family,"gaussian")
  expect_identical(analysis$results$plot$state$model$family$family,"gaussian")
  restored = flexplot:::glmgeneralizedOptions$new();restored$fromProtoBuf(options$asProtoBuf())
  expect_identical(restored$family,"gaussian")
})

test_that("backend Automatic resolution works before predictors and uses observed native values", {
  jmvcore:::initProtoBuf()
  data = small;data$y = factor(rep(c("No","Yes"),length.out=nrow(data)),levels=c("No","Yes","unused"))
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  saved = list(out="y",family="auto",graphic=FALSE)
  pb$names = names(saved);pb$options = lapply(saved,jamovi_mixed_saved_option,option_template=template,group_template=pb)
  options = flexplot:::glmgeneralizedOptions$new();options$fromProtoBuf(pb)
  analysis = jamovi_reporting_host(options,data)
  expect_true(analysis$run());expect_true(analysis$results$waiting$visible)
  expect_identical(options$family,"auto");expect_identical(options$automaticFamily,"binomial")
  response = flexplot:::glmgeneralizedOptions$new();response$fromProtoBuf(options$asProtoBuf())
  expect_identical(response$automaticFamily,"binomial");expect_identical(response$automaticFamilyOutcome,"y")
  expect_true(jamovi_reporting_update(analysis,list(preds="x")))
  expect_identical(analysis$results$plot$state$model$family$family,"binomial")
  expect_match(analysis$results$specification$content,"Success event: Yes",fixed=TRUE)
})
