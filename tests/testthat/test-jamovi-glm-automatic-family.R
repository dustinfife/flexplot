test_that("Automatic eligibility uses exactly two observed supported values", {
  binary = flexplot:::jamovi_glm_binary_outcome
  for (response in list(factor(c("No","Yes",NA)),ordered(c("No","Yes",NA)),
      c(TRUE,FALSE,NA),c(0,1,NA_real_),c(0L,1L,NA_integer_),c("No","Yes",NA),
      factor(c("No","Yes"),levels=c("unused","Yes","No")))) expect_true(binary(response))
  for (response in list(c(3,7),c(0,0,NA),c(NA_real_,NA_real_),logical(),NULL,
      factor(c("No","No"),levels=c("No","Yes")),factor(c(NA,NA),levels=c("No","Yes")),
      c(0,1,2),c(0,Inf),as.Date(c("2020-01-01","2020-01-02")),matrix(c(0,1)),c(0+0i,1+0i)))
    expect_false(binary(response))
})

automatic_glm_host = function(data, family="auto", compare=FALSE) {
  jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","a","z"),family=family,
    modelTerms=list("x","a","z"),comparisonTerms=list("x","a"),compare=compare,
    graphic=FALSE,comparisonGraphic=FALSE,plotX="x",plotPanels="a",comparisonX="x",comparisonPanels="a",ghostLines=TRUE),data)
}

test_that("Automatic and explicit Binomial share event orientation and all inference/predictions", {
  withr::local_seed(1001)
  data = small
  values = rbinom(nrow(data),1,plogis(.2+.25*data$x+.15*data$z))
  values[c(4,5)] = NA_integer_
  data$z[1:3] = NA_real_
  representations = list(values,as.logical(values),factor(values,levels=c(1,0),labels=c("Yes","No")),
    ordered(values,levels=c(1,0),labels=c("Yes","No")),factor(values,levels=c(2,1,0),labels=c("unused","Yes","No")))
  for (response in representations) for (compare in c(FALSE,TRUE)) {
    data$y = response
    auto = automatic_glm_host(data,compare=compare)
    manual = automatic_glm_host(data,"binomial",compare)
    expect_true(auto$run());expect_true(manual$run())
    expect_identical(auto$options$family,"auto")
    expect_identical(auto$options$automaticFamily,"binomial")
    expect_identical(auto$options$automaticFamilyOutcome,"y")
    tables = function(analysis)lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item)item$asDF)
    expect_equal(tables(auto),tables(manual),tolerance=0)
    first = auto$results$plot$state
    second = manual$results$plot$state
    expect_equal(coef(first$model),coef(second$model),tolerance=0)
    expect_equal(vcov(first$model),vcov(second$model),tolerance=0)
    expect_equal(fitted(first$model),fitted(second$model),tolerance=0)
    expect_equal(stats::nobs(first$model),sum(complete.cases(data[c("y","x","a","z")])))
    expect_identical(first$model$family$family,"binomial")
    expect_identical(first$model$family$link,"logit")
    expect_identical(auto$results$specification$content,manual$results$specification$content)
    if(is.factor(response)) expect_match(auto$results$specification$content,"Success event: No",fixed=TRUE)
    for (scale in c("probability","logit")) {
      frames = function(plot)lapply(plot$layers,function(layer)if(is.data.frame(layer$data))layer$data else NULL)
      plot = function(state)flexplot:::jamovi_glm_generalized_plot(state,x="x",panels="a",scale=scale,ghostLines=TRUE)
      expect_equal(frames(plot(first)),frames(plot(second)),tolerance=0)
      if(compare) {
        draw = function(analysis)flexplot:::jamovi_glm_generalized_comparison_plot(analysis$results$comparisonplot$state,scale=scale)
        expect_equal(frames(draw(auto)),frames(draw(manual)),tolerance=0)
      }
    }
  }
})

test_that("automatic choice uses full observations but retains fitted-sample validation", {
  data = small
  data$y = rep(0:1,length.out=nrow(data))
  data$x[data$y==1] = NA_real_
  for(compare in c(FALSE,TRUE)) {
    analysis = automatic_glm_host(data,compare=compare)
    expect_true(analysis$run())
    expect_identical(analysis$options$automaticFamily,"binomial")
    expect_match(analysis$results$explanation$content,"both levels observed",fixed=TRUE)
    expect_null(analysis$results$plot$state)
  }
  data = small;data$y = factor(rep(c("No","Yes"),length.out=nrow(data)),levels=c("No","Yes","unused"))
  data$x[data$y=="Yes"] = NA_real_
  analysis = automatic_glm_host(data)
  expect_true(analysis$run());expect_match(analysis$results$explanation$content,"exactly two observed outcome levels",fixed=TRUE)
})

test_that("named families remain manual across outcome edits and Automatic can be selected again", {
  data = small;data$y = rep(0:1,length.out=nrow(data));data$other = data$y+3
  analysis = automatic_glm_host(data,"gaussian")
  expect_true(analysis$run())
  expect_identical(analysis$results$plot$state$model$family$family,"gaussian")
  expect_true(jamovi_reporting_update(analysis,list(out="other")))
  expect_identical(analysis$options$family,"gaussian")
  expect_true(jamovi_reporting_update(analysis,list(family="auto")))
  expect_identical(analysis$options$automaticFamily,"gaussian")
  expect_true(jamovi_reporting_update(analysis,list(out="y")))
  expect_identical(analysis$options$family,"auto")
  expect_identical(analysis$options$automaticFamily,"binomial")
  expect_true(jamovi_reporting_update(analysis,list(family="binomial",out="other")))
  expect_identical(analysis$options$family,"binomial")
  expect_match(analysis$results$explanation$content,"numeric 0/1",fixed=TRUE)
})
