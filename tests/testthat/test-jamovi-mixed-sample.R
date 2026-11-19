policy_b_analysis = function(data, first="REML", second="REML", ...) {
  jamovi_mixed_analysis(data, preds=c("x","z"), modelTerms=list("x"), randomSlopes="x",
    compare=TRUE, comparisonTerms=list("x","z"), comparisonRandomSlopes="x",
    estimation=first, comparisonEstimation=second, ...)
}

test_that("common-sample tables preserve all four requested estimation combinations", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$z[1:60] = NA_real_
  common = data[61:240,,drop=FALSE]
  for (first in c("REML","ML")) for (second in c("REML","ML")) {
    analysis = policy_b_analysis(data, first, second, showCorrelations=TRUE, includeP=TRUE)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    models = list(lme4::lmer(y~x+(1+x|id),common,REML=first=="REML"),
      lme4::lmer(y~x+z+(1+x|id),common,REML=second=="REML"))
    expect_identical(analysis$results$plot$state$model,state$model1)
    expect_identical(rownames(model.frame(state$model1)),rownames(common))
    expect_identical(rownames(model.frame(state$model2)),rownames(common))
    expect_equal(vapply(list(state$model1,state$model2),nobs,numeric(1)),c(180,180))
    expect_identical(vapply(list(state$model1,state$model2),lme4::isREML,logical(1)),c(first=="REML",second=="REML"))
    expect_equal(lme4::fixef(state$model1),lme4::fixef(models[[1]]),tolerance=1e-10)
    expect_equal(lme4::fixef(state$model2),lme4::fixef(models[[2]]),tolerance=1e-10)
    reported = analysis$results$fixed$asDF
    expect_equal(reported$estimate,unname(lme4::fixef(models[[1]])),tolerance=1e-10)
    expect_equal(reported$se,unname(summary(models[[1]])$coefficients[,2]),tolerance=1e-10)
    components = as.data.frame(lme4::VarCorr(models[[1]]))
    components = components[is.na(components$var2),]
    expect_equal(analysis$results$random$asDF$variance,components$vcov,tolerance=1e-10)
    expect_equal(analysis$results$random$asDF$sd,components$sdcor,tolerance=1e-10)
    expect_equal(unname(as.matrix(analysis$results$correlations$asDF[-1])),
      unname(attr(lme4::VarCorr(models[[1]])[[1]],"correlation")),tolerance=1e-10)
    specification = analysis$results$comparisonSpecification$content
    expect_match(specification,"</p><p>180 complete observations; 12 groups; 60 observations omitted.</p>",fixed=TRUE)
    expect_identical(lengths(regmatches(specification,gregexpr("complete observations",specification,fixed=TRUE))),1L)
    expect_identical(analysis$results$specification$content,"")
    ml = lapply(models,function(model) if(lme4::isREML(model)) lme4::refitML(model) else model)
    expect_equal(state$statistics$aic,vapply(ml,AIC,numeric(1)),tolerance=1e-8)
    expect_equal(state$statistics$bic,vapply(ml,BIC,numeric(1)),tolerance=1e-8)
    expect_equal(state$statistics$bayes,c(bf.bic(ml[[1]],ml[[2]]),bf.bic(ml[[2]],ml[[1]])),tolerance=1e-8)
    expected = anova(ml[[1]],ml[[2]],refit=FALSE)
    expect_equal(state$statistics$lr,expected$Chisq[2],tolerance=1e-8)
    expect_equal(state$statistics$p,expected$`Pr(>Chisq)`[2],tolerance=1e-8)
    if(first=="REML") expect_equal(unname(lme4::fixef(state$model1)["x"]),.5152104,tolerance=1e-7)
    if(second=="REML") expect_equal(unname(lme4::fixef(state$model2)["x"]),.5138160,tolerance=1e-7)
  }
})

test_that("comparison mode never fits a redundant standalone model and reuses label caches", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data(); data$z[1:60] = NA_real_
  capture = new.env(parent=emptyenv()); capture$rows = integer()
  original = flexplot:::jamovi_mixed_fit
  local_mocked_bindings(jamovi_mixed_fit=function(options,data) {
    result = original(options,data)
    capture$rows = c(capture$rows,nobs(result$model))
    result
  })
  analysis = policy_b_analysis(data)
  expect_true(analysis$run())
  expect_equal(capture$rows,c(180,180))
  model = analysis$results$plot$state$model
  expect_true(jamovi_reporting_update(analysis,list(plotLabels=list(list(role="x",variable="x",kind="title",label="Time")))))
  expect_equal(capture$rows,c(180,180))
  expect_identical(analysis$results$plot$state$model,model)
})

test_that("comparison sample follows either model's required variables, not the predictor pool", {
  skip_if_not_installed("jmvcore")
  for (missing in c("none","first","second","unused")) {
    data = jamovi_mixed_test_data()
    if(missing=="first") data$x[1:60] = NA_real_
    if(missing=="second") data$z[1:60] = NA_real_
    if(missing=="unused") data$a[] = NA
    analysis = jamovi_mixed_analysis(data,preds=c("x","z","a"),modelTerms=list("x"),compare=TRUE,
      comparisonTerms=list("z"),randomSlopes="x",comparisonRandomSlopes=NULL,graphic=FALSE)
    expect_true(analysis$run())
    state = analysis$results$comparisonplot$state
    n = if(missing %in% c("first","second")) 180 else 240
    expect_equal(nobs(analysis$results$plot$state$model),n)
    expect_equal(nobs(state$model2),n)
    common = data[complete.cases(data[c("y","x","z","id")]),]
    expected = lme4::lmer(y~z+(1|id),common)
    expect_equal(lme4::fixef(state$model2),lme4::fixef(expected),tolerance=1e-10)
    expect_identical(lme4::getME(state$model2,"cnms")[[1]],"(Intercept)")
  }
  data = jamovi_mixed_test_data();data$z[1:60] = NA_real_
  for (specification in list(list(comparisonTerms=list("x"),comparisonRandomSlopes="z"),
                            list(comparisonTerms=list("x"),comparisonPolynomials=TRUE,comparisonQuadratic="z"))) {
    values = c(list(data=data,preds=c("x","z"),modelTerms=list("x"),compare=TRUE,graphic=FALSE),specification)
    analysis = do.call(jamovi_mixed_analysis,values)
    expect_true(analysis$run())
    expect_equal(nobs(analysis$results$plot$state$model),180)
  }
})

test_that("live Model 2 changes and comparison switching invalidate incompatible samples", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data();data$z[1:60] = NA_real_
  options = flexplot:::mixedmodelOptions$new(out="y",preds=c("x","z"),cluster="id",
    modelTerms=list("x"),randomSlopes="x",compare=FALSE,comparisonTerms=list("x","z"),
    comparisonRandomSlopes="x",graphic=FALSE)
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  expect_equal(nobs(analysis$results$plot$state$model),240)
  for (change in list(list(compare=TRUE),list(comparisonTerms=list("x")),
                      list(comparisonTerms=list("x","z")),list(compare=FALSE),list(compare=TRUE))) {
    expect_true(jamovi_reporting_update(analysis,change))
    common = options$compare && "z" %in% unlist(options$comparisonTerms)
    expected = lme4::lmer(y~x+(1+x|id),if(common)data[61:240,] else data)
    model = analysis$results$plot$state$model
    expect_equal(nobs(model),if(common)180 else 240)
    expect_equal(lme4::fixef(model),lme4::fixef(expected),tolerance=1e-10)
  }
})

test_that("old saved primary states are recalculated on the common sample", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data();data$z[1:60] = NA_real_
  analysis = policy_b_analysis(data)
  legacy = flexplot:::jamovi_mixed_fit(analysis$options,data)
  legacy$fitOptions = lapply(c("out","preds","interactions","polynomials","modelTerms","quadratic","cluster",
    "randomIntercept","randomSlopes","estimation"),analysis$options$get)
  legacy$inputData = data
  expect_equal(nobs(legacy$model),240)
  analysis$results$plot$setState(legacy)
  expect_true(analysis$run())
  expect_equal(nobs(analysis$results$plot$state$model),180)
  expect_equal(analysis$options$samplePolicyVersion,0)
  expect_identical(analysis$options$modelTerms,list("x"))
  expect_identical(analysis$options$comparisonTerms,list("x","z"))
})

test_that("common-sample population, cluster and CAS predictions keep their original methods", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data();data$z[1:60] = NA_real_
  analysis = policy_b_analysis(data,"REML","ML")
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  common = data[61:240,]
  models = list(lme4::lmer(y~x+(1+x|id),common,REML=TRUE),
    lme4::lmer(y~x+z+(1+x|id),common,REML=FALSE))
  for (type in c("population","cluster")) {
    plot = withr::with_seed(741,flexplot:::jamovi_mixed_comparison_plot(state,"x","id",type=type))
    curve = Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data),plot$layers)[[1]]$data
    for (label in unique(as.character(curve$model))) {
      model = models[[if(grepl("model1",label,fixed=TRUE))1L else 2L]]
      rows = curve[as.character(curve$model)==label,,drop=FALSE]
      expected = vapply(seq_len(nrow(rows)),function(i) {
        profile = common;profile$x = rows$x[i];profile$id = rows$id[i]
        mean(predict(model,profile,re.form=if(type=="cluster")NULL else NA))
      },numeric(1))
      expect_equal(rows$prediction,expected,tolerance=1e-10)
    }
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
  plot = flexplot:::jamovi_mixed_comparison_plot(state,"x",type="cas",casGhost=TRUE)
  children = attr(plot,"jamovi_cas_children")
  for (i in 1:2) {
    model = models[[i]]
    expected = common$y-(predict(model,re.form=NULL)-predict(model,re.form=NA))
    expect_equal(children[[i]]$data$y_adj,unname(expected),tolerance=1e-10)
    curve = attr(children[[i]],"cas_prediction")
    expected = vapply(curve$x,function(x) {
      profile = common;profile$x = x;mean(predict(model,profile,re.form=NA))
    },numeric(1))
    expect_equal(curve$prediction,expected,tolerance=1e-10)
    ghost = Filter(function(layer)identical(layer$aes_params$linetype,"dashed"),children[[i]]$layers)
    expect_length(ghost,if(i==1L)1L else 0L)
    if(i==1L) expect_identical(ghost[[1]]$data,attr(children[[2]],"cas_prediction"))
  }
  expect_no_warning(patchwork::patchworkGrob(plot))
})
