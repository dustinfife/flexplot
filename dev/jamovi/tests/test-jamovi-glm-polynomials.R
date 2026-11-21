test_that("native GLM polynomial controls preserve fitted model spaces across families", {
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")[1]
  output = tempfile(fileext=".tsv")
  withr::defer(unlink(paste0(output,c("", ".header.R", ".restored.json", ".interactions.json", ".polynomials.json"))))
  log = system2(Sys.which("node"),shQuote(c(test_path("js","jamovi-glm-ui.cjs"),root,compiler,output,runtime,"glmgeneralized")),stdout=TRUE,stderr=TRUE)
  expect_null(attr(log,"status"),info=paste(log,collapse="\n"))
  expect_true(file.exists(paste0(output,".polynomials.json")))
  records = jsonlite::read_json(paste0(output,".polynomials.json"),simplifyVector=FALSE)
  set.seed(714)
  d = data.frame(x=rnorm(160),z=rnorm(160))
  eta = .4+.2*d$x-.1*d$z+.15*d$x^2+.1*d$z^2+.08*d$x*d$z
  outcomes = list(gaussian=eta+rnorm(nrow(d),sd=.5),binomial=rbinom(nrow(d),1,plogis(eta)),
                  poisson=rpois(nrow(d),exp(eta)),gamma=rgamma(nrow(d),shape=4,scale=exp(eta)/4))
  for (family_name in names(outcomes)) {
    data = d;data$y = outcomes[[family_name]]
    data$x[1:7] = NA_real_;data$z[8:16] = NA_real_
    family = switch(family_name,gaussian=gaussian(),binomial=binomial(),poisson=poisson(),gamma=Gamma(link="log"))
    analysis = NULL
    for (record in records) {
      values = record$options;values$family = if (family_name=="gamma") "gammaLog" else family_name
      for (name in c("preds","quadratic","comparisonQuadratic","plotPanels","comparisonPanels","mainEffects","comparisonMainEffects"))
        values[name] = list(unlist(values[[name]],use.names=FALSE))
      for (name in c("modelTerms","comparisonTerms"))
        values[name] = list(if (is.null(values[[name]])) NULL else lapply(values[[name]],unlist,use.names=FALSE))
      if (is.null(analysis)) analysis = jamovi_reporting_host(do.call(flexplot:::glmgeneralizedOptions$new,values),data) else
        expect_true(jamovi_reporting_update(analysis,values),info=record$stage)
      expect_true(analysis$run(),info=paste(family_name,record$stage))
      if (record$stage=="empty") {
        expect_true(analysis$results$waiting$visible);next
      }
      first = switch(record$stage,
        quadratic=y~x+z+I(x^2), `square-only`=y~z+I(x^2),
        `two-squares`=y~z+x+I(x^2)+I(z^2),
        `interaction-squares`=,`independent-second`=,`comparison-off`=,`comparison-on`=y~x*z+I(x^2)+I(z^2),
        `primary-off`=,`second-off`=,`repeated-edits`=,`off-reopened`=y~x*z,
        `legacy-0`=,`legacy-1`=,`legacy-enabled`=,reopened=y~z+I(x^2),
        `removed-predictor`=y~z,y~x+z)
      second = switch(record$stage,
        `second-off`=,`repeated-edits`=,`off-reopened`=y~x,
        `removed-predictor`=y~I(z^2),y~x+I(z^2))
      required = if (isTRUE(values$compare)) union(all.vars(first),all.vars(second)) else all.vars(first)
      eligible = data[complete.cases(data[,required,drop=FALSE]),,drop=FALSE]
      expected = glm(first,data=eligible,family=family)
      model = analysis$results$plot$state$model
      expect_equal(coef(model),coef(expected),tolerance=1e-12,info=paste(family_name,record$stage))
      expect_equal(sqrt(diag(vcov(model))),sqrt(diag(vcov(expected))),tolerance=1e-12)
      expect_equal(predict(model,newdata=d,type="response"),predict(expected,newdata=d,type="response"),tolerance=1e-12)
      expect_equal(nobs(model),nobs(expected))
      if (isTRUE(values$compare)) {
        comparison = analysis$results$comparisonplot$state
        expected2 = glm(second,data=eligible,family=family)
        expect_equal(coef(comparison$model1),coef(expected),tolerance=1e-12)
        expect_equal(coef(comparison$model2),coef(expected2),tolerance=1e-12)
        expect_equal(sqrt(diag(vcov(comparison$model2))),sqrt(diag(vcov(expected2))),tolerance=1e-12)
        expect_equal(predict(comparison$model2,newdata=d,type="response"),predict(expected2,newdata=d,type="response"),tolerance=1e-12)
        expect_equal(nobs(comparison$model1),nobs(comparison$model2))
        expect_equal(AIC(comparison$model2),AIC(expected2),tolerance=1e-12)
        expect_equal(BIC(comparison$model1),BIC(expected),tolerance=1e-12)
        expect_equal(as.numeric(logLik(comparison$model2)),as.numeric(logLik(expected2)),tolerance=1e-12)
        reported = analysis$results$comparison$asDF
        expect_equal(reported$aic,round(c(AIC(expected),AIC(expected2)),3))
        expect_equal(reported$bic,round(c(BIC(expected),BIC(expected2)),3))
      }
    }
  }
})

test_that("GLM polynomial flags and migration version round-trip through saved messages", {
  jmvcore:::initProtoBuf()
  message = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  option = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (version in 0:2) {
    saved = list(out="y",preds=c("x","z"),modelTerms=list("z"),quadratic="x",
      comparisonTerms=list("x"),comparisonQuadratic="z",compare=FALSE,interactionSpecVersion=version)
    if (version==2) saved = c(saved,list(polynomials=TRUE,comparisonPolynomials=TRUE))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=option,group_template=message)
    initial = flexplot:::glmgeneralizedOptions$new();initial$fromProtoBuf(message)
    restored = flexplot:::glmgeneralizedOptions$new();restored$fromProtoBuf(initial$asProtoBuf())
    for (name in names(saved)) expect_equal(restored[[name]],saved[[name]])
  }
})
