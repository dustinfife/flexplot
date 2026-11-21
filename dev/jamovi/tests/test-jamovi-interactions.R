test_that("native interaction switches migrate and drive independent fitted models", {
  node = Sys.which("node")
  compiler = system.file("node_modules", "jamovi-compiler", package="jmvtools")
  runtime = Sys.glob("/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets/analysisui-*.js")[1]
  set.seed(713)
  d = data.frame(x=rnorm(180), z=rnorm(180))
  for (analysis_name in c("glmbasic", "glmgeneralized")) {
    output = tempfile(fileext=".tsv")
    withr::defer(unlink(paste0(output,c("", ".header.R", ".restored.json", ".interactions.json", ".quadratic.json"))))
    log = system2(node, shQuote(c(test_path("js","jamovi-glm-ui.cjs"), root, compiler, output, runtime, analysis_name)),
      stdout=TRUE, stderr=TRUE)
    expect_null(attr(log,"status"), info=paste(log,collapse="\n"))
    if (!file.exists(paste0(output,".interactions.json"))) next
    records = jsonlite::read_json(paste0(output,".interactions.json"), simplifyVector=FALSE)
    d$y = if (analysis_name == "glmbasic") .3+.7*d$x-.2*d$z+.3*d$x*d$z+rnorm(nrow(d)) else
      rbinom(nrow(d),1,plogis(.3+.7*d$x-.2*d$z+.3*d$x*d$z))
    data = d
    data$z[1:8] = NA_real_
    analysis = NULL
    for (record in records) {
      values = record$options
      for (name in intersect(c("preds","quadratic","comparisonQuadratic","plotPanels","comparisonPanels",
                              "mainEffects","comparisonMainEffects"),names(values)))
        values[name] = list(unlist(values[[name]],use.names=FALSE))
      for (name in c("modelTerms","comparisonTerms")) values[name] = list(
        if (is.null(values[[name]])) NULL else lapply(values[[name]],unlist,use.names=FALSE))
      options = do.call(get(paste0(analysis_name,"Options"),asNamespace("flexplot"))$new, values)
      if (is.null(analysis)) {
        analysis = if (analysis_name == "glmbasic") flexplot:::glmbasicClass$new(options=options,data=data) else
          jamovi_reporting_host(options,data)
      } else expect_true(jamovi_reporting_update(analysis,values),info=record$stage)
      expect_true(analysis$run(),info=record$stage)
      waiting = analysis_name == "glmgeneralized" && record$stage %in% c("empty-primary","empty-enabled")
      if (waiting) {
        expect_true(analysis$results$waiting$visible)
        expect_null(analysis$results$plot$state)
        next
      }
      # Independent R formulas for the exact intended specifications, not UI helpers.
      first = switch(record$stage,
        `main-effects-edited`=y~x, interaction=y~x*z, `independent-second`=y~x*z,
        `interaction-only`=y~x:z, `legacy-comparison-off`=y~x*z,
        reopened=y~x*z, `legacy-enabled`=y~x*z, `staged-legacy`=y~x*z, `empty-primary`=y~1, `empty-enabled`=y~1,
        y~x+z)
      second = if (record$stage == "second-off") y~z else y~z+x:z
      eligible = if (isTRUE(values$compare)) data[complete.cases(data[,union(all.vars(first),all.vars(second))]),] else data
      fit = function(formula) if (analysis_name == "glmbasic") lm(formula,eligible) else glm(formula,eligible,family=binomial())
      expected = fit(first)
      actual = if (analysis_name == "glmbasic") lm(analysis$results$plot$state$formula,analysis$results$plot$state$data) else
        analysis$results$plot$state$model
      expect_identical(attr(terms(actual),"term.labels"), attr(terms(expected),"term.labels"),info=record$stage)
      expect_equal(coef(actual),coef(expected),tolerance=1e-12,info=record$stage)
      expect_equal(predict(actual,newdata=d,type="response"),predict(expected,newdata=d,type="response"),tolerance=1e-12)
      expect_equal(nobs(actual),nobs(expected))
      if (isTRUE(values$compare)) {
        comparison = analysis$results$comparisonplot$state
        expected2 = fit(second)
        expect_equal(coef(comparison$model1),coef(expected),tolerance=1e-12)
        expect_equal(coef(comparison$model2),coef(expected2),tolerance=1e-12)
        expect_equal(nobs(comparison$model1),nobs(comparison$model2))
        expect_equal(AIC(comparison$model2),AIC(expected2),tolerance=1e-12)
        expect_equal(BIC(comparison$model1),BIC(expected),tolerance=1e-12)
      }
    }
  }
})

test_that("interaction flags and projections survive actual saved-option messages", {
  jmvcore:::initProtoBuf()
  group = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  option = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  for (name in c("glmbasic","glmgeneralized")) {
    factory = get(paste0(name,"Options"),asNamespace("flexplot"))
    legacy = list(out="y",preds=c("x","z"),modelTerms=list("x",c("x","z")),
      comparisonTerms=list(c("x","z")),compare=FALSE)
    message = group$clone()
    message$names = names(legacy)
    message$options = lapply(legacy,jamovi_mixed_saved_option,option_template=option,group_template=group)
    old = factory$new();old$fromProtoBuf(message)
    expect_equal(old$interactionSpecVersion,0L)
    expect_equal(old$modelTerms,legacy$modelTerms)
    expect_equal(old$comparisonTerms,legacy$comparisonTerms)
    saved = c(legacy, list(interactionSpecVersion=1L,interactions=TRUE,comparisonInteractions=TRUE,
      mainEffects="x",comparisonMainEffects=character()))
    message$names = names(saved)
    message$options = lapply(saved,jamovi_mixed_saved_option,option_template=option,group_template=group)
    current = factory$new();current$fromProtoBuf(message)
    restored = factory$new();restored$fromProtoBuf(current$asProtoBuf())
    for (field in c("interactionSpecVersion","interactions","comparisonInteractions","mainEffects",
                    "comparisonMainEffects","modelTerms","comparisonTerms","compare"))
      expect_equal(restored[[field]],current[[field]])
  }
})
