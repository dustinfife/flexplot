source(file.path(root,"dev/jamovi/e2e/fixtures.R"),local=TRUE)
source(file.path(root,"dev/jamovi/e2e/generate.R"),local=TRUE)

test_that("E2E fixtures are deterministic, typed, valid and do not consume caller RNG", {
  set.seed(73); before = .Random.seed
  fixtures = e2e_fixtures()
  expect_identical(.Random.seed,before)
  expect_identical(fixtures,e2e_fixtures())
  expect_identical(fixtures,readRDS(file.path(root,"dev/jamovi/e2e/artifacts/fixtures.rds")))
  data = fixtures$primary
  expect_identical(dim(data),c(436L,34L))
  expect_identical(levels(data$group),c("middle","low","high"))
  expect_true(is.ordered(data$ordered))
  expect_identical(levels(data$category_y),c("Disagree","Neutral","Agree"))
  expect_identical(levels(data$factor_y),c("No","Yes"))
  expect_true(all(data$binary_y %in% 0:1))
  for(name in c("count_y","nb_y")) expect_true(all(data[[name]] >= 0 & data[[name]]==floor(data[[name]])))
  expect_true(all(data$gamma_y>0 & data$gamma_inverse_y>0))
  expect_equal(range(as.numeric(table(data$id))),c(12,27))
  expect_equal(nlevels(data$id),24L)
  expect_lt(abs(cor(data$x,data$z)),.15)
  expect_gt(cor(data$x,data$correlated),.8)
  expect_equal(which(is.na(data$x_missing)),seq(7,nrow(data),17))
  expect_equal(which(is.na(data$z_missing)),seq(3,nrow(data),13))
  expect_equal(which(is.na(data$y_missing)),seq(11,nrow(data),19))
  expect_equal(which(is.na(data$binary_missing)),seq(5,nrow(data),23))
  ordinary = model.matrix(~x+z+binary+group+x:z+x:binary+I(x^2),data)
  expect_equal(qr(ordinary)$rank,ncol(ordinary))
  expect_gt(coef(lm(quadratic_y~x+z+binary+group+I(x^2),data))["I(x^2)"],.7)
  interactions = coef(lm(interaction_y~x+z+binary+group+x:z+x:binary,data))
  expect_gt(interactions["x:z"],.9)
  expect_gt(interactions["x:binaryYes"],1)
  expect_gt(var(data$nb_y),mean(data$nb_y))
  expect_true(all(c("has space","a+b","tick`name","back\\slash","log(y)","1","stop('must not execute')") %in% names(data)))
})

test_that("purpose-built edge fixtures expose only the intended pathological cases", {
  fixtures = e2e_fixtures()
  expect_true(all(fixtures$separation$y==as.integer(fixtures$separation$x>0)))
  x = model.matrix(~x+x_duplicate,fixtures$rank_deficient)
  expect_lt(qr(x)$rank,ncol(x))
  expect_true(any(table(fixtures$sparse$x,fixtures$sparse$group)==0L))
  expect_true(any(table(fixtures$sparse$x)==1L))
  singular = suppressMessages(lme4::lmer(y~x+(1+x|id),fixtures$singular))
  expect_true(lme4::isSingular(singular))
  expect_equal(fixtures$paired$y[31:60]-fixtures$paired$y[1:30],rep(.5,30))
  expect_identical(which(is.na(fixtures$common_sample$z)),1:60)
})

test_that("E2E manifest covers every actual option and has valid resource references", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"),simplifyVector=FALSE)
  fixtures = e2e_fixtures()
  expect_true(e2e_validate_manifest(manifest,fixtures,root))
  invalid = manifest
  invalid$scenarios[[1]]$steps[[1]]$expected_options$unknownOption = TRUE
  expect_error(e2e_validate_manifest(invalid,fixtures,root))
  invalid = manifest
  invalid$scenarios[[2]]$steps[[1]]$expected_options$line = "unavailable-method"
  expect_error(e2e_validate_manifest(invalid,fixtures,root),"invalid value")
  inventory = e2e_inventory(root,manifest)
  expect_identical(jsonlite::fromJSON(jsonlite::toJSON(inventory,auto_unbox=TRUE,null="null"),simplifyVector=FALSE),
    jsonlite::read_json(file.path(root,"dev/jamovi/e2e/artifacts/inventory.json"),simplifyVector=FALSE))
  ids = unlist(lapply(manifest$scenarios,`[[`,"id"))
  expect_equal(length(ids),77)
  expect_false(anyDuplicated(ids)>0)
  for(analysis in inventory$analyses) for(option in analysis$options) expect_true(length(option$scenarios)>0,info=option$id)
  expect_true(all(vapply(manifest$scenarios,function(s)length(s$verification$interface)>0 &&
    length(s$verification$statistical)>0 && length(s$verification$visual)>0,logical(1))))
  expect_true(all(vapply(manifest$scenarios,function(s) isTRUE(s$save_reopen$required),logical(1))))
})

test_that("E2E reference results regenerate and are independent of flexplot computations", {
  directory = withr::local_tempdir()
  generated = suppressMessages(e2e_generate(root,directory))
  stored = file.path(root,"dev/jamovi/e2e/artifacts")
  expect_identical(generated$fixtures,readRDS(file.path(stored,"fixtures.rds")))
  expected = readRDS(file.path(stored,"references.rds"))
  mixed = startsWith(names(expected),"MM-")
  expect_equal(generated$references[mixed],expected[mixed],tolerance=1e-5)
  expect_equal(generated$references[!mixed],expected[!mixed],tolerance=1e-8)
  for(file in list.files(stored,pattern="\\.csv$"))
    expect_identical(readBin(file.path(directory,file),"raw",file.info(file.path(directory,file))$size),
      readBin(file.path(stored,file),"raw",file.info(file.path(stored,file))$size),info=file)
  code = paste(readLines(file.path(root,"dev/jamovi/e2e/fixtures.R")),collapse="\n")
  expect_false(grepl("flexplot:::|jamovi_.*fit\\(|compare_fits\\(",code))
})

test_that("independent reference models reproduce permanent common-sample and CAS oracles", {
  data = e2e_fixtures()$common_sample
  first = lme4::lmer(y~x+(1+x|id),data)
  common = data[61:240,]
  second = lme4::lmer(y~x+(1+x|id),common)
  third = lme4::lmer(y~x+z+(1+x|id),common)
  expect_equal(unname(lme4::fixef(first)["x"]),.6083895,tolerance=1e-6)
  expect_equal(unname(lme4::fixef(second)["x"]),.5152104,tolerance=1e-6)
  expect_equal(unname(lme4::fixef(third)["x"]),.5138160,tolerance=1e-6)
  refs = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))
  expect_equal(refs[["MM-10/1"]]$models[[1]]$n,240)
  expect_equal(refs[["MM-10/2"]]$models[[1]]$n,180)
  expect_equal(refs[["MM-10/2"]]$models[[1]]$groups,12)
  expect_equal(refs[["MM-10/2"]]$models[[1]]$adjusted_outcomes,
    unname(common$y-(predict(second,re.form=NULL)-predict(second,re.form=NA))),tolerance=1e-10)
  expect_equal(refs[["MM-10/2"]]$models[[1]]$conditional_predictions,unname(predict(second)),tolerance=1e-10)
})

test_that("representative real R6 fits agree with new independent fixture checkpoints", {
  fixtures = e2e_fixtures()
  refs = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"),simplifyVector=FALSE)
  for(id in c("LM-04","GLM-01","GLM-02","GLM-03","GLM-04","GLM-05","GLM-06","MM-02","MM-10")) {
    scenario = Filter(function(s)s$id==id,manifest$scenarios)[[1]]
    index = if(id=="MM-10") 2L else 1L
    values = scenario$steps[[index]]$expected_options
    for(name in intersect(c("preds","plotPanels","quadratic","randomSlopes","comparisonFixed",
        "comparisonQuadratic","comparisonRandomSlopes"),names(values))) values[name] = list(unlist(values[[name]],use.names=FALSE))
    for(name in intersect(c("modelTerms","comparisonTerms"),names(values))) values[name] = list(lapply(values[[name]],unlist,use.names=FALSE))
    values$graphic = FALSE
    options = do.call(get(paste0(scenario$analysis,"Options"),asNamespace("flexplot"))$new,values)
    analysis = get(paste0(scenario$analysis,"Class"),asNamespace("flexplot"))$new(options=options,data=fixtures[[scenario$dataset]])
    expect_true(analysis$run(),info=id)
    expected = refs[[paste0(id,"/",index)]]$models[[1]]
    if(scenario$analysis=="glmbasic") {
      expect_equal(analysis$results$glmcat$asDF$means,unname(expected$coefficients),tolerance=1e-10)
      expect_equal(analysis$results$glmcat$asDF$se,unname(expected$se),tolerance=1e-10)
    } else {
      model = analysis$results$plot$state$model
      beta = if(inherits(model,"merMod"))lme4::fixef(model) else coef(model)
      expect_equal(beta,expected$coefficients,tolerance=1e-10,info=id)
      expect_equal(sqrt(diag(as.matrix(vcov(model)))),expected$se,tolerance=1e-10,info=id)
      expect_equal(nobs(model),expected$n,info=id)
    }
  }
})

test_that("planned fitted checkpoint formulas match the actual current option semantics", {
  fixtures = e2e_fixtures()
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"),simplifyVector=FALSE)
  for(scenario in manifest$scenarios) for(i in seq_along(scenario$steps)) {
    checkpoint = scenario$steps[[i]]
    if(is.null(checkpoint$reference) || scenario$analysis=="flexplota") next
    values = checkpoint$expected_options
    for(name in intersect(c("preds","plotPanels","quadratic","randomSlopes","comparisonFixed",
        "comparisonQuadratic","comparisonRandomSlopes","comparisonPanels"),names(values))) values[name] = list(unlist(values[[name]],use.names=FALSE))
    for(name in intersect(c("modelTerms","comparisonTerms"),names(values))) values[name] = list(lapply(values[[name]],unlist,use.names=FALSE))
    options = do.call(get(paste0(scenario$analysis,"Options"),asNamespace("flexplot"))$new,values)
    data = fixtures[[scenario$dataset]]
    actual = switch(scenario$analysis,
      glmbasic=list(flexplot:::jamovi_lm_formula(options,data)),
      glmgeneralized=list(flexplot:::jamovi_glm_generalized_formula(options,data)),
      mixedmodel=list(flexplot:::jamovi_mixed_formula(options,data)$formula))
    if(isTRUE(options$compare)) actual[[2]] = switch(scenario$analysis,
      glmbasic=flexplot:::jamovi_lm_formula(options,data,second=TRUE),
      glmgeneralized=flexplot:::jamovi_glm_generalized_formula(options,data,options$comparisonTerms,options$comparisonQuadratic),
      mixedmodel=flexplot:::jamovi_mixed_formula(flexplot:::jamovi_mixed_second_options(options),data)$formula)
    expect_equal(length(actual),length(checkpoint$reference$formulas),info=paste(scenario$id,i))
    for(j in seq_along(actual)) expect_identical(
      attr(terms(actual[[j]]),"term.labels"),attr(terms(as.formula(checkpoint$reference$formulas[[j]])),"term.labels"),
      info=paste(scenario$id,i,j))
  }
})
