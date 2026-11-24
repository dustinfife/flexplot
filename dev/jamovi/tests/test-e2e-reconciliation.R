source(file.path(root,"dev/jamovi/e2e/generate.R"),local=TRUE)
source(file.path(root,"dev/jamovi/e2e/fixtures.R"),local=TRUE)
source(file.path(root,"dev/jamovi/e2e/harness.R"),local=TRUE)

e2e_current_manifest = function() jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))

test_that("coverage contracts reject invented edits and unsupported indirect mappings", {
  manifest = e2e_current_manifest();fixtures = e2e_fixtures()
  expect_true(e2e_validate_manifest(manifest,fixtures,root))
  index = which(vapply(manifest$scenarios,function(s)s$id=="GLM-24",logical(1)))
  invalid = manifest;invalid$scenarios[[index]]$steps[[1]]$changes$automaticFamily = "binomial"
  expect_error(e2e_validate_manifest(invalid,fixtures,root))
  invalid = manifest;invalid$scenarios[[index]]$steps[[1]]$required_observations = list()
  expect_error(e2e_validate_manifest(invalid,fixtures,root))
  invalid = manifest;invalid$scenarios[[index]]$indirect_coverage[[1]]$checkpoint = 999L
  expect_error(e2e_validate_manifest(invalid,fixtures,root))
  invalid = manifest;invalid$scenarios[[index]]$steps[[5]]$action = "set_options_through_native_controls"
  expect_error(e2e_validate_manifest(invalid,fixtures,root))
  inventory = e2e_inventory(root,manifest)
  expect_equal(sum(vapply(inventory$analyses,function(a)length(a$options),integer(1))),150L)
  # An option cannot stay covered once its real action and indirect contract are removed.
  removed = manifest
  for(i in seq_along(removed$scenarios)) if(removed$scenarios[[i]]$analysis=="mixedmodel") {
    s = removed$scenarios[[i]]
    s$initialization$options$comparisonClusters = NULL
    for(j in seq_along(s$steps)) s$steps[[j]]$changes$comparisonClusters = NULL
    s$indirect_coverage = Filter(function(c)c$option!="comparisonClusters",s$indirect_coverage)
    removed$scenarios[[i]] = s
  }
  options = e2e_inventory(root,removed)$analyses$mixedmodel$options
  expect_length(Filter(function(o)o$definition$name=="comparisonClusters",options)[[1]]$scenarios,0)
  expect_identical(table(vapply(manifest$scenarios,`[[`,character(1),"analysis")),
    table(rep(c("flexplota","glmbasic","glmgeneralized","mixedmodel"),c(16,17,24,20))))
  refs = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))
  keys = unlist(lapply(manifest$scenarios,function(s) {
    indices = which(vapply(s$steps,function(st)!is.null(st$reference),logical(1)))
    if(length(indices)) paste0(s$id,"/",indices) else character()
  }),use.names=FALSE)
  expect_identical(keys,names(refs));expect_length(keys,156L)
})

test_that("metadata-only generation preserves numerical and visual artifacts", {
  directory = withr::local_tempdir()
  e2e_generate_metadata(root,directory)
  expect_setequal(list.files(directory),c("inventory.json","coverage.csv","scenario-index.csv"))
  for(file in list.files(directory)) expect_identical(readBin(file.path(directory,file),"raw",file.info(file.path(directory,file))$size),
    readBin(file.path(root,"dev/jamovi/e2e/artifacts",file),"raw",file.info(file.path(root,"dev/jamovi/e2e/artifacts",file))$size))
})

test_that("manifest legacy inputs restore through installed real protobuf and migrate idempotently", {
  jmvcore:::initProtoBuf()
  fixtures = e2e_fixtures()
  for(scenario in e2e_current_manifest()$scenarios) for(step in scenario$steps) {
    if(step$action!="restore_saved_options") next
    pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    before = step$restoration$saved_options
    pb$names = names(before)
    pb$options = lapply(before,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    constructor = get(paste0(scenario$analysis,"Options"),asNamespace("flexplot"))
    options = constructor$new();options$fromProtoBuf(pb)
    expect_setequal(options$asProtoBuf()$names,names(before))
    if(scenario$analysis=="glmgeneralized") flexplot:::jamovi_glm_family_restore(options)
    flexplot:::jamovi_output_migrate(options,scenario$analysis)
    flexplot:::jamovi_plot_editor_migrate(options,scenario$analysis)
    target = if(scenario$analysis=="glmbasic") 1L else 2L
    expect_equal(options$outputOptionsVersion,target,info=scenario$id)
    expect_equal(options$plotLabelEditorVersion,1L,info=scenario$id)
    if(scenario$analysis=="glmgeneralized") expect_identical(options$family,"gaussian")
    if(scenario$analysis=="mixedmodel" && isTRUE(before$outputOptionsVersion<2)) {
      expect_equal(options$comparisonX,options$plotX)
      expect_equal(options$comparisonPanels,options$plotPanels)
      expect_equal(options$comparisonPredictionType,options$predictionType)
      expect_equal(options$comparisonClusters,options$clusters)
    }
    if(!is.null(before$includeCI)) expect_identical(options$includeCI,before$includeCI)
    if(scenario$analysis=="glmbasic") {
      if("line" %in% names(before)) expect_identical(options$line,before$line)
      if("center" %in% names(before)) expect_identical(options$center,before$center)
    }
    reopened = constructor$new();reopened$fromProtoBuf(options$asProtoBuf())
    flexplot:::jamovi_output_migrate(reopened,scenario$analysis)
    expect_equal(reopened$outputOptionsVersion,options$outputOptionsVersion)
    expect_equal(reopened$plotLabelStore,options$plotLabelStore)
    expect_equal(reopened$comparisonPlotLabelStore,options$comparisonPlotLabelStore)
    if(length(before$plotLabelStore)) {
      expect_true(options$modifyPlotLabels)
      expect_equal(reopened$plotLabelStore,before$plotLabelStore)
      if(scenario$analysis=="mixedmodel") {
        expect_true(options$modifyComparisonPlotLabels)
        expect_equal(reopened$comparisonPlotLabelStore,before$plotLabelStore)
      }
    }
  }
})

test_that("derived Automatic checkpoints agree with independently fitted fixture outcomes", {
  fixtures = e2e_fixtures()
  scenario = Filter(function(s)s$id=="GLM-24",e2e_current_manifest()$scenarios)[[1]]
  for(index in c(1L,3L,4L)) {
    step = scenario$steps[[index]];values = step$expected_options
    values$preds = unlist(values$preds,use.names=FALSE)
    values$modelTerms = lapply(values$modelTerms,unlist,use.names=FALSE)
    values$graphic = FALSE
    analysis = jamovi_reporting_host(do.call(flexplot:::glmgeneralizedOptions$new,values),fixtures$primary)
    expect_true(analysis$run())
    model = analysis$results$plot$state$model
    family = if(values$family=="auto")"binomial" else "gaussian"
    reference = glm(as.formula(paste(values$out,"~x+z+binary")),fixtures$primary,family=family)
    expect_identical(model$family$family,family)
    expect_identical(model$family$link,reference$family$link)
    expect_equal(coef(model),coef(reference),tolerance=1e-10)
    expect_equal(vcov(model),vcov(reference),tolerance=1e-10)
    if(values$family=="auto") {
      expect_identical(analysis$options$automaticFamily,"binomial")
      expect_identical(analysis$options$automaticFamilyOutcome,values$out)
      expect_identical(analysis$options$family,"auto")
    }
  }
})

test_that("MM-19 native transitions preserve intended main fit independently of comparison display", {
  scenario = Filter(function(s)s$id=="MM-19",e2e_current_manifest()$scenarios)[[1]]
  convert = function(values) {
    for(name in intersect(c("preds","plotPanels","comparisonPanels","randomSlopes","comparisonRandomSlopes"),names(values)))
      values[name] = list(unlist(values[[name]],use.names=FALSE))
    for(name in intersect(c("modelTerms","comparisonTerms"),names(values)))
      values[name] = list(lapply(values[[name]],unlist,use.names=FALSE))
    values
  }
  options = do.call(flexplot:::mixedmodelOptions$new,convert(scenario$steps[[1]]$expected_options))
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=e2e_fixtures()$primary),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  reference = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))[["MM-07/1"]]
  intended = analysis$results$plot$state$model
  expect_true(lme4::isREML(intended))
  expect_equal(lme4::fixef(intended),reference$models[[1]]$coefficients,tolerance=1e-10)
  expect_equal(as.matrix(vcov(intended)),reference$models[[1]]$vcov,tolerance=1e-10)
  for(index in c(3:10)) {
    step = scenario$steps[[index]]
    expect_true(jamovi_reporting_update(analysis,convert(step$changes)))
    for(name in names(step$expected_result_visibility))
      expect_identical(analysis$results[[name]]$visible,step$expected_result_visibility[[name]],info=paste(index,name))
    if(isTRUE(options$graphic)) {
      expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
      expect_identical(analysis$results$plot$state$model,intended)
      expect_identical(analysis$results$plot$state$displayLayout$type,options$predictionType)
    }
    if(isTRUE(options$comparisonGraphic))
      expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot),
        info=paste(index,analysis$results$comparisonplot$.__enclos_env__$private$.error))
    if(isTRUE(options$estimates))
      expect_equal(analysis$results$fixed$asDF$estimate,unname(reference$models[[1]]$coefficients),tolerance=1e-10)
  }
})

test_that("MM-15 actual current protobuf layouts fail locally and recover without stale images", {
  scenario = Filter(function(s)s$id=="MM-15",e2e_current_manifest()$scenarios)[[1]]
  jmvcore:::initProtoBuf()
  pb = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
  template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
  options = flexplot:::mixedmodelOptions$new()
  analysis = flexplot:::mixedmodelClass$new(options=options,data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=e2e_fixtures()$primary),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  for(index in seq_along(scenario$steps)) {
    # These are current host states, not legacy shared-plot migration inputs.
    values = c(scenario$steps[[index]]$expected_options,list(outputOptionsVersion=2L,plotLabelEditorVersion=1L))
    pb$names = names(values)
    pb$options = lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    options$fromProtoBuf(pb)
    analysis$optionsChangedHandler(names(values))
    expect_true(analysis$run())
    expect_gt(analysis$results$fixed$rowCount,0)
    main = analysis$results$plot;comparison = analysis$results$comparisonplot
    if(index==1L) {
      expect_match(main$.__enclos_env__$private$.error,"place the grouping variable")
      expect_null(main$state)
    } else if(index==2L) {
      expect_identical(main$.__enclos_env__$private$.error,"")
      expect_true(analysis$.__enclos_env__$private$.plot(main))
      expect_match(comparison$.__enclos_env__$private$.error,"place the shared grouping variable")
      expect_null(comparison$state)
      expect_gt(analysis$results$comparison$rowCount,0)
    } else {
      expect_identical(main$.__enclos_env__$private$.error,"")
      expect_identical(comparison$.__enclos_env__$private$.error,"")
      expect_true(analysis$.__enclos_env__$private$.plot(main))
      expect_true(analysis$.__enclos_env__$private$.comparisonplot(comparison))
      reference = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))[["MM-15/3"]]
      expect_equal(analysis$results$fixed$asDF$estimate,unname(reference$models[[1]]$coefficients),tolerance=1e-10)
    }
  }
})
