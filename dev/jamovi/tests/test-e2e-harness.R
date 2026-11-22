source(file.path(root,"dev/jamovi/e2e/harness.R"),local=TRUE)

e2e_synthetic_observation = function(scenario,index=1L) {
  checkpoint = scenario$steps[[index]]
  list(source="native_gui",checkpoint_id=paste0(scenario$id,"/",index),analysis_id=2L,
    desktop=list(secondary_monitor=TRUE,window_count=1L,owned_document=TRUE),
    accessibility="Synthetic fixture for validator tests; not evidence of actual GUI execution",
    native_status="complete",visible_options=checkpoint$expected_options,
    rendered_images=list(plot=TRUE),saved=list(source="saved_gui_document",sha256="synthetic-fixture",
      analyses=list(list(id=2L,name=scenario$analysis,status="ANALYSIS_COMPLETE",error="",revision=0L,
        options=checkpoint$expected_options,results=list(name="results",type="group",status="ANALYSIS_COMPLETE",error="",
          stale=FALSE,visible="YES",children=list(list(name="plot",type="image",status="ANALYSIS_COMPLETE",stale=FALSE,
            error="",visible="YES",resource_exists=TRUE)))))))
}

test_that("harness fails controlled visible-option, numerical, missing/stale and restoration faults", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
  scenario = Filter(function(s)s$id=="FP-03",manifest$scenarios)[[1]]
  good = e2e_synthetic_observation(scenario)
  expect_identical(e2e_checkpoint(scenario,1L,good)$status,"passed")
  bad = good;bad$visible_options$preds = list("wrong")
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"failed")
  bad = good;bad$saved$analyses[[1]]$results$children = list()
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"failed")
  bad = good;bad$native_status = "running"
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"running")
  bad = good;bad$checkpoint_id = paste0(scenario$id,"/1/restored")
  expect_identical(e2e_checkpoint(scenario,1L,bad,restored=TRUE)$status,"blocked")
  bad$reopened_in_gui = TRUE
  bad$saved$analyses[[1]]$options$preds = list("lost-on-reopen")
  bad$visible_options$preds = list("lost-on-reopen")
  restored_result = e2e_checkpoint(scenario,1L,bad,restored=TRUE)
  expect_identical(restored_result$status,"failed")
  expect_match(paste(restored_result$problems,collapse=";"),"Saved option mismatch")
  expect_identical(e2e_checkpoint(scenario,1L,NULL)$status,"blocked")
  stale = e2e_synthetic_observation(scenario,2L)
  previous = e2e_checkpoint(scenario,1L,good)
  stale_result = e2e_checkpoint(scenario,2L,stale,previous=previous)
  expect_identical(stale_result$status,"failed")
  expect_match(paste(stale_result$problems,collapse=";"),"Stale saved result")
  lm_case = Filter(function(s)s$id=="LM-04",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(lm_case)
  table = list(name="glmcat",type="table",status="ANALYSIS_COMPLETE",error="",visible="YES",stale=FALSE,
    columns=list(list(name="var",values=list("(Intercept)","x")),list(name="means",values=list(99,99)),
      list(name="se",values=list(1,1)),list(name="lower",values=list(-1,-1)),list(name="upper",values=list(1,1))))
  observation$saved$analyses[[1]]$results$children[[2]] = table
  reference = list(models=list(list(coefficient_names=c("(Intercept)","x"),coefficients=c(0,1),se=c(1,1),lower=c(-1,-1),upper=c(1,1))))
  expect_identical(e2e_checkpoint(lm_case,1L,observation,reference)$status,"failed")
  observation$saved$analyses[[1]]$results$children[[2]]$columns[[2]]$values = list(0,1)
  expect_identical(e2e_checkpoint(lm_case,1L,observation,reference)$status,"passed")
})

test_that("runner times out on persistent waiting without calling arbitrary completion success", {
  scenario = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))$scenarios[[3]]
  clock = new.env(parent=emptyenv());clock$value = 0
  driver = list(act=function(request)NULL,observe=function(request)NULL)
  directory = withr::local_tempdir()
  result = e2e_run_scenario(scenario,driver,list(),directory,timeout=2,
    now=function()clock$value,poll=function(){clock$value=clock$value+1})
  expect_identical(result$status,"blocked")
  expect_gt(result$not_executed,0)
  record = jsonlite::read_json(file.path(directory,"01-result.json"))
  expect_match(record$result$reason,"Timeout")
  expect_true(file.exists(file.path(directory,"summary.txt")))
})

test_that("pilot inventory and saved inspector exclude live backend mutation", {
  expect_true(file.exists(file.path(root,"dev/jamovi/e2e/inspect-omv.py")))
  expect_true(all(c("FP-03","LM-03","LM-09","GLM-08","MM-10","MM-11") %in%
    vapply(jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))$scenarios,`[[`,character(1),"id")))
  code = paste(readLines(file.path(root,"dev/jamovi/e2e/inspect-omv.py")),collapse="\n")
  expect_false(grepl("websocket|set_options|AnalysisRequest",code))
})

test_that("real installed protocol decoding and native driver verification pass without GUI actions", {
  home = Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app")
  withr::local_envvar(c(PYTHONHOME=file.path(home,"Contents/Resources"),
    PYTHONPATH=file.path(home,"Contents/Resources/jamovi/server")))
  expect_equal(system2(file.path(home,"Contents/MacOS/python"),shQuote(file.path(root,"dev/jamovi/e2e/check-inspector.py"))),0)
  node = Sys.which("node")
  if(!nzchar(node)) node = tail(Sys.glob(path.expand("~/.nvm/versions/node/*/bin/node")),1)
  expect_equal(system2(node,shQuote(file.path(root,"dev/jamovi/e2e/check-cua-driver.cjs"))),0)
})

test_that("driver failures are recorded as blocked and later checkpoints remain unexecuted", {
  scenario = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))$scenarios[[3]]
  directory = withr::local_tempdir()
  result = e2e_run_scenario(scenario,list(act=function(request)stop("native target unavailable"),
    observe=function(request)stop("must not inspect after blocked action")),list(),directory)
  expect_identical(result$status,"blocked")
  expect_identical(result$attempted_checkpoints,1L)
  expect_identical(result$passed_checkpoints,0L)
  expect_gt(result$not_executed,0)
  expect_match(jsonlite::read_json(file.path(directory,"01-result.json"))$result$reason,"native target unavailable")
})

test_that("the CLI returns nonzero for injected option, numerical and missing-result faults", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
  scenario = Filter(function(s)s$id=="LM-04",manifest$scenarios)[[1]]
  reference = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))[["LM-04/1"]]$models[[1]]
  base = e2e_synthetic_observation(scenario)
  columns = list(var=as.list(reference$coefficient_names),means=as.list(unname(reference$coefficients)),
    se=as.list(unname(reference$se)),lower=as.list(unname(reference$lower)),upper=as.list(unname(reference$upper)))
  base$saved$analyses[[1]]$results$children[[2]] = list(name="glmcat",type="table",status="ANALYSIS_COMPLETE",
    stale=FALSE,error="",visible="YES",columns=lapply(names(columns),function(name)list(name=name,values=columns[[name]])))
  directory = withr::local_tempdir()
  for(kind in c("valid","visible","numerical","missing")) {
    observation = base
    if(kind=="visible") observation$visible_options$out = "incorrect"
    if(kind=="numerical") observation$saved$analyses[[1]]$results$children[[2]]$columns[[2]]$values[[1]] = 999
    if(kind=="missing") observation$saved$analyses[[1]]$results$children = list()
    input = file.path(directory,paste0(kind,".json"))
    jsonlite::write_json(observation,input,auto_unbox=TRUE,null="null",digits=NA)
    log = file.path(directory,paste0(kind,".log"))
    status = suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),shQuote(c("--vanilla",
      file.path(root,"dev/jamovi/e2e/run.R"),"verify","LM-04","1",input)),stdout=log,stderr=log))
    expect_identical(as.integer(status),if(kind=="valid")0L else 1L,info=paste(readLines(log,warn=FALSE),collapse="\n"))
    result = jsonlite::read_json(log)
    expect_identical(result$status,if(kind=="valid")"passed" else "failed")
    if(kind!="valid") expect_match(paste(result$problems,collapse=";"),switch(kind,
      visible="Visible/saved mismatch",numerical="Incorrect coefficients",missing="Missing visible plot"))
  }
})

test_that("module-version preflight blocks missing or stale installations before GUI actions", {
  directory = withr::local_tempdir()
  path = file.path(directory,"installed.yaml")
  expect_identical(e2e_module_preflight(root,path)$status,"blocked")
  yaml::write_yaml(list(version="stale",`build-hash`="controlled-fixture"),path)
  expect_identical(e2e_module_preflight(root,path)$status,"blocked")
  current = yaml::read_yaml(file.path(root,"jamovi/0000.yaml"))$version
  yaml::write_yaml(list(version=current,`build-hash`="controlled-fixture"),path)
  expect_identical(e2e_module_preflight(root,path)$status,"blocked")
  expect_match(e2e_module_preflight(root,path)$reason,"receipt|stamp|R package")
})

test_that("native option deltas reject collateral edits and identify initial host context", {
  before = list(analyses=list(list(id=8L,options=list(modelTerms=list("x","z"),compare=TRUE))))
  after = before;after$analyses[[1]]$options$modelTerms = list("x")
  expect_identical(e2e_option_delta(before,after,8L,"modelTerms")$status,"passed")
  after$analyses[[1]]$options$compare = FALSE
  expect_identical(e2e_option_delta(before,after,8L,"modelTerms")$status,"failed")
  after = before;after$analyses[[1]]$options$.ppi = 72
  expect_identical(e2e_option_delta(before,after,8L,"modelTerms")$status,"failed")
  expect_identical(e2e_option_delta(before,after,8L,"modelTerms",allow_host_initialization=TRUE)$host_initializations,".ppi")
})

test_that("synchronized source versions and reproducible build identities are enforced", {
  source(file.path(root,"dev/jamovi/build-identity.R"),local=TRUE)
  versions = jamovi_dev_versions(root)
  expect_identical(versions$package,versions$module)
  directory = withr::local_tempdir();dir.create(file.path(directory,"jamovi"))
  writeLines(c("Package: flexplot","Version: 0.27.0"),file.path(directory,"DESCRIPTION"))
  yaml::write_yaml(list(version="0.12.3"),file.path(directory,"jamovi/0000.yaml"))
  expect_error(jamovi_dev_versions(directory),"synchronization error")
  yaml::write_yaml(list(version="0.27.0"),file.path(directory,"jamovi/0000.yaml"))
  expect_identical(jamovi_dev_versions(directory)$module,"0.27.0")
  node = Sys.which("node")
  if(!nzchar(node)) node = tail(Sys.glob(path.expand("~/.nvm/versions/node/*/bin/node")),1)
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  expect_equal(system2(node,shQuote(c(file.path(root,"dev/jamovi/check-build-identity.cjs"),compiler))),0)
})


test_that("the running GUI must serve the verified installed build", {
  installed = list(status="passed",installed="0.27.0",build_hash="source-sha256-current")
  expect_identical(e2e_served_preflight(installed,NULL)$status,"blocked")
  expect_identical(e2e_served_preflight(installed,list(version="0.12.3",`build-hash`="old"))$status,"blocked")
  expect_identical(e2e_served_preflight(installed,list(version="0.27.0",`build-hash`="old"))$status,"blocked")
  expect_true(e2e_served_preflight(installed,list(version="0.27.0",`build-hash`="source-sha256-current"))$running_gui_verified)
  expect_identical(e2e_running_preflight(installed,"https://example.org/modules/flexplot")$status,"blocked")
})

test_that("restoration permits only an explicitly owned second secondary-monitor window", {
  original = list(id="native-instance-1",title="primary",document="/test/primary.omv",
    source="native_gui",secondary_monitor=TRUE,disposable=TRUE)
  first = e2e_register_window(list(),original)
  expect_identical(first$status,"passed")
  second = original;second$id = "native-instance-2";second$title = "unique-restoration";second$document = "/test/unique-restoration.omv"
  expect_identical(e2e_register_window(first$registry,second)$status,"blocked")
  next_window = e2e_register_window(first$registry,second,"save_reopen")
  expect_identical(next_window$status,"passed")
  expect_identical(e2e_authorize_window_close(next_window$registry,original)$status,"blocked")
  expect_identical(e2e_authorize_window_close(next_window$registry,second)$status,"passed")
  wrong = second;wrong$title = "user document"
  expect_identical(e2e_authorize_window_close(next_window$registry,wrong)$status,"blocked")
  wrong = second;wrong$secondary_monitor = FALSE
  expect_identical(e2e_register_window(first$registry,wrong,"save_reopen")$status,"blocked")
  expect_identical(e2e_authorize_window_close(next_window$registry,wrong)$status,"blocked")
  third = second;third$id = "native-instance-3";third$title = "third"
  expect_identical(e2e_register_window(next_window$registry,third,"save_reopen")$status,"blocked")
  expect_identical(e2e_register_window(first$registry,original,"save_reopen")$status,"blocked")
  desktop = list(secondary_monitor=TRUE,owned_document=TRUE,window_count=2L,
    active_window_id=second$id,window_registry=next_window$registry)
  expect_true(e2e_desktop_verified(desktop))
  desktop$window_registry[[2]]$purpose = NULL
  expect_false(e2e_desktop_verified(desktop))
  desktop$window_registry = NULL
  expect_false(e2e_desktop_verified(desktop))
  scenario = Filter(function(s)s$id=="FP-03",jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))$scenarios)[[1]]
  observation = e2e_synthetic_observation(scenario)
  observation$desktop = list(secondary_monitor=TRUE,owned_document=TRUE,window_count=2L,
    active_window_id=second$id,window_registry=next_window$registry)
  expect_identical(e2e_checkpoint(scenario,1L,observation)$status,"passed")
  observation$desktop$active_window_id = "unrelated"
  expect_identical(e2e_checkpoint(scenario,1L,observation)$status,"blocked")
})

test_that("preparing a pilot never silently overwrites a prior document", {
  directory = withr::local_tempdir()
  script = file.path(directory,"dev/jamovi/e2e/build-pilot.R")
  dir.create(dirname(script),recursive=TRUE)
  file.copy(file.path(root,"dev/jamovi/e2e/build-pilot.R"),script)
  output = file.path(directory,".jamovi-dev/e2e-pilot/primary.omv")
  dir.create(dirname(output),recursive=TRUE)
  writeBin(charToRaw("preserve this disposable evidence"),output)
  before = tools::md5sum(output)
  log = tempfile()
  status = suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),shQuote(c("--vanilla",script)),stdout=log,stderr=log))
  expect_identical(as.integer(status),1L)
  expect_match(paste(readLines(log),collapse="\n"),"documents already exist")
  expect_identical(tools::md5sum(output),before)
})

test_that("diagnostic checkpoints require observable text and reject error-only empty HTML", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
  scenario = Filter(function(s)s$id=="GLM-18",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(scenario)
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds="x",family="binomial",graphic=FALSE),jamovi_diagnostic_data())
  expect_true(analysis$run())
  item = analysis$results$explanation
  message = item$.__enclos_env__$private$.error
  observation$native_status = "error"
  observation$saved$analyses[[1]]$results$children = list(list(name="explanation",type="html",status="ANALYSIS_ERROR",
    error=message,content=item$content,visible="YES"))
  observation$diagnostics = list(list(result="explanation",phase="fitting_error",visible=TRUE,text=message,source="renderer_dom"))
  checked = e2e_checkpoint(scenario,1L,observation)
  expect_identical(checked$status,"passed")
  expect_identical(checked$diagnostic_phase,"fitting_error")
  caught = observation;caught$native_status = "complete" # R computation completed; the fitted model did not.
  expect_identical(e2e_checkpoint(scenario,1L,caught)$status,"passed")
  expect_identical(e2e_checkpoint(scenario,1L,caught)$diagnostic_phase,"fitting_error")
  alternate = scenario
  alternate$steps[[1]]$expected_warnings_or_errors = list()
  alternate$steps[[1]]$expected_diagnostic_text_any = c("unseen alternative","numeric 0/1")
  expect_identical(e2e_checkpoint(alternate,1L,observation)$status,"passed")
  alternate$steps[[1]]$expected_diagnostic_text_any = "unseen alternative"
  expect_identical(e2e_checkpoint(alternate,1L,observation)$status,"failed")
  bad = observation;bad$diagnostics = NULL
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"blocked")
  # The saved error exists, but the installed HTML renderer added no visible text.
  bad = observation;bad$saved$analyses[[1]]$results$children[[1]]$content = ""
  bad$diagnostics[[1]]$text = "";bad$diagnostics[[1]]$visible = FALSE
  checked = e2e_checkpoint(scenario,1L,bad)
  expect_identical(checked$status,"failed")
  expect_match(paste(checked$problems,collapse=";"),"not visibly rendered")
  bad = observation;bad$diagnostics[[1]]$text = "An unrelated message"
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"failed")
  bad = observation;bad$diagnostics[[1]]$source = "saved_gui_document"
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"blocked")
  bad = observation;bad$diagnostics[[1]]$phase = "rendering_error"
  expect_identical(e2e_checkpoint(scenario,1L,bad)$status,"failed")
  render = observation
  render$diagnostics[[1]]$phase = "rendering_error";render$diagnostics[[1]]$result = "plot"
  render$saved$analyses[[1]]$results$children[[1]]$name = "plot"
  render$saved$analyses[[1]]$results$children[[1]]$type = "image"
  render_scenario = scenario;render_scenario$steps[[1]]$expected_diagnostic_phase = "rendering_error"
  expect_identical(e2e_checkpoint(render_scenario,1L,render)$diagnostic_phase,"rendering_error")
  expect_identical(e2e_checkpoint(render_scenario,1L,render)$status,"passed")
  waiting = Filter(function(s)s$id=="GLM-07",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(waiting)
  observation$native_status = "waiting"
  observation$saved$analyses[[1]]$results$children = list(list(name="waiting",type="html",status="ANALYSIS_COMPLETE",
    error="",visible="YES",content="<p>Select an outcome and predictors.</p>"))
  observation$diagnostics = list(list(result="waiting",phase="waiting",visible=TRUE,
    text="Select an outcome and predictors.",source="accessibility"))
  expect_identical(e2e_checkpoint(waiting,1L,observation)$diagnostic_phase,"waiting")
  expect_identical(e2e_checkpoint(waiting,1L,observation)$status,"passed")
  warning = Filter(function(s)s$id=="MM-13",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(warning)
  observation$saved$analyses[[1]]$results$children = list(list(name="diagnostics",type="html",status="ANALYSIS_COMPLETE",error="",visible="YES"))
  observation$diagnostics = list(list(result="diagnostics",phase="warning",visible=TRUE,text="Singular fit detected",source="accessibility"))
  expect_identical(e2e_checkpoint(warning,1L,observation)$status,"passed")
  good = Filter(function(s)s$id=="FP-03",manifest$scenarios)[[1]]
  expect_identical(e2e_checkpoint(good,1L,e2e_synthetic_observation(good))$diagnostic_phase,"success")
})

test_that("GLM output checkpoints reject missing or incorrectly visible sections", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
  scenario = Filter(function(s)s$id=="GLM-02",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(scenario,7L)
  for (name in c("coefficients","irt","probabilities"))
    observation$saved$analyses[[1]]$results$children[[length(observation$saved$analyses[[1]]$results$children)+1L]] =
      list(name=name,type="table",status="ANALYSIS_COMPLETE",error="",visible="NO")
  expect_identical(e2e_checkpoint(scenario,7L,observation)$status,"passed")
  bad = observation;bad$saved$analyses[[1]]$results$children[[2]]$visible = "YES"
  expect_identical(e2e_checkpoint(scenario,7L,bad)$status,"failed")
  bad = observation;bad$saved$analyses[[1]]$results$children[[2]] = NULL
  expect_identical(e2e_checkpoint(scenario,7L,bad)$status,"failed")
})

test_that("panel-ghost checkpoints cannot pass from saved options alone", {
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
  scenario = Filter(function(s)s$id=="GLM-22",manifest$scenarios)[[1]]
  observation = e2e_synthetic_observation(scenario,2L)
  expect_identical(e2e_checkpoint(scenario,2L,observation)$status,"blocked")
  # Controlled review inputs test the validator; these are not GUI evidence.
  observation$panel_ghosts = list(source="native_gui_review",evidence="Controlled validator fixture, not a real GUI review",visible=TRUE,
    source_preserved=TRUE,no_source_duplicate=TRUE,destinations_verified=TRUE,source_coordinates_verified=TRUE,scale="probability")
  expect_identical(e2e_checkpoint(scenario,2L,observation)$status,"passed")
  for (key in c("source_preserved","no_source_duplicate","destinations_verified","source_coordinates_verified")) {
    bad=observation;bad$panel_ghosts[[key]]=FALSE
    expect_identical(e2e_checkpoint(scenario,2L,bad)$status,"failed")
  }
  bad=observation;bad$panel_ghosts$source="reconstructed_R6"
  expect_identical(e2e_checkpoint(scenario,2L,bad)$status,"blocked")
  bad=observation;bad$panel_ghosts$scale="logit"
  expect_identical(e2e_checkpoint(scenario,2L,bad)$status,"failed")
})
