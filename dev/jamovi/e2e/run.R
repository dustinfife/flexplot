#!/usr/bin/env Rscript
script = gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1]),fixed=TRUE)
root = normalizePath(file.path(dirname(script),"../../.."))
source(file.path(root,"dev/jamovi/e2e/harness.R"))
args = commandArgs(TRUE)
if(!length(args)) stop("Use: run.R plan [ID] | inspect FILE | run ID BRIDGE_DIR [TIMEOUT] GUI_MODULE_ENDPOINT | verify ID STEP OBSERVATION_JSON")
manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
references = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/references.rds"))
command = args[1]
if(command=="inspect") {
  cat(jsonlite::toJSON(e2e_inspect_document(root,args[2]),auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA),"\n")
} else if(command=="plan") {
  cases = Filter(function(s)if(length(args)>1) s$id==args[2] else s$id %in% e2e_pilot_ids,manifest$scenarios)
  if(!length(cases)) stop("Unknown scenario")
  cat(jsonlite::toJSON(cases,auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA),"\n")
} else {
  cases = Filter(function(s)s$id==args[2],manifest$scenarios)
  if(length(cases)!=1L) stop("Choose exactly one valid scenario; the full campaign is disabled")
  scenario = cases[[1]]
  if(command=="verify") {
    index = as.integer(args[3]);observation = jsonlite::read_json(args[4])
    if(!is.null(observation$saved_file)) observation$saved = e2e_inspect_document(root,observation$saved_file)
    result = e2e_checkpoint(scenario,index,observation,references[[paste0(scenario$id,"/",index)]],restored=isTRUE(observation$reopened_in_gui))
    cat(jsonlite::toJSON(result,auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA),"\n")
    if(result$status!="passed") quit(status=1)
  } else if(command=="run") {
    preflight = e2e_module_preflight(root)
    preflight = e2e_running_preflight(preflight,if(length(args)>4)args[5] else NULL)
    if(preflight$status!="passed") {
      cat(jsonlite::toJSON(preflight,pretty=TRUE,auto_unbox=TRUE),"\n")
      quit(status=2)
    }
    bridge = normalizePath(args[3],mustWork=FALSE)
    output = tempfile(paste0(scenario$id,"-"),tmpdir=file.path(root,".jamovi-dev/e2e-runs"))
    dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
    driver = e2e_file_driver(bridge)
    observe = driver$observe
    driver$observe = function(request) {
      observation = observe(request)
      if(!is.null(observation$saved_file) && file.exists(observation$saved_file))
        observation$saved = e2e_inspect_document(root,observation$saved_file)
      observation
    }
    source(file.path(root,"dev/jamovi.R"))
    lock = jamovi_dev_lock(root)
    result = tryCatch(e2e_run_scenario(scenario,driver,references,output,
      timeout=if(length(args)>3)as.numeric(args[4]) else 90),finally=unlink(lock,recursive=TRUE))
    cat(jsonlite::toJSON(result,auto_unbox=TRUE,pretty=TRUE,digits=NA),"\nReport:",output,"\n")
    if(result$status!="passed") quit(status=1)
  } else stop("Unknown command")
}
