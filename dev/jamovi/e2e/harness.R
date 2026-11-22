# Manifest-driven orchestration. GUI actions are supplied by a native-control driver,
# never by reconstructing an R6 analysis or setting server options directly.
e2e_pilot_ids = c("FP-03","LM-03","LM-09","GLM-08","MM-10","MM-11")

e2e_inspect_document = function(root, path, home=Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app")) {
  if (!file.exists(path)) stop("Saved GUI document not found: ",path)
  previous = Sys.getenv(c("PYTHONHOME","PYTHONPATH"),unset=NA_character_)
  on.exit(for(name in names(previous)) if(is.na(previous[[name]])) Sys.unsetenv(name) else
    do.call(Sys.setenv,stats::setNames(list(previous[[name]]),name)),add=TRUE)
  Sys.setenv(PYTHONHOME=file.path(home,"Contents/Resources"),PYTHONPATH=file.path(home,"Contents/Resources/jamovi/server"))
  output = tempfile();on.exit(unlink(output),add=TRUE)
  status = system2(file.path(home,"Contents/MacOS/python"),shQuote(c(file.path(root,"dev/jamovi/e2e/inspect-omv.py"),normalizePath(path))),stdout=output)
  if(status != 0L) stop("Could not inspect saved jamovi state")
  jsonlite::read_json(output,simplifyVector=FALSE)
}

e2e_elements = function(node) {
  result = list(node)
  for(child in node$children) result = c(result,e2e_elements(child))
  result
}

e2e_option_equal = function(first, second) {
  # jsonlite's one-element arrays and R character vectors represent the same option.
  canonical = function(value) {
    if(is.null(value)) return(NULL)
    if(!is.list(value) && length(value)<=1L) return(unname(value))
    if(!is.list(value)) value = unname(as.list(value))
    lapply(value,canonical)
  }
  identical(canonical(first),canonical(second))
}

e2e_checkpoint = function(scenario, index, observation, reference=NULL, previous=NULL, restored=FALSE) {
  checkpoint = scenario$steps[[index]]
  problems = character();missing = character()
  if(is.null(observation)) return(list(status="blocked",reason="No native GUI observation supplied"))
  if(!identical(observation$source,"native_gui")) missing = c(missing,"native GUI provenance")
  if(!e2e_desktop_verified(observation$desktop)) missing = c(missing,"verified disposable secondary-monitor window ownership")
  if(!nzchar(paste(observation$accessibility,collapse=""))) missing = c(missing,"native accessibility evidence")
  if(!identical(observation$checkpoint_id,paste0(scenario$id,"/",index,if(restored)"/restored")))
    problems = c(problems,"Observation belongs to a different checkpoint")
  snapshot = observation$saved
  if(is.null(snapshot) || !identical(snapshot$source,"saved_gui_document")) missing = c(missing,"saved host state")
  if(isTRUE(restored) && !isTRUE(observation$reopened_in_gui)) missing = c(missing,"actual save/close/reopen")
  candidates = Filter(function(a)identical(a$name,scenario$analysis) && a$id==observation$analysis_id,snapshot$analyses)
  if(length(candidates)!=1L) return(list(status="blocked",reason="Unique saved analysis could not be identified"))
  analysis = candidates[[1]]
  for(name in names(checkpoint$expected_options)) {
    expected = checkpoint$expected_options[[name]]
    if(!e2e_option_equal(analysis$options[[name]],expected)) problems = c(problems,paste("Saved option mismatch:",name))
    if(!name %in% names(observation$visible_options) && !identical(observation$control_visibility[[name]],FALSE))
      missing = c(missing,paste("visible option",name)) else if(name %in% names(observation$visible_options))
      if(!e2e_option_equal(observation$visible_options[[name]],analysis$options[[name]]))
        problems = c(problems,paste("Visible/saved mismatch:",name))
  }
  changed = !is.null(previous) && any(vapply(names(checkpoint$expected_options),function(name)
    !e2e_option_equal(checkpoint$expected_options[[name]],previous$saved_options[[name]]),logical(1)))
  # jamovi strips analysis revisions to zero in saved files. Do not rely on them.
  if(changed && !restored && identical(snapshot$sha256,previous$saved_sha256))
    problems = c(problems,"Stale saved result after live edit")
  elements = e2e_elements(analysis$results)
  visible = Filter(function(e)!e$visible %in% c("NO","DEFAULT_NO"),elements)
  for (name in names(checkpoint$expected_result_visibility)) {
    matches = Filter(function(element) identical(element$name,name),elements)
    if (length(matches) != 1L) {
      problems = c(problems,paste("Missing or duplicated output section:",name))
    } else if (!identical(!matches[[1]]$visible %in% c("NO","DEFAULT_NO"),
        checkpoint$expected_result_visibility[[name]])) {
      problems = c(problems,paste("Incorrect output-section visibility:",name))
    }
  }
  if (length(checkpoint$expected_plot_labels)) {
    review = observation$plot_labels
    if (is.null(review) || !identical(review$source,"native_gui_review") ||
        !is.character(review$evidence) || length(review$evidence)!=1L || !nzchar(review$evidence))
      missing = c(missing,"native rendered label review") else
      if (!all(unlist(checkpoint$expected_plot_labels) %in% unlist(review$text)))
        problems = c(problems,"Expected custom labels are not visibly rendered")
  }
  if (!is.null(checkpoint$expected_panel_ghosts)) {
    review = observation$panel_ghosts
    if (is.null(review) || !identical(review$source,"native_gui_review") ||
        !is.character(review$evidence) || length(review$evidence)!=1L || !nzchar(review$evidence)) {
      missing = c(missing,"native rendered panel-ghost review (saved options are insufficient)")
    } else {
      if (!identical(review$visible,checkpoint$expected_panel_ghosts))
        problems = c(problems,"Incorrect native panel-ghost visibility")
      if (isTRUE(checkpoint$expected_panel_ghosts)) {
        for (key in c("source_preserved","no_source_duplicate","destinations_verified","source_coordinates_verified")) {
          if (is.null(review[[key]])) missing = c(missing,paste("native ghost review",key)) else
            if (!isTRUE(review[[key]])) problems = c(problems,paste("Incorrect native panel ghosts:",key))
        }
        if (!identical(review$scale,checkpoint$expected_ghost_scale))
          problems = c(problems,"Incorrect native panel-ghost scale")
      }
    }
  }
  running = analysis$status %in% c("ANALYSIS_NONE","ANALYSIS_INITED","ANALYSIS_RUNNING","ANALYSIS_RENDERING") ||
    any(vapply(visible,function(e)e$status %in% c("ANALYSIS_RUNNING","ANALYSIS_RENDERING"),logical(1)))
  if(running || observation$native_status %in% c("running",if(checkpoint$expected_status!="waiting")"waiting"))
    return(list(status="running",reason="Observable analysis/result state has not completed",revision=analysis$revision))
  if(is.null(observation$native_status)) missing = c(missing,"native completion status")
  # A caught fitting error (or waiting instruction) can finish the R request
  # successfully. Its visible, typed diagnostic below determines the outcome.
  allowed_status = checkpoint$expected_status
  if (checkpoint$expected_status == "error" || checkpoint$expected_status == "waiting" &&
      length(checkpoint$expected_diagnostic_phase)) allowed_status = c(allowed_status,"complete")
  if(!is.null(observation$native_status) && !observation$native_status %in% allowed_status && checkpoint$expected_status!="diagnostic")
    problems = c(problems,"Native analysis status differs from the requested state")
  if(checkpoint$expected_status=="complete") {
    if(!identical(analysis$status,"ANALYSIS_COMPLETE")) problems = c(problems,"Saved analysis not complete")
    errors = vapply(visible,function(e)nzchar(e$error),logical(1))
    if(nzchar(analysis$error) || any(errors)) problems = c(problems,"Visible result contains an error")
    images = Filter(function(e)identical(e$type,"image"),visible)
    if(!identical(analysis$options$graphic,FALSE)) {
      if(!length(images)) problems = c(problems,"Missing visible plot")
      for(image in images) {
        if(!isTRUE(image$resource_exists)) problems = c(problems,paste("Missing image resource:",image$name))
        if(!isTRUE(observation$rendered_images[[image$name]])) missing = c(missing,paste("actual displayed image",image$name))
      }
    }
  }
  # Saved error metadata is not evidence that the user can see a diagnostic.
  # Require a separately observed, analysis-scoped accessibility/DOM message.
  patterns = unlist(checkpoint$expected_warnings_or_errors, use.names=FALSE)
  alternatives = unlist(checkpoint$expected_diagnostic_text_any, use.names=FALSE)
  needs_diagnostic = checkpoint$expected_status %in% c("error","diagnostic") || length(patterns) || length(alternatives) || length(checkpoint$expected_diagnostic_phase)
  diagnostic_phase = if (checkpoint$expected_status == "complete") "success" else if (checkpoint$expected_status == "waiting") "waiting" else NULL
  if (needs_diagnostic) {
    diagnostics = observation$diagnostics
    if (is.null(diagnostics)) {
      missing = c(missing, "observable diagnostic text (saved metadata is insufficient)")
    } else {
      inspected = Filter(function(entry) is.list(entry) && length(entry$source) == 1L &&
        entry$source %in% c("accessibility","renderer_dom") &&
        length(entry$result) == 1L && entry$result %in% vapply(visible, `[[`, character(1), "name"), diagnostics)
      if (length(inspected) != length(diagnostics)) missing = c(missing, "native diagnostic provenance and result scope")
      shown = Filter(function(entry) isTRUE(entry$visible) && is.character(entry$text) &&
        length(entry$text) == 1L && !is.na(entry$text) && nzchar(trimws(entry$text)), inspected)
      if (!length(shown)) {
        if (length(inspected) || !length(diagnostics)) problems = c(problems, "Expected diagnostic is not visibly rendered")
      } else {
        for (entry in shown) {
          if (length(entry$phase) != 1L) next
          node = visible[[match(entry$result,vapply(visible,`[[`,character(1),"name"))]]
          if (identical(entry$phase,"rendering_error") && !identical(node$type,"image") ||
              identical(entry$phase,"fitting_error") && identical(node$type,"image") ||
              identical(entry$phase,"waiting") && !identical(entry$result,"waiting"))
            problems = c(problems,"Diagnostic phase contradicts its saved result type")
          if (entry$phase %in% c("fitting_error","rendering_error") && !nzchar(node$error) && !nzchar(analysis$error))
            problems = c(problems,"Observed error diagnostic has no corresponding saved error")
        }
        phases = unlist(lapply(shown, `[[`, "phase"), use.names=FALSE)
        allowed = unlist(checkpoint$expected_diagnostic_phase, use.names=FALSE)
        if (!length(allowed)) allowed = switch(checkpoint$expected_status,
          waiting="waiting", error=c("fitting_error","rendering_error"),
          diagnostic=c("warning","fitting_error","rendering_error"), "warning")
        selected = shown[vapply(shown, function(entry) length(entry$phase) == 1L && entry$phase %in% allowed, logical(1))]
        if (!length(selected)) problems = c(problems, "Diagnostic phase does not match waiting, fitting error, rendering error, or warning expectation")
        text = tolower(paste(vapply(selected, `[[`, character(1), "text"), collapse="\n"))
        for (pattern in patterns) if (!grepl(tolower(pattern), text, fixed=TRUE))
          problems = c(problems, paste("Missing visible diagnostic text:",pattern))
        if (length(alternatives) && !any(vapply(alternatives, function(pattern) grepl(tolower(pattern),text,fixed=TRUE), logical(1))))
          problems = c(problems, "None of the expected visible diagnostic alternatives appeared")
        diagnostic_phase = unique(phases)
      }
    }
  }
  if (checkpoint$expected_status == "error" && !nzchar(analysis$error) &&
      !any(vapply(visible,function(element)nzchar(element$error),logical(1))))
    problems = c(problems,"Expected error is absent from saved result state")
  numerical = list()
  if(!is.null(reference) && checkpoint$expected_status=="complete" && scenario$analysis!="flexplota") {
    model_count = length(reference$models)
    for(i in seq_len(model_count)) {
      name = if(scenario$analysis=="glmbasic") "glmcat" else if(scenario$analysis=="mixedmodel")
        if(i==1L) "fixed" else "fixed2" else "coefficients"
      # LM/GLM do not expose Model 2 coefficient tables: do not claim they were checked.
      if(i>1L && scenario$analysis!="mixedmodel") next
      tables = Filter(function(e)identical(e$name,name) && identical(e$type,"table"),visible)
      if(length(tables)!=1L) { problems = c(problems,paste("Missing coefficient table:",name));next }
      table = tables[[1]]
      columns = stats::setNames(lapply(table$columns,`[[`,"values"),vapply(table$columns,`[[`,character(1),"name"))
      labels = unlist(columns[[if(name=="glmcat")"var" else "term"]],use.names=FALSE)
      values = columns[[if(name=="glmcat")"means" else if(name %in% c("fixed","fixed2"))"estimate" else "raw.coefficients"]]
      expected = reference$models[[i]]
      if(!identical(labels,expected$coefficient_names)) { problems = c(problems,paste("Coefficient set mismatch:",name));next }
      actual = as.numeric(unlist(values))
      tolerance = if(scenario$analysis=="mixedmodel") 1e-5 else if(scenario$analysis=="glmgeneralized") .00050001 else 1e-8
      if(length(actual)!=length(expected$coefficients) || any(!is.finite(actual)) ||
          any(abs(actual-unname(expected$coefficients))>tolerance)) problems = c(problems,paste("Incorrect coefficients:",name))
      numerical[[name]] = list(actual=actual,expected=unname(expected$coefficients),tolerance=tolerance)
      se_name = if(name=="coefficients") "SE" else "se"
      for(field in c(se_name,"lower","upper")) {
        expected_value = if(field==se_name) expected$se else expected[[field]]
        observed_value = as.numeric(unlist(columns[[field]]))
        if(length(observed_value)!=length(expected_value) || any(!is.finite(observed_value)) ||
            any(abs(observed_value-unname(expected_value))>if(scenario$analysis=="mixedmodel")1e-5 else 1e-8))
          problems = c(problems,paste("Incorrect coefficient uncertainty:",name,field))
      }
    }
  }
  list(status=if(length(problems))"failed" else if(length(missing))"blocked" else "passed",
    problems=unique(problems),unobserved=unique(missing),revision=analysis$revision,
    saved_sha256=snapshot$sha256,saved_options=analysis$options,numerical=numerical,diagnostic_phase=diagnostic_phase)
}

e2e_run_scenario = function(scenario, driver, references, output, timeout=30, now=function()proc.time()[[3]],poll=function()Sys.sleep(.1)) {
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  records = list();previous = NULL
  tasks = c(seq_along(scenario$steps),if(isTRUE(scenario$save_reopen$required))length(scenario$steps))
  for(number in seq_along(tasks)) {
    index = tasks[number];restored = number>length(scenario$steps)
    request = list(id=paste0(scenario$id,"/",index,if(restored)"/restored"),scenario=scenario$id,
      analysis=scenario$analysis,dataset=scenario$dataset,checkpoint=index,restored=restored,
      action=if(restored)"save_close_reopen_through_gui" else scenario$steps[[index]],timeout=timeout)
    path = file.path(output,paste0(sprintf("%02d",number),"-request.json"))
    jsonlite::write_json(request,path,pretty=TRUE,auto_unbox=TRUE,null="null",digits=NA)
    started = now();observation = NULL
    action_error = tryCatch(driver$act(request),error=identity)
    repeat {
      if(is.list(action_error) && isTRUE(action_error$status %in% c("blocked","failed"))) {
        checked = action_error
        break
      }
      if(inherits(action_error,"error")) {
        checked = list(status="blocked",reason=paste("Native driver error:",conditionMessage(action_error)))
        break
      }
      observation = tryCatch(driver$observe(request),error=identity)
      if(inherits(observation,"error")) {
        checked = list(status="blocked",reason=paste("Native inspection error:",conditionMessage(observation)))
        observation = NULL
        break
      }
      if(!is.null(observation)) {
        checked = e2e_checkpoint(scenario,index,observation,references[[paste0(scenario$id,"/",index)]],previous,restored)
        if(checked$status!="running") break
      }
      if(now()-started>=timeout) {
        checked = list(status="blocked",reason="Timeout: no completed native/saved checkpoint observed")
        break
      }
      poll()
    }
    record = list(request=request,result=checked,observation=observation,elapsed=now()-started)
    records[[number]] = record
    jsonlite::write_json(record,file.path(output,paste0(sprintf("%02d",number),"-result.json")),pretty=TRUE,auto_unbox=TRUE,na="null",null="null",digits=NA)
    if(checked$status!="passed") break
    previous = checked
  }
  status = if(any(vapply(records,function(r)r$result$status=="failed",logical(1))))"failed" else
    if(length(records)!=length(tasks) || any(vapply(records,function(r)r$result$status!="passed",logical(1))))"blocked" else "passed"
  summary = list(scenario=scenario$id,status=status,attempted_checkpoints=length(records),
    passed_checkpoints=sum(vapply(records,function(r)r$result$status=="passed",logical(1))),required_checkpoints=length(tasks),
    not_executed=max(0,length(tasks)-length(records)))
  jsonlite::write_json(summary,file.path(output,"summary.json"),pretty=TRUE,auto_unbox=TRUE,digits=NA)
  writeLines(c(paste(scenario$id,status),paste("Attempted checkpoints:",length(records),"of",length(tasks)),
    paste("Passed checkpoints:",summary$passed_checkpoints),
    vapply(records,function(r)paste(r$request$id,r$result$status,paste(c(r$result$problems,r$result$unobserved,r$result$reason),collapse="; ")),character(1))),
    file.path(output,"summary.txt"))
  summary
}

# A file bridge lets the desktop driver use fresh native accessibility indices.
# It does not launch an app, set options through an API, or synthesize backend state.
e2e_file_driver = function(directory) {
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  list(act=function(request) {
    unlink(file.path(directory,"observation.json"))
    temporary = file.path(directory,"request.pending.json")
    jsonlite::write_json(request,temporary,auto_unbox=TRUE,pretty=TRUE,null="null")
    if(!file.rename(temporary,file.path(directory,"request.json"))) stop("Cannot publish GUI request")
  },observe=function(request) {
    path = file.path(directory,"observation.json")
    if(!file.exists(path)) return(NULL)
    tryCatch(jsonlite::read_json(path,simplifyVector=FALSE),error=function(error)NULL)
  })
}

e2e_module_preflight = function(root, installed=file.path(path.expand("~/Library/Application Support/jamovi/modules/flexplot"),"jamovi.yaml")) {
  expected = unname(read.dcf(file.path(root,"DESCRIPTION"))[1,"Version"])
  source_version = as.character(yaml::read_yaml(file.path(root,"jamovi/0000.yaml"))$version)
  blocked = function(reason,...) list(status="blocked",reason=reason,expected=expected,...)
  if(!identical(source_version,expected)) return(blocked("Source module/R package version synchronization error"))
  if(!file.exists(installed)) return(blocked("Installed flexplot metadata unavailable"))
  metadata = yaml::read_yaml(installed)
  if(!identical(as.character(metadata$version),expected))
    return(blocked("Installed module version differs from the checkout",installed=metadata$version,build_hash=metadata[["build-hash"]]))
  receipt_path = file.path(root,".jamovi-dev/build-receipt.json")
  if(!file.exists(receipt_path)) return(blocked("No verified archive/source installation receipt; rebuild with dev/jamovi.R install"))
  result = tryCatch({
    commands = new.env(parent=environment())
    sys.source(file.path(root,"dev/jamovi.R"),commands)
    sys.source(file.path(root,"dev/jamovi/build-identity.R"),commands)
    receipt = jsonlite::read_json(receipt_path)
    current = commands$jamovi_dev_identity(root)
    if(!identical(receipt$source$fingerprint,current$fingerprint)) stop("Installed source fingerprint is stale")
    if(!identical(metadata[["build-hash"]],paste0("source-sha256-",current$fingerprint))) stop("Installed build stamp differs from source")
    runtime_version = unname(read.dcf(file.path(dirname(installed),"R/flexplot/DESCRIPTION"))[1,"Version"])
    if(!identical(runtime_version,expected)) stop("Installed R package version differs from module")
    payload = commands$jamovi_dev_payload(root,receipt$archive,dirname(installed),system.file("node_modules/jamovi-compiler",package="jmvtools"))
    if(!identical(payload$archive_sha256,receipt$payload$archive_sha256) ||
       !identical(payload$payload_sha256,receipt$payload$payload_sha256)) stop("Archive receipt checksum mismatch")
    list(status="passed",expected=expected,installed=metadata$version,source_sha256=current$fingerprint,
      archive_sha256=payload$archive_sha256,payload_sha256=payload$payload_sha256,build_hash=metadata[["build-hash"]])
  },error=function(error)blocked(conditionMessage(error)))
  result
}

# Verify the complete option batch around a native action, not only its target.
e2e_option_delta = function(before, after, analysis_id, allowed, allow_host_initialization=FALSE) {
  first = Filter(function(a)a$id==analysis_id,before$analyses)
  second = Filter(function(a)a$id==analysis_id,after$analyses)
  if(length(first)!=1L || length(second)!=1L) return(list(status="blocked",reason="Analysis identity unavailable"))
  keys = union(names(first[[1]]$options),names(second[[1]]$options))
  changed = keys[!vapply(keys,function(key)e2e_option_equal(first[[1]]$options[[key]],second[[1]]$options[[key]]),logical(1))]
  initialized = if(allow_host_initialization) intersect(changed,setdiff(c(".ppi","theme","palette","decSymbol",".lang"),names(first[[1]]$options))) else character()
  unexpected = setdiff(changed,c(allowed,initialized))
  list(status=if(length(unexpected))"failed" else "passed",changed=setdiff(changed,initialized),host_initializations=initialized,unexpected=unexpected)
}

# The read-only module descriptor must come from the running GUI's observed server.
# Do not open an InfoRequest WebSocket: it can replace the GUI communication channel.
e2e_served_preflight = function(installed, metadata) {
  if(!identical(installed$status,"passed")) return(installed)
  if(is.null(metadata)) return(list(status="blocked",reason="Running GUI module descriptor not observed"))
  if(!identical(as.character(metadata$version),as.character(installed$installed)) ||
     !identical(metadata[["build-hash"]],installed$build_hash))
    return(list(status="blocked",reason="Running GUI module differs from the verified installed build"))
  c(installed,list(running_gui_verified=TRUE))
}

e2e_running_preflight = function(installed, endpoint) {
  if(!identical(installed$status,"passed")) return(installed)
  if(is.null(endpoint) || !grepl("^http://127[.]0[.]0[.]1:[0-9]+/modules/flexplot$",endpoint))
    return(list(status="blocked",reason="Supply the read-only module endpoint from the existing GUI's observed server URL"))
  tryCatch(e2e_served_preflight(installed,yaml::yaml.load(paste(readLines(endpoint,warn=FALSE),collapse="\n"))),
    error=function(error)list(status="blocked",reason=paste("Running GUI descriptor unavailable:",conditionMessage(error))))
}

# Records come from native observations; this policy never opens, moves or closes a GUI.
# Keep closed records as an audit trail. Never infer ownership from an app name alone.
e2e_register_window = function(registry, window, purpose="existing_test") {
  block = function(reason) list(status="blocked",reason=reason,registry=registry)
  if(!is.character(purpose) || length(purpose)!=1L || is.na(purpose)) return(block("Window purpose must be explicit"))
  required = c("id","title","document","source","secondary_monitor","disposable")
  if(!all(required %in% names(window)) || !identical(window$source,"native_gui") ||
      !isTRUE(window$secondary_monitor) || !isTRUE(window$disposable) ||
      any(!vapply(window[c("id","title","document")],function(x)is.character(x) && length(x)==1L && !is.na(x) && nzchar(x),logical(1))))
    return(block("Exact native window/document identity and secondary-monitor ownership confirmation required"))
  active = Filter(function(w)!isTRUE(w$closed),registry)
  if(any(vapply(active,function(w)w$id==window$id || w$title==window$title,logical(1))))
    return(block("Duplicate or ambiguous window registration"))
  if(length(active)>=2L || (length(active)==1L && purpose!="save_reopen") ||
      !purpose %in% c("existing_test","save_reopen"))
    return(block("At most two disposable windows; a second is allowed only for save/reopen"))
  window$created_by_test = identical(purpose,"save_reopen")
  window$purpose = purpose
  window$closed = FALSE
  list(status="passed",registry=c(registry,list(window)))
}

e2e_authorize_window_close = function(registry, observed) {
  matches = Filter(function(w)!isTRUE(w$closed) && identical(w$id,observed$id),registry)
  if(length(matches)!=1L) return(list(status="blocked",reason="Unique registered window not observed"))
  owned = matches[[1]]
  if(!isTRUE(owned$created_by_test) || !isTRUE(observed$secondary_monitor) ||
      !identical(observed$source,"native_gui") || !identical(owned$title,observed$title) ||
      !identical(owned$document,observed$document))
    return(list(status="blocked",reason="Refusing to close an existing, unrelated, moved or mismatched document"))
  list(status="passed",id=owned$id,title=owned$title,document=owned$document)
}

e2e_desktop_verified = function(desktop) {
  if(!isTRUE(desktop$secondary_monitor) || !isTRUE(desktop$owned_document)) return(FALSE)
  if(isTRUE(desktop$window_count==1L)) return(TRUE) # Original single-window observation contract.
  if(!isTRUE(desktop$window_count==2L)) return(FALSE)
  registry = desktop$window_registry
  if(length(registry)!=2L || is.null(desktop$active_window_id)) return(FALSE)
  verified = list()
  for(window in registry) {
    added = e2e_register_window(verified,window,window$purpose)
    if(added$status!="passed" || isTRUE(window$closed)) return(FALSE)
    verified = added$registry
  }
  any(vapply(verified,function(w)identical(w$id,desktop$active_window_id),logical(1)))
}
