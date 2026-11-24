#!/usr/bin/env Rscript
# Headless artifact generation only. No installation, GUI or application access.
e2e_root = function() {
  argument = grep("^--file=",commandArgs(FALSE),value=TRUE)
  if (!length(argument)) stop("Call generate.R with Rscript, or supply root to e2e_generate()")
  normalizePath(file.path(dirname(sub("^--file=","",argument[1])),"../../.."))
}

e2e_inventory = function(root, manifest) {
  result = list()
  tests = list.files(c(file.path(root,"tests/testthat"),file.path(root,"dev/jamovi/tests")),
    pattern="^test.*\\.R$",full.names=TRUE)
  for (analysis in c("flexplota","glmbasic","glmgeneralized","mixedmodel")) {
    definition = yaml::read_yaml(file.path(root,"jamovi",paste0(analysis,".a.yaml")))
    ui = yaml::read_yaml(file.path(root,"jamovi",paste0(analysis,".u.yaml")))
    results = yaml::read_yaml(file.path(root,"jamovi",paste0(analysis,".r.yaml")))
    controls = new.env(parent=emptyenv())
    controls$value = list()
    visit = function(node, ancestry=character(), inherited=character()) {
      if (!is.list(node)) return(invisible(NULL))
      conditions = c(inherited,unlist(node[intersect(c("enable","contentLink","visible"),names(node))]))
      path = c(ancestry,if (!is.null(node$label)) node$label else if(!is.null(node$name)) node$name)
      if (!is.null(node$type)) controls$value[[length(controls$value)+1L]] = list(name=node$name,type=node$type,
        path=path,conditions=conditions,maxItemCount=node$maxItemCount,permitted=node$permitted)
      for (child in node$children) visit(child,path,conditions)
      invisible(NULL)
    }
    visit(ui)
    selected = Filter(function(s) s$analysis==analysis,manifest$scenarios)
    options = lapply(definition$options,function(option) {
      covered = Filter(function(s) any(vapply(s$steps,function(step) option$name %in%
        c(names(s$initial_options),names(step$changes),if(step$action=="edit_generated_plot_label_rows") { if(is.null(step$label_option)) "plotLabels" else step$label_option }),logical(1))),selected)
      if(option$name %in% c("plotLabelStore","comparisonPlotLabelStore")) {
        visible = sub("LabelStore$","Labels",option$name)
        covered = Filter(function(s)any(vapply(s$steps,function(step)
          identical(step$action,"edit_generated_plot_label_rows") &&
          identical(if(is.null(step$label_option)) "plotLabels" else step$label_option,visible),logical(1))),selected)
      }
      if(option$name %in% c("data","samplePolicyVersion","comparisonMagnet","outputOptionsVersion","plotLabelEditorVersion")) covered = selected
      list(id=paste(analysis,option$name,sep="."),definition=option,
        scenarios=vapply(covered,`[[`,character(1),"id"),
        coverage=if(length(covered)) "planned GUI checkpoint; not yet GUI verified" else "gap: no planned option transition")
    })
    js = if(analysis=="flexplota")"flexplota.events" else analysis
    source = file.path("jamovi/js",paste0(js,".js"))
    lines = readLines(file.path(root,source),warn=FALSE)
    handlers = grep("function|_changed:|_updated:",lines,value=TRUE)
    result[[analysis]] = list(title=definition$title,options=options,controls=controls$value,results=results$items,
      sources=c(file.path("jamovi",paste0(analysis,c(".a.yaml",".u.yaml",".r.yaml"))),source,
        file.path("R",paste0(analysis,".b.R"))),handler_inventory=handlers,
      existing_tests=unique(unlist(lapply(selected,`[[`,"existing_coverage"))))
  }
  list(schema_version=1,analyses=result,test_files=sub(paste0(root,"/"),"",tests,fixed=TRUE))
}

e2e_validate_manifest = function(manifest, fixtures, root) {
  ids = vapply(manifest$scenarios,`[[`,character(1),"id")
  stopifnot(!anyDuplicated(ids),length(ids)>0)
  for (scenario in manifest$scenarios) {
    stopifnot(scenario$dataset %in% names(fixtures),length(scenario$steps)>0,
      isTRUE(scenario$gui_execution_required),scenario$priority %in% c("P0","P1","P2"))
    definitions = yaml::read_yaml(file.path(root,"jamovi",paste0(scenario$analysis,".a.yaml")))$options
    allowed = vapply(definitions,`[[`,character(1),"name")
    for (step in scenario$steps) {
      stopifnot(all(names(step$expected_options) %in% allowed),all(names(step$changes) %in% allowed),
        isTRUE(step$verify_visible_against_saved_options),step$expected_status %in% c("complete","waiting","error","diagnostic"))
      phases = unlist(step$expected_diagnostic_phase, use.names=FALSE)
      stopifnot(all(phases %in% c("waiting","warning","fitting_error","rendering_error")))
      for (definition in definitions) {
        value = step$expected_options[[definition$name]]
        if(is.null(value)) next
        if(identical(definition$type,"List")) {
          choices = vapply(definition$options,function(choice) if(is.list(choice)) choice$name else choice,character(1))
          if(length(value)!=1L || !value %in% choices) stop(scenario$id,": invalid value for ",definition$name)
        }
        if(identical(definition$type,"Bool") && (!is.logical(value) || length(value)!=1L))
          stop(scenario$id,": invalid boolean for ",definition$name)
      }
      for (key in intersect(c("out","preds","given","cluster","plotX","plotPanels","plotGroup", "comparisonFixed",
          "comparisonX","comparisonPanels","comparisonGroup","quadratic","comparisonQuadratic","randomSlopes","comparisonRandomSlopes"),names(step$expected_options))) {
        values = unlist(step$expected_options[[key]],use.names=FALSE)
        if(!all(values %in% names(fixtures[[scenario$dataset]]))) stop(scenario$id,": missing fixture column: ",key)
      }
      if (!is.null(step$reference)) for (formula in step$reference$formulas)
        stopifnot(all(all.vars(stats::as.formula(formula)) %in% names(fixtures[[scenario$dataset]])))
    }
    for (file in scenario$existing_coverage) stopifnot(any(file.exists(file.path(root,c("tests/testthat","dev/jamovi/tests"),file))))
  }
  invisible(TRUE)
}

e2e_generate = function(root, output=file.path(root,"dev/jamovi/e2e/artifacts")) {
  for(package in c("yaml","jsonlite","lme4","MASS"))
    if(!requireNamespace(package,quietly=TRUE)) stop("Existing development dependency required: ",package)
  source(file.path(root,"dev/jamovi/e2e/fixtures.R"),local=TRUE)
  manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"),simplifyVector=FALSE)
  fixtures = e2e_fixtures()
  e2e_validate_manifest(manifest,fixtures,root)
  inventory = e2e_inventory(root,manifest)
  references = list()
  for (scenario in manifest$scenarios) for(i in seq_along(scenario$steps)) {
    specification = scenario$steps[[i]]$reference
    if(is.null(specification)) next
    key = paste0(scenario$id,"/",i)
    references[[key]] = e2e_reference_fit(specification,fixtures[[scenario$dataset]])
  }
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  for (name in names(fixtures)) utils::write.csv(fixtures[[name]],file.path(output,paste0(name,".csv")),row.names=FALSE,na="")
  saveRDS(fixtures,file.path(output,"fixtures.rds"),version=3,compress="xz")
  saveRDS(references,file.path(output,"references.rds"),version=3,compress="xz")
  jsonlite::write_json(references,file.path(output,"references.json"),auto_unbox=TRUE,digits=NA,pretty=TRUE,na="null",null="null")
  jsonlite::write_json(inventory,file.path(output,"inventory.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  schema = lapply(fixtures,function(data) lapply(data,function(column) list(class=class(column),levels=levels(column),
    missing=sum(is.na(column)))))
  jsonlite::write_json(schema,file.path(output,"data-schema.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  coverage = do.call(rbind,lapply(names(inventory$analyses),function(analysis) do.call(rbind,lapply(
    inventory$analyses[[analysis]]$options,function(option) data.frame(feature=option$id,
      scenarios=paste(option$scenarios,collapse=";"),status=option$coverage,
      evidence=paste(inventory$analyses[[analysis]]$existing_tests,collapse=";"))))))
  utils::write.csv(coverage,file.path(output,"coverage.csv"),row.names=FALSE)
  scenario_index = do.call(rbind,lapply(manifest$scenarios,function(s) data.frame(id=s$id,analysis=s$analysis,
    title=s$title,dataset=s$dataset,priority=s$priority,risk=s$risk,checkpoints=length(s$steps),
    references=sum(vapply(s$steps,function(step)!is.null(step$reference),logical(1))),
    launch_paths=paste(s$launch_paths,collapse=";"),gui_status="not_run")))
  utils::write.csv(scenario_index,file.path(output,"scenario-index.csv"),row.names=FALSE)
  versions = vapply(c("R","yaml","jsonlite","lme4","MASS"),function(package)
    if(package=="R") as.character(getRversion()) else as.character(utils::packageVersion(package)),character(1))
  provenance = list(generator="dev/jamovi/e2e/fixtures.R",seed=20261120,common_sample_seed=925,
    versions=as.list(versions),dimensions=lapply(fixtures,dim),fixture_md5=as.list(stats::setNames(unname(tools::md5sum(file.path(output,paste0(names(fixtures),".csv")))),names(fixtures))),
    prediction_contract="references use all eligible rows at diagnostic x=-1,0,1; actual GUI grid/panel values require new predictions at captured coordinates")
  jsonlite::write_json(provenance,file.path(output,"provenance.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("Generated",nrow(fixtures$primary),"primary rows,",ncol(fixtures$primary),"columns;",length(manifest$scenarios),
    "scenarios;",length(references),"independent fitted checkpoints. No GUI was opened.\n")
  invisible(list(fixtures=fixtures,references=references,inventory=inventory))
}
if(sys.nframe()==0L) {
  arguments = commandArgs(TRUE)
  e2e_generate(e2e_root(),if(length(arguments)) arguments[1] else file.path(e2e_root(),"dev/jamovi/e2e/artifacts"))
}
