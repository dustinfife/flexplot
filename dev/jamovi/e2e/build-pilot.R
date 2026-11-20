#!/usr/bin/env Rscript
# Prepared starting documents only; these are NOT evidence of GUI execution.
script = gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1]),fixed=TRUE)
root = normalizePath(file.path(dirname(script),"../../.."))
output = file.path(root,".jamovi-dev/e2e-pilot")
files = file.path(output,paste0(c("primary","common_sample"),".omv"))
if(any(file.exists(files)) && !"--force" %in% commandArgs(TRUE))
  stop("Prepared pilot documents already exist. Preserve open/evidence files; use --force only for deliberate regeneration of closed disposable documents.")
home = Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app")
.libPaths(c(.libPaths(),file.path(home,"Contents/Resources/modules/base/R")))
devtools::load_all(root,quiet=TRUE,helpers=FALSE)
jmvcore:::initProtoBuf()
source(file.path(root,"dev/jamovi/fixture.R"))
manifest = jsonlite::read_json(file.path(root,"dev/jamovi/e2e/manifest.json"))
fixtures = readRDS(file.path(root,"dev/jamovi/e2e/artifacts/fixtures.rds"))
ids = c("FP-03","LM-03","LM-09","GLM-08","MM-11","MM-10")
selected = lapply(ids,function(id)Filter(function(s)s$id==id,manifest$scenarios)[[1]])
dir.create(output,recursive=TRUE,showWarnings=FALSE)
for(dataset in c("primary","common_sample")) local({
  dataset_name = dataset
  input = fixtures[[dataset_name]]
  cases = Filter(function(s)s$dataset==dataset_name,selected)
  work = tempfile("e2e-prepared-");dir.create(work)
  on.exit(unlink(work,recursive=TRUE),add=TRUE)
  archive = file.path(work,"prepared.omv")
  jmvReadWrite::write_omv(dtaFrm=input,fleOut=archive,frcWrt=TRUE)
  configurations = list()
  for(i in seq_along(cases)) {
    scenario = cases[[i]]
    values = scenario$initial_options
    for(name in intersect(c("preds","plotPanels","quadratic","randomSlopes","comparisonFixed",
      "comparisonQuadratic","comparisonRandomSlopes","comparisonPanels","given"),names(values))) values[name] = list(unlist(values[[name]],use.names=FALSE))
    for(name in intersect(c("modelTerms","comparisonTerms"),names(values))) values[name] = list(lapply(values[[name]],unlist,use.names=FALSE))
    options = do.call(get(paste0(scenario$analysis,"Options"),asNamespace("flexplot"))$new,values)
    analysis = get(paste0(scenario$analysis,"Class"),asNamespace("flexplot"))$new(options=options,data=input,analysisId=as.integer(2*i))
    stopifnot(isTRUE(analysis$run()))
    prefix = sprintf("%02d %s",2*i,scenario$analysis)
    dir.create(file.path(work,prefix,"resources"),recursive=TRUE)
    resource = jamovi_smoke_resource
    environment(resource) = list2env(list(root=work,prefix=prefix),parent=environment(resource))
    analysis$.setResourcesPathSource(resource);analysis$.createImages()
    response = analysis$asProtoBuf(final=TRUE,includeState=FALSE)
    response$title = scenario$id;response$hasTitle = TRUE;response$enabled = TRUE
    RProtoBuf::serialize(response,file.path(work,prefix,"analysis"))
    configurations[[i]] = list(name=scenario$analysis,options=values)
  }
  previous = setwd(work);on.exit(setwd(previous),add=TRUE)
  utils::zip(archive,list.files(work,recursive=TRUE,pattern="analysis$|\\.png$"),flags="-q")
  definitions = lapply(c("flexplota","glmbasic","glmgeneralized","mixedmodel"),function(name)
    yaml::read_yaml(file.path(root,"jamovi",paste0(name,".a.yaml")))$options)
  names(definitions) = c("flexplota","glmbasic","glmgeneralized","mixedmodel")
  config = file.path(work,"config.json")
  jsonlite::write_json(list(configurations=configurations,definitions=definitions),config,auto_unbox=TRUE,null="null")
  Sys.setenv(PYTHONHOME=file.path(home,"Contents/Resources"),PYTHONPATH=file.path(home,"Contents/Resources/jamovi/server"))
  stopifnot(system2(file.path(home,"Contents/MacOS/python"),shQuote(c(file.path(root,"dev/jamovi/finalize-fixture.py"),archive,config)))==0L)
  stopifnot(file.copy(archive,file.path(output,paste0(dataset_name,".omv")),overwrite=TRUE))
})
cat("Prepared pilot documents in",output,"; no GUI was launched.\n")
