# Build a native .omv using existing jmvcore results, not a second analysis engine.
jamovi_smoke_resource = function(name, extension) {
  list(rootPath=root, relPath=paste0(prefix, "/resources/", name, ".", extension))
}

jamovi_smoke_fixture = function(root, home, output) {
  bundled = file.path(home, "Contents/Resources/modules/base/R")
  previous = .libPaths()
  on.exit(.libPaths(previous), add=TRUE)
  .libPaths(c(.libPaths(), bundled))
  for (package in c("devtools", "jmvcore", "jmvReadWrite", "RProtoBuf")) {
    if (!requireNamespace(package, quietly=TRUE)) stop("Fixture requires ", package,
      ". Use jamovi's bundled libraries or your existing development installation.")
  }
  devtools::load_all(root, quiet=TRUE)
  jmvcore:::initProtoBuf()
  set.seed(91452)
  data = data.frame(x=rep(seq(-2, 2, length.out=60), 2), z=rnorm(120),
    group=factor(rep(c("A", "B"), each=60)))
  eta = .2 + .5*data$x + .3*data$z + .5*(data$group=="B") + .35*data$x*(data$group=="B")
  data$continuous = eta+rnorm(nrow(data), sd=.7)
  data$binary = factor(rbinom(nrow(data),1,plogis(eta)), levels=c(0,1), labels=c("No","Yes"))
  data$count = rpois(nrow(data),exp(.3+.25*data$x+.2*(data$group=="B")+.1*data$z))
  data$id = factor(rep(seq_len(12),10))
  intercept = rnorm(12,sd=1)
  slope = rnorm(12,sd=.7)
  data$mixed = eta+intercept[data$id]+slope[data$id]*data$x+rnorm(120,sd=.3)
  configurations = list(
    list(name="glmbasic", title="LM - main effects", options=list(out="continuous",preds=c("x","group"),
      modelTerms=list("x","group"),plotX="x",plotGroup="group")),
    list(name="glmbasic", title="LM - interaction", options=list(out="continuous",preds=c("x","group","z"),
      modelTerms=list("x","group","z",c("x","group")),plotX="x",plotPanels="group")),
    list(name="glmgeneralized", title="GLM - logistic interaction", options=list(out="binary",preds=c("x","group","z"),
      family="binomial",modelTerms=list("x","group","z",c("x","group")),plotX="x",plotPanels="group")),
    list(name="glmgeneralized", title="GLM - Poisson", options=list(out="count",preds=c("x","group","z"),
      family="poisson",modelTerms=list("x","group","z"),plotX="x",plotPanels="group")),
    list(name="glmgeneralized", title="GLM - logistic model comparison", options=list(out="binary",preds=c("x","group","z"),
      family="binomial",modelTerms=list("x","group","z",c("x","group")),
      compare=TRUE,comparisonTerms=list("x","group","z"),comparisonX="x",comparisonPanels="group",
      plotX="x",plotPanels="group")),
    list(name="mixedmodel", title="Mixed - random intercept", options=list(out="mixed",preds=c("x","z"),
      cluster="id",modelTerms=list("x","z"),plotX="x",plotPanels="id")),
    list(name="mixedmodel", title="Mixed - random slope and quadratic", options=list(out="mixed",preds=c("x","z"),
      cluster="id",modelTerms=list("x","z"),randomSlopes="x",randomIntercept=FALSE,quadratic="x",polynomials=TRUE,estimation="ML",
      predictionType="population",plotX="x",plotPanels="id")),
    list(name="mixedmodel", title="Mixed - fixed interaction", options=list(out="mixed",preds=c("x","z","group"),
      cluster="id",modelTerms=list("x","z","group",c("x","group")),interactions=TRUE,randomSlopes="x",
      plotX="x",plotPanels="id",plotGroup="group")))
  work = tempfile("jamovi-smoke-")
  dir.create(work)
  on.exit(unlink(work,recursive=TRUE),add=TRUE)
  archive = file.path(work,"smoke.omv")
  jmvReadWrite::write_omv(dtaFrm=data,fleOut=archive,frcWrt=TRUE)
  original_theme = ggplot2::theme_get()
  on.exit(ggplot2::theme_set(original_theme),add=TRUE)
  files = character()
  for (i in seq_along(configurations)) {
    specification = configurations[[i]]
    ns = asNamespace("flexplot")
    option_class = get(paste0(specification$name,"Options"),ns)
    analysis_class = get(paste0(specification$name,"Class"),ns)
    options = do.call(option_class$new,specification$options)
    analysis = analysis_class$new(options=options,data=data,analysisId=as.integer(2*i))
    if (!isTRUE(analysis$run())) stop("Smoke analysis did not complete: ", specification$title)
    prefix = sprintf("%02d %s",2*i,specification$name)
    dir.create(file.path(work,prefix,"resources"),recursive=TRUE)
    resource = jamovi_smoke_resource
    environment(resource) = list2env(list(root=work,prefix=prefix),parent=environment(resource))
    analysis$.setResourcesPathSource(resource)
    analysis$.createImages()
    response = analysis$asProtoBuf(final=TRUE,includeState=FALSE)
    response$title = specification$title
    response$hasTitle = TRUE
    response$enabled = TRUE
    RProtoBuf::serialize(response,file.path(work,prefix,"analysis"))
    files = c(files,list.files(file.path(work,prefix),recursive=TRUE,full.names=TRUE))
  }
  previous_dir = setwd(work)
  on.exit(setwd(previous_dir),add=TRUE)
  relative = substring(files,nchar(work)+2L)
  status = utils::zip(archive,files=relative,flags="-q")
  if (!identical(as.integer(status),0L)) stop("Could not archive smoke analyses")
  manifest = list(configurations=configurations, definitions=list(
    glmbasic=yaml::read_yaml(file.path(root,"jamovi/glmbasic.a.yaml"))$options,
    mixedmodel=yaml::read_yaml(file.path(root,"jamovi/mixedmodel.a.yaml"))$options,
    glmgeneralized=yaml::read_yaml(file.path(root,"jamovi/glmgeneralized.a.yaml"))$options))
  manifest_path = file.path(work,"manifest.json")
  jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,null="null")
  previous_python = Sys.getenv(c("PYTHONHOME","PYTHONPATH"),unset=NA_character_)
  on.exit({
    for (name in names(previous_python)) {
      if (is.na(previous_python[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv,setNames(list(previous_python[[name]]),name))
    }
  },add=TRUE)
  Sys.setenv(PYTHONHOME=file.path(home,"Contents/Resources"),
    PYTHONPATH=file.path(home,"Contents/Resources/jamovi/server"))
  status = system2(file.path(home,"Contents/MacOS/python"),shQuote(c(
    file.path(root,"dev/jamovi/finalize-fixture.py"),archive,manifest_path)))
  if (!identical(as.integer(status),0L)) stop("Could not serialize saved analysis options")
  dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
  if (!file.copy(archive,output,overwrite=TRUE)) stop("Could not write ",output)
  invisible(output)
}
