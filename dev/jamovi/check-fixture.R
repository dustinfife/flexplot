#!/usr/bin/env Rscript
# Verify saved options through the actual R6 classes, without opening jamovi.
script = sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
root = normalizePath(file.path(dirname(script),"../.."))
home = Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app")
.libPaths(c(.libPaths(),file.path(home,"Contents/Resources/modules/base/R")))
devtools::load_all(root,quiet=TRUE)
jmvcore:::initProtoBuf()
file = file.path(root,".jamovi-dev/smoke.omv")
if (!file.exists(file)) stop("Run Rscript dev/jamovi.R fixture first")
data = jmvReadWrite::read_omv(fleInp=file)
entries = utils::unzip(file,list=TRUE)$Name
analyses = grep("/analysis$",entries,value=TRUE)
stopifnot(length(analyses)==8L,length(grep("/resources/.*png$",entries))==9L,nrow(data)==120L)
formulas = list(continuous~x+group,continuous~x+group+z+x:group,
  binary~x+group+z+x:group,count~x+group+z,binary~x+group+z+x:group,
  mixed~x+z+(1|id),mixed~x+z+I(x^2)+(0+x|id),mixed~x+z+group+x:group+(1+x|id))
for (i in seq_along(analyses)) {
  connection = unz(file,analyses[i],open="rb")
  raw = readBin(connection,"raw",1000000L);close(connection)
  message = RProtoBuf::read(RProtoBuf::P("jamovi.coms.AnalysisResponse"),raw)
  options = get(paste0(message$name,"Options"),asNamespace("flexplot"))$new()
  options$fromProtoBuf(message$options)
  stopifnot(length(options$preds)>0L,length(options$modelTerms)>0L)
  analysis = get(paste0(message$name,"Class"),asNamespace("flexplot"))$new(options=options,data=data)
  stopifnot(analysis$run())
  state = analysis$results$plot$state
  formula = if (message$name %in% c("glmgeneralized","mixedmodel")) stats::formula(state$model) else state$formula
  stopifnot(isTRUE(all.equal(stats::model.matrix(reformulas::nobars(formula),data),stats::model.matrix(reformulas::nobars(formulas[[i]]),data))))
  if (message$name=="mixedmodel") {
    independent = lme4::lmer(formulas[[i]],data=data,REML=options$estimation=="REML")
    stopifnot(isTRUE(all.equal(lme4::fixef(state$model),lme4::fixef(independent),tolerance=1e-10)),
      isTRUE(all.equal(as.data.frame(lme4::VarCorr(state$model)),as.data.frame(lme4::VarCorr(independent)),tolerance=1e-10)),
      identical(lme4::isREML(state$model),options$estimation=="REML"),
      identical(options$cluster,"id"),identical(options$randomIntercept,i!=7L))
    if(i %in% c(7L,8L)) stopifnot(identical(options$randomSlopes,"x"))
    if(i==7L) stopifnot(identical(options$quadratic,"x"),identical(options$predictionType,"population"))
  }
  if (message$name=="glmgeneralized") {
    independent = glm(formulas[[i]],data=data,family=if(i %in% c(3L,5L)) binomial() else poisson())
    stopifnot(isTRUE(all.equal(stats::coef(state$model),stats::coef(independent),tolerance=1e-12)))
    if (isTRUE(options$compare)) {
      comparison = analysis$results$comparisonplot$state
      reduced = glm(binary~x+group+z, data=data, family=binomial())
      stopifnot(isTRUE(all.equal(coef(comparison$model2),coef(reduced),tolerance=1e-12)),
        analysis$results$comparison$visible,analysis$results$comparisonplot$visible)
    }
  }
}
cat("PASS: eight saved model specifications including three Mixed Models and a GLM comparison restore through R6; fixed matrices, coefficients and variance components agree with independent models.\n")
