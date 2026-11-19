#!/usr/bin/env Rscript
# Read-only comparisons by default; --candidates writes review files outside _snaps.
args = commandArgs(TRUE)
if (length(args) && (length(args)!=2L || args[1]!="--candidates"))
  stop("Usage: Rscript --vanilla dev/visual-tests.R [--candidates DIRECTORY]")
script = sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
root = normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
for (package in c("devtools","vdiffr")) {
  if (!requireNamespace(package,quietly=TRUE)) stop(package," is required for visual development tests")
}
Sys.setenv(NOT_CRAN="true",FLEXPLOT_SNAPSHOT_CANDIDATES="")
if (length(args)) {
  dir.create(args[2],recursive=TRUE,showWarnings=FALSE)
  candidate_dir = normalizePath(args[2],mustWork=TRUE)
  baseline_dir = normalizePath(file.path(root,"tests/testthat/_snaps"),mustWork=TRUE)
  if (identical(candidate_dir,baseline_dir) || startsWith(candidate_dir,paste0(baseline_dir,"/")))
    stop("Review candidates must be outside the approved baseline directory")
  Sys.setenv(FLEXPLOT_SNAPSHOT_CANDIDATES=candidate_dir)
  cat("Writing review candidates to ",candidate_dir,"; approved baselines remain read-only.\n",sep="")
}
helpers = new.env()
sys.source(file.path(root,"tests/testthat/helper-visual.R"),envir=helpers)
cases = helpers$flexplot_visual_cases(file.path(root,"tests/testthat"))
files = sub("^test-","",tools::file_path_sans_ext(unique(c(cases$test,"test-visual-snapshots.R"))))
options(device=function(...) grDevices::pdf(file=NULL))
devtools::test(root,filter=paste0("^(",paste(files,collapse="|"),")$"),stop_on_failure=TRUE)
