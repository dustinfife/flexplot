#!/usr/bin/env Rscript
# Infrastructure checks only: no installation and no GUI launch.
script = sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
root = normalizePath(file.path(dirname(script),"../.."))
commands = new.env(parent=globalenv())
sys.source(file.path(root,"dev/jamovi.R"),commands)
work = tempfile("jamovi-workflow-check-");dir.create(work)
tracked = c(file.path(root,c("DESCRIPTION","NAMESPACE","R/00jmv.R")),
  list.files(file.path(root,"R"),pattern="\\.h\\.R$",full.names=TRUE))
before = tools::md5sum(tracked)
stage = commands$jamovi_dev_stage(root)
stopifnot(!dir.exists(file.path(stage,".git")),!dir.exists(file.path(stage,".jamovi-dev")),
  !dir.exists(file.path(stage,"dev")),file.exists(file.path(stage,"jamovi/glmgeneralized.a.yaml")))
compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
home = Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app")
version = system2("/usr/libexec/PlistBuddy",shQuote(c("-c","Print CFBundleShortVersionString",
  file.path(home,"Contents/Info.plist"))),stdout=TRUE)
version = paste(strsplit(version[1],".",fixed=TRUE)[[1]][1:3],collapse=".")
commands$jamovi_dev_run(commands$jamovi_dev_node(),c(file.path(compiler,"index.js"),"--prepare",stage,
  "--home",home,"--assume-app-version",version))
stopifnot(identical(before,tools::md5sum(tracked)))
# Mock all launch/window APIs: these checks never open jamovi.
record = new.env(parent=emptyenv());record$items = list();record$gui = list()
record$running = FALSE;record$open = NULL;record$blocked = FALSE;record$launch_error = FALSE
commands$jamovi_dev_run = function(command,args=character(),log=NULL) {
  record$items[[length(record$items)+1L]] = list(command=command,args=args)
  if (command == "/usr/bin/open") {
    stopifnot(!"-n" %in% args)
    if (record$launch_error) stop("simulated launch failure")
    record$running = TRUE;record$open = tools::file_path_sans_ext(basename(tail(args,1)))
    stopifnot(inherits(try(commands$jamovi_dev_cleanup(work),silent=TRUE),"try-error"))
  }
  invisible(0L)
}
commands$jamovi_dev_gui = function(root,action,stem="") {
  record$gui[[length(record$gui)+1L]] = c(action,stem)
  if (record$blocked) stop("simulated macOS permission failure")
  if (action == "wait") stopifnot(record$running,identical(record$open,stem))
  if (action == "close" && identical(record$open,stem)) record$open = NULL
  invisible("confirmed")
}
original = file.path(work,"original.omv");writeBin(charToRaw("test document"),original)
source_hash = tools::md5sum(original)
registry = file.path(work,".jamovi-dev/gui-session/document.rds")
# Nonrunning launch succeeds and leaves its document open after returning.
copy1 = commands$jamovi_dev_main(c("open","--file",original),work)
stopifnot(record$running,file.exists(registry),identical(readRDS(registry),copy1),
  identical(record$open,tools::file_path_sans_ext(basename(copy1))),length(record$gui)==1L,
  !dir.exists(file.path(work,".jamovi-dev/operation-lock")),
  identical(unname(tools::md5sum(copy1)),unname(source_hash)))
# Running app: only the registered predecessor closes before the next copy opens.
copy2 = commands$jamovi_dev_main(c("open","--file",original),work)
stopifnot(copy1 != copy2,identical(record$gui[[2]],c("close",tools::file_path_sans_ext(basename(copy1)))),
  identical(readRDS(registry),copy2),identical(source_hash,tools::md5sum(original)))
# Permission failures block replacement and preserve the existing ownership record.
record$blocked = TRUE;nlaunch = length(record$items)
stopifnot(inherits(try(commands$jamovi_dev_main(c("open","--file",original),work),silent=TRUE),"try-error"),
  length(record$items)==nlaunch,identical(readRDS(registry),copy2),
  inherits(try(commands$jamovi_dev_main("cleanup",work),silent=TRUE),"try-error"))
record$blocked = FALSE
commands$jamovi_dev_main("cleanup",work)
stopifnot(!file.exists(registry),is.null(record$open))
commands$jamovi_dev_main("cleanup",work)
# Launch failure retains ownership, with no exit-time closing or prompt.
record$launch_error = TRUE
stopifnot(inherits(try(commands$jamovi_dev_open(work,home,original),silent=TRUE),"try-error"),file.exists(registry))
record$launch_error = FALSE;record$running = FALSE
commands$jamovi_dev_main("cleanup",work)
# Real install --open control flow; external installer and stamp check are mocked.
commands$jamovi_dev_stage = function(root) {
  path = tempfile("mock-install-");dir.create(path);dir.create(file.path(path,"build"));path
}
commands$jamovi_dev_confirm_install = function(stamp,log) stopifnot(startsWith(stamp,"source-sha256-"))
# A fake source root supplies explicit identity/receipt stand-ins; no installed module is touched.
dir.create(file.path(work,"dev/jamovi"),recursive=TRUE)
writeLines(c(
  'jamovi_dev_versions = function(root) list(package="0.27.0",module="0.27.0")',
  'jamovi_dev_identity = function(root) list(fingerprint="mock-source")',
  'jamovi_dev_record_install = function(root,archive,compiler,identity,versions,stamp,output) { stopifnot(identical(stamp,"source-sha256-mock-source")); invisible(NULL) }'
),file.path(work,"dev/jamovi/build-identity.R"))
for (running in c(FALSE,TRUE)) {
  record$running = running
  commands$jamovi_dev_main(c("install","--open","--file",original),work)
  stopifnot(file.exists(registry),record$running,!is.null(record$open),
    !dir.exists(file.path(work,".jamovi-dev/operation-lock")))
}
commands$jamovi_dev_main("cleanup",work)
# Invalid inputs and overlapping commands never launch anything.
nlaunch = length(record$items)
lock = commands$jamovi_dev_lock(work)
stopifnot(inherits(try(commands$jamovi_dev_main(c("open","--file",original),work),silent=TRUE),"try-error"))
unlink(lock,recursive=TRUE)
stopifnot(inherits(try(commands$jamovi_dev_main(c("open","--file",file.path(work,"absent.omv")),work),silent=TRUE),"try-error"),
  inherits(try(commands$jamovi_dev_main("invalid",work),silent=TRUE),"try-error"),length(record$items)==nlaunch)
# Run the actual macOS helper against a simulated System Events API.
system_status = system2(commands$jamovi_dev_node(),shQuote(c(file.path(root,"dev/jamovi/check-gui-session.cjs"),root)))
stopifnot(system_status==0L)
unlink(c(work,stage),recursive=TRUE)
cat("PASS: open/install --open/cleanup, running and nonrunning app, permissions, launch errors, locks, preserved originals; no GUI launched.\n")
