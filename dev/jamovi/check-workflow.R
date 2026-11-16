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
# File-opening uses a new copy and the ordinary macOS document-open command.
commands$jamovi_dev_run = function(command,args=character(),log=NULL) {
  record$items[[length(record$items)+1L]] = list(command=command,args=args)
  invisible(0L)
}
record = new.env(parent=emptyenv());record$items = list()
original = file.path(work,"original.omv");writeBin(charToRaw("test document"),original)
source_hash = tools::md5sum(original)
copy1 = commands$jamovi_dev_open(work,"/Applications/jamovi.app",original)
copy2 = commands$jamovi_dev_open(work,"/Applications/jamovi.app",original)
stopifnot(copy1!=copy2,identical(unname(tools::md5sum(copy1)),unname(source_hash)),
  identical(source_hash,tools::md5sum(original)),record$items[[1]]$command=="/usr/bin/open",
  identical(record$items[[1]]$args[1:2],c("-a","/Applications/jamovi.app")))
# Invalid invocations fail before any install or file-opening command.
stopifnot(inherits(try(commands$jamovi_dev_main(c("open","--file",file.path(work,"absent.omv")),root),silent=TRUE),"try-error"),
  inherits(try(commands$jamovi_dev_main("invalid",root),silent=TRUE),"try-error"),length(record$items)==2L)
unlink(c(work,stage),recursive=TRUE)
cat("PASS: isolated generation leaves source unchanged; document copies are distinct; invalid commands fail safely.\n")
