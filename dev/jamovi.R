#!/usr/bin/env Rscript
# Development-only commands. No module installation happens in test/ui modes.
jamovi_dev_node = function() {
  node = Sys.which("node")
  if (nzchar(node)) return(unname(node))
  candidates = Sys.glob(path.expand("~/.nvm/versions/node/*/bin/node"))
  if (!length(candidates)) stop("Node is unavailable. Activate your existing Node installation.")
  versions = sub("^v","",basename(dirname(dirname(candidates))))
  candidates[order(numeric_version(versions),decreasing=TRUE)[1]]
}

jamovi_dev_run = function(command, args=character(), log=NULL) {
  if (is.null(log)) {
    status = system2(command,shQuote(args))
  } else {
    status = system2(command,shQuote(args),stdout=log,stderr=log)
  }
  if (!identical(as.integer(status),0L)) {
    if (!is.null(log) && file.exists(log)) cat(tail(readLines(log,warn=FALSE),30),sep="\n")
    stop("Command failed (exit ",status,")",if (!is.null(log)) paste0(". Full log: ",log) else "")
  }
  invisible(status)
}

jamovi_dev_stage = function(root) {
  stage = tempfile("flexplot-jamovi-")
  dir.create(stage)
  # jmvtools writes headers, NAMESPACE and build files: run it only on this copy.
  entries = list.files(root,all.files=TRUE,no..=TRUE,full.names=TRUE)
  exclude = c(".git",".jamovi-dev","build","temp","dev",".Rproj.user")
  entries = entries[!basename(entries) %in% exclude & !grepl("\\.(jmo|Rproj)$",entries)]
  if (!all(file.copy(entries,stage,recursive=TRUE,copy.mode=TRUE))) stop("Could not stage the package")
  cache = file.path(root,".jamovi-dev","cache")
  if (!dir.exists(cache)) cache = file.path(root,"build")
  if (dir.exists(cache)) {
    dir.create(file.path(stage,"build"))
    cached = list.files(cache,full.names=TRUE)
    if (!all(file.copy(cached,file.path(stage,"build"),recursive=TRUE))) stop("Could not copy the dependency cache")
  }
  stage
}

jamovi_dev_open = function(root, home, file) {
  if (!file.exists(file)) stop("Saved analysis file does not exist: ",file)
  if (tolower(tools::file_ext(file)) != "omv") stop("Choose a saved .omv file")
  sessions = file.path(root,".jamovi-dev","open-sessions")
  dir.create(sessions,recursive=TRUE,showWarnings=FALSE)
  copy = tempfile(paste0(tools::file_path_sans_ext(basename(file)),"-"),tmpdir=sessions,fileext=".omv")
  if (!file.copy(file,copy)) stop("Could not create a disposable copy")
  jamovi_dev_run("/usr/bin/open",c("-a",home,copy))
  cat("Sent a disposable document to jamovi; no close or quit commands were sent.\n",copy,"\n",sep="")
  invisible(copy)
}

jamovi_dev_main = function(args, root) {
  if (!length(args) || args[1] %in% c("help","--help","-h")) {
    cat("Usage: Rscript dev/jamovi.R <ui|test|install|fixture|open> [--open] [--deps] [--force] [--file PATH]\n",
      "  ui       Compile/bundle definitions and run installed-UI handler tests; no install\n",
      "  test     Run focused jamovi R6 tests; no install\n",
      "  install  Build an isolated copy and update running jamovi (cached dependencies)\n",
      "           --deps explicitly permits dependency installation/refresh\n",
      "           --open opens a disposable copy of the smoke fixture after installation\n",
      "  fixture  Generate preconfigured LM, LM interaction, logistic, and Poisson .omv\n",
      "           --force replaces the generated fixture, never a user session\n",
      "  open     Open a disposable copy of the fixture, or --file your-saved.omv\n",
      "JAMOVI_HOME defaults to /Applications/jamovi.app.\n",sep="")
    return(invisible(NULL))
  }
  command = args[1]
  flags = args[-1]
  file = file.path(root,".jamovi-dev","smoke.omv")
  custom_file = "--file" %in% flags
  if ("--file" %in% flags) {
    index = match("--file",flags)
    if (index == length(flags) || startsWith(flags[index+1],"--")) stop("--file needs a path")
    file = normalizePath(path.expand(flags[index+1]),mustWork=FALSE)
    flags = flags[-c(index,index+1)]
  }
  if (any(!flags %in% c("--open","--deps","--force"))) stop("Unknown option; use --help")
  if (!command %in% c("ui","test","install","fixture","open")) stop("Unknown command; use --help")
  home = path.expand(Sys.getenv("JAMOVI_HOME","/Applications/jamovi.app"))
  output = file.path(root,".jamovi-dev")
  dir.create(output,showWarnings=FALSE)
  if (command == "test") {
    if (!requireNamespace("devtools",quietly=TRUE)) stop("devtools is required for test mode")
    previous_path = Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH=previous_path),add=TRUE)
    Sys.setenv(PATH=paste(dirname(jamovi_dev_node()),previous_path,sep=.Platform$path.sep))
    result = devtools::test(root,filter="jamovi",stop_on_failure=TRUE)
    if (any(as.data.frame(result)$error)) stop("Focused R6 tests reported an error")
    cat("PASS: focused jamovi tests; no module installed.\n")
    return(invisible(NULL))
  }
  home = normalizePath(home,mustWork=TRUE)
  if (command == "ui") {
    compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
    if (!nzchar(compiler)) stop("jmvtools is required for UI compilation")
    node = jamovi_dev_node()
    work = tempfile("jamovi-ui-");dir.create(work)
    on.exit(unlink(work,recursive=TRUE),add=TRUE)
    jamovi_dev_run(node,c(file.path(root,"dev/jamovi/validate-ui.mjs"),root,compiler,work))
    runtime = Sys.glob(file.path(home,"Contents/Resources/jamovi/client/dist/assets/analysisui-*.js"))
    if (!length(runtime)) stop("Installed jamovi UI runtime not found; cannot verify handlers")
    for (analysis in c("glmbasic","glmgeneralized")) jamovi_dev_run(node,c(
      file.path(root,"tests/testthat/js/jamovi-glm-ui.cjs"),root,compiler,
      file.path(work,paste0(analysis,".tsv")),runtime[1],analysis))
    cat("PASS: installed-UI handler tests; no module installed.\n")
    return(invisible(NULL))
  }
  if (custom_file && command != "fixture" && !file.exists(file)) stop("Saved .omv file does not exist: ",file)
  refresh_default = command == "install" && "--open" %in% flags && !custom_file
  if (command == "fixture" || (command == "open" && !file.exists(file)) || refresh_default) {
    if (file.exists(file) && !"--force" %in% flags && !refresh_default) stop("Fixture already exists; use --force to regenerate it")
    source(file.path(root,"dev/jamovi/fixture.R"))
    jamovi_smoke_fixture(root,home,file)
    cat("Created preconfigured smoke fixture: ",file,"\n",sep="")
    if (command == "fixture") return(invisible(NULL))
  }
  if (command == "open") return(jamovi_dev_open(root,home,file))
  compiler = system.file("node_modules/jamovi-compiler",package="jmvtools")
  if (!nzchar(compiler)) stop("jmvtools is required for installation")
  stage = jamovi_dev_stage(root)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  if (!dir.exists(file.path(stage,"build")) && !"--deps" %in% flags) stop("No dependency cache. Run install --deps once.")
  stamp = paste0("dev-",format(Sys.time(),"%Y%m%d%H%M%S"),"-",Sys.getpid())
  log = file.path(output,"install.log")
  build_args = c(file.path(compiler,"index.js"),"--install",stage,"--home",home,"--build-hash",stamp)
  if (!"--deps" %in% flags) build_args = c(build_args,"--skip-deps","--skip-remotes")
  cat("Building in an isolated directory. Log: ",log,"\n",sep="")
  cat("jamovi may briefly recalculate open analyses while its R engines restart.\n")
  started = Sys.time()
  jamovi_dev_run(jamovi_dev_node(),build_args,log=log)
  installed = file.path(path.expand("~/Library/Application Support/jamovi/modules/flexplot"),"jamovi.yaml")
  confirmed = FALSE
  for (attempt in seq_len(60)) {
    if (file.exists(installed) && any(grepl(stamp,readLines(installed,warn=FALSE),fixed=TRUE))) {
      confirmed = TRUE;break
    }
    Sys.sleep(.5)
  }
  if (!confirmed) stop("Installer returned, but the installed build stamp was not confirmed. Check ",log)
  if ("--deps" %in% flags) {
    cache = file.path(output,"cache");dir.create(cache,showWarnings=FALSE)
    if (!all(file.copy(list.files(file.path(stage,"build"),full.names=TRUE),cache,recursive=TRUE)))
      warning("Installed, but could not retain the dependency cache")
  }
  cat("PASS: installed ",stamp," in ",round(as.numeric(difftime(Sys.time(),started,units="secs")),1),
      " s; application restart not requested.\n",sep="")
  if ("--open" %in% flags) jamovi_dev_open(root,home,file)
  invisible(stamp)
}

if (sys.nframe() == 0L) {
  script = sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
  root = normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
  tryCatch(jamovi_dev_main(commandArgs(TRUE),root),error=function(error) {
    cat("FAIL: ",conditionMessage(error),"\n",sep="",file=stderr())
    quit(status=1L)
  })
}
