# Tripwire for checkout/host dependencies in every packaged R test and helper.
# This is static inspection only: test expressions and paths are never evaluated.
jamovi_test_source_dependencies = function(root, directory=file.path(root,"tests")) {
  patterns = readLines(file.path(root,".Rbuildignore"),warn=FALSE)
  patterns = patterns[nzchar(patterns) & !startsWith(trimws(patterns),"#")]
  description = read.dcf(file.path(root,"DESCRIPTION"))
  fields = intersect(c("Depends","Imports","Suggests","LinkingTo"),colnames(description))
  declared = trimws(gsub("\\s*\\([^)]*\\)","",strsplit(paste(description[1,fields],collapse=","),",")[[1]]))
  declared = unique(c(declared,description[1,"Package"],rownames(utils::installed.packages(priority="base"))))
  rows = list()
  excluded = function(candidates) {
    candidates = sub("^(\\.\\./)+","",candidates)
    repeat {
      shortened = gsub("[^/]+/\\.\\./","",candidates)
      if (identical(shortened,candidates)) break
      candidates = shortened
    }
    prefixes = unlist(lapply(strsplit(candidates,"/",fixed=TRUE),function(parts)
      vapply(seq_along(parts),function(i) paste(parts[seq_len(i)],collapse="/"),character(1))))
    any(vapply(patterns,function(pattern) any(grepl(pattern,prefixes)),logical(1)))
  }
  call_name = function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (is.call(x) && is.symbol(x[[1]]) && as.character(x[[1]]) %in% c("::",":::")) return(as.character(x[[3]]))
    ""
  }
  strings = function(x) {
    if (is.character(x)) return(x)
    # Installed-resource lookups are opaque to their surrounding read call.
    if (is.call(x) && identical(call_name(x[[1]]),"system.file")) return(character())
    if (!is.recursive(x)) return(character())
    unlist(lapply(seq_along(x),function(i) if (identical(x[[i]],quote(expr=))) character() else strings(x[[i]])),use.names=FALSE)
  }
  walk = function(x, file) {
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return(invisible(NULL))
    if (is.call(x)) {
      name = call_name(x[[1]])
      literals = strings(as.list(x)[-1])
      reason = NULL
      if (name %in% c("skip_if_not_installed","requireNamespace","loadNamespace","library","require") &&
          length(literals) && !literals[1] %in% declared) reason = "undeclared test dependency"
      if (name == "system.file" && "jmvtools" %in% literals) reason = "development UI compiler"
      if (name == "load_all") reason = "development checkout loader"
      if (name == "system.file" && any(grepl("^(/Users/|/Applications/|/Library/)",literals)))
        reason = "absolute checkout/host library"
      path_calls = c("file.path","test_path","source","sys.source","load","readLines","readRDS","read.delim",
        "read.csv","read_yaml","read_json","readBin","file","unz","Sys.glob","path.expand","normalizePath",
        "local_libpaths",".libPaths","file.exists","dir.exists","setwd","system2","system","scan")
      if (name %in% path_calls && length(literals)) {
        if (any(grepl("^(/Users/|/Applications/|/Library/|~[/\\\\])",literals))) reason = "absolute checkout/host path"
        if (name == "test_path" && sum(literals == "..") >= 2L) reason = "path escapes packaged tests"
        candidates = unique(c(literals,paste(literals,collapse="/")))
        if (name == "test_path") candidates = c(candidates,paste0("tests/testthat/",paste(literals,collapse="/")))
        if (excluded(candidates))
          reason = "resource excluded by .Rbuildignore"
        if (name %in% c("source","sys.source") && any(grepl("^R(/|$)",candidates))) reason = "raw checkout R source"
      }
      if (!is.null(reason)) rows[[length(rows)+1L]] <<- data.frame(file=file,call=name,reason=reason)
    }
    for (i in seq_along(x)) if (!identical(x[[i]],quote(expr=))) walk(x[[i]],file)
    invisible(NULL)
  }
  for (file in list.files(directory,pattern="\\.[Rr]$",full.names=TRUE,recursive=TRUE)) walk(parse(file,keep.source=TRUE),file)
  # Inspect quoted paths in auxiliary scripts too; pure packaged JS fixtures are valid.
  for (file in list.files(directory,pattern="\\.(cjs|mjs|js)$",full.names=TRUE,recursive=TRUE)) {
    code = paste(readLines(file,warn=FALSE),collapse="\n")
    literals = regmatches(code,gregexpr("\"[^\"\n]*\"|'[^'\n]*'",code,perl=TRUE))[[1]]
    literals = substring(literals,2L,nchar(literals)-1L)
    if (excluded(literals) || any(grepl("^(/Users/|/Applications/|/Library/)",literals)))
      rows[[length(rows)+1L]] = data.frame(file=file,call="auxiliary script",reason="source-only or host-runtime path")
  }
  if (!length(rows)) return(data.frame(file=character(),call=character(),reason=character()))
  unique(do.call(rbind,rows))
}
