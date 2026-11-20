# Development build identity. Never changes a source version to hide a mismatch.
jamovi_dev_versions = function(root) {
  package = unname(read.dcf(file.path(root,"DESCRIPTION"))[1,"Version"])
  module = as.character(yaml::read_yaml(file.path(root,"jamovi/0000.yaml"))$version)
  if(!identical(package,module)) stop("Version synchronization error: DESCRIPTION ",package," versus jamovi/0000.yaml ",module)
  list(package=package,module=module)
}

jamovi_dev_identity = function(root) {
  output = tempfile();on.exit(unlink(output),add=TRUE)
  jamovi_dev_run(jamovi_dev_node(),c(file.path(root,"dev/jamovi/build-identity.cjs"),"source",root),log=output)
  jsonlite::read_json(output)
}

jamovi_dev_payload = function(root, archive, installed, compiler) {
  output = tempfile();on.exit(unlink(output),add=TRUE)
  jamovi_dev_run(jamovi_dev_node(),c(file.path(root,"dev/jamovi/build-identity.cjs"),"payload",archive,installed,compiler),log=output)
  jsonlite::read_json(output)
}

jamovi_dev_record_install = function(root, archive, compiler, identity, versions, stamp, output) {
  installed = path.expand("~/Library/Application Support/jamovi/modules/flexplot")
  metadata = yaml::read_yaml(file.path(installed,"jamovi.yaml"))
  description = read.dcf(file.path(installed,"R/flexplot/DESCRIPTION"))[1,"Version"]
  if(!identical(as.character(metadata$version),versions$module) || !identical(unname(description),versions$package))
    stop("Installed module and R package versions do not match the synchronized source")
  payload = jamovi_dev_payload(root,archive,installed,compiler)
  receipt = list(schema_version=1,source=identity,versions=versions,stamp=stamp,
    archive=normalizePath(archive),installed=installed,payload=payload,
    commit=system2("git",c("-C",shQuote(root),"rev-parse","HEAD"),stdout=TRUE))
  jsonlite::write_json(receipt,file.path(output,"build-receipt.json"),auto_unbox=TRUE,pretty=TRUE,digits=NA)
  invisible(receipt)
}
