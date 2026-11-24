isolation_resources = function(analysis) {
  output = tempfile("isolation-")
  dir.create(output)
  analysis$.setResourcesPathSource(function(name,ext)
    list(rootPath=output,relPath=paste0(name,".",ext)))
  output
}

isolation_image = function(analysis, name, output, error=NULL) {
  image = analysis$results[[name]]
  expect_true(suppressWarnings(analysis$.createImage(paste0(".",name),image,noThrow=TRUE)))
  expect_gt(file.info(file.path(output,image$filePath))$size,1000)
  expect_identical(image$status,if (is.null(error)) "complete" else "error")
  if (!is.null(error)) {
    expect_match(image$.__enclos_env__$private$.error,error,fixed=TRUE)
    expect_null(image$state)
  }
}

isolation_title = function(rows, text) {
  index = which(vapply(rows,function(row) row$role=="title",logical(1)))[1]
  rows[[index]]$label = text
  rows
}
