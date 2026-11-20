test_that("both development commands fail for invalid UI metadata or JavaScript", {
  for (kind in c("metadata","javascript")) {
    directory = withr::local_tempdir()
    for (name in c("jamovi","dev","tests"))
      expect_true(file.copy(file.path(root,name),directory,recursive=TRUE))
    expect_true(file.copy(file.path(root,".Rbuildignore"),directory))
    expect_true(file.copy(file.path(root,"DESCRIPTION"),directory))
    if (kind == "metadata") {
      file = file.path(directory,"jamovi/glmgeneralized.r.yaml")
      definition = yaml::read_yaml(file)
      definition$refs = "missing-test-citation"
      yaml::write_yaml(definition,file)
    } else cat("\nfunction broken( {\n",file=file.path(directory,"jamovi/js/glmgeneralized.js"),append=TRUE)
    log = tempfile()
    withr::defer(unlink(log))
    command = if (kind == "metadata") "ui" else "test"
    status = system2(file.path(R.home("bin"),"Rscript"),shQuote(c("--vanilla",file.path(directory,"dev/jamovi.R"),command)),
      stdout=log,stderr=log)
    expect_false(identical(as.integer(status),0L))
    expect_match(paste(readLines(log,warn=FALSE),collapse="\n"),
      if (kind == "metadata") "Missing citation" else "Unexpected|ParseError|Syntax")
  }
})
