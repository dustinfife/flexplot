# Exact vdiffr SVG rendering, without testthat's automatic baseline creation.
expect_flexplot_snapshot = function(reference, title, fig, cran=FALSE,
    baseline_dir=testthat::test_path("_snaps"),
    candidate_dir=Sys.getenv("FLEXPLOT_SNAPSHOT_CANDIDATES", "")) {
  stopifnot(is.character(reference), length(reference)==1L,
    !grepl("^/", reference), !any(strsplit(reference,"/",fixed=TRUE)[[1]] %in% c("..","")))
  testthat::local_edition(3)
  testthat::announce_snapshot_file(name=basename(reference))
  path = file.path(baseline_dir, reference)
  present = file.exists(path)
  instruction = paste("Missing SVG baseline:", reference,
    "\nRun dev/visual-tests.R --candidates DIR, review the SVG, then explicitly copy the approved reference.")
  if (!present && !nzchar(candidate_dir)) {
    testthat::expect_true(present, info=instruction)
    return(invisible(NULL))
  }
  if (!cran) testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  if (nzchar(candidate_dir)) {
    candidate_dir = normalizePath(candidate_dir, mustWork=TRUE)
    protected = normalizePath(baseline_dir, mustWork=TRUE)
    if (identical(candidate_dir,protected) || startsWith(candidate_dir,paste0(protected,"/")))
      stop("Review candidates must be outside the approved baseline directory.")
  }
  actual = tempfile(fileext=".svg")
  withr::defer(unlink(actual))
  vdiffr::write_svg(fig, actual, title)
  if (nzchar(candidate_dir)) {
    candidate = file.path(candidate_dir, reference)
    dir.create(dirname(candidate), recursive=TRUE, showWarnings=FALSE)
    if (!file.copy(actual,candidate,overwrite=TRUE)) stop("Could not write review candidate: ",candidate)
  }
  if (!present) {
    testthat::expect_true(present, info=instruction)
    return(invisible(NULL))
  }
  testthat::expect_true(testthat::compare_file_text(path,actual),
    info=paste("SVG differs from approved baseline:",reference,
      "\nRun dev/visual-tests.R --candidates DIR to inspect candidates; ordinary tests never update references."))
  invisible(NULL)
}

# Inspect test expressions without evaluating them; references are explicit literals.
flexplot_visual_cases = function(directory=testthat::test_path()) {
  cases = new.env(parent=emptyenv())
  cases$rows = list()
  walk = function(x, file) {
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return(invisible(NULL))
    if (is.call(x) && (identical(x[[1]],quote(vdiffr::expect_doppelganger)) ||
        identical(x[[1]],as.name("expect_doppelganger"))))
      stop("Use expect_flexplot_snapshot instead of the automatic snapshot API: ",file)
    if (is.call(x) && identical(x[[1]],as.name("expect_flexplot_snapshot")) &&
        length(x)>=4L && is.character(x[[2]]) && is.character(x[[3]])) {
      cases$rows[[length(cases$rows)+1L]] = data.frame(test=basename(file), reference=x[[2]], title=x[[3]])
    }
    for (i in seq_along(x)) {
      if (!identical(x[[i]],quote(expr=))) walk(x[[i]],file)
    }
    invisible(NULL)
  }
  for (file in list.files(directory,pattern="^test.*\\.R$",full.names=TRUE)) walk(parse(file),file)
  do.call(rbind,cases$rows)
}
