test_that("every visual case has exactly one versioned reference and no stale artifacts", {
  cases = flexplot_visual_cases()
  expect_gt(nrow(cases), 0L)
  expect_false(anyDuplicated(cases$reference)>0L)
  expect_setequal(cases$reference, list.files(test_path("_snaps"),pattern="\\.svg$",recursive=TRUE))
  expect_true(all(file.exists(file.path(test_path("_snaps"),cases$reference))))
})

test_that("missing visual references fail before rendering and never create baselines", {
  baseline = withr::local_tempdir()
  reference = "probe.svg"
  expect_failure(expect_flexplot_snapshot(reference,"probe",stop("Must not render"),
    cran=TRUE,baseline_dir=baseline,candidate_dir=""),"Missing SVG baseline")
  expect_length(list.files(baseline,recursive=TRUE),0L)
})

test_that("visual comparisons are exact, repeatable and read-only", {
  skip_if_not_installed("vdiffr")
  baseline = withr::local_tempdir()
  reference = "probe.svg"
  path = file.path(baseline,reference)
  fig = function() graphics::plot(1:3,col="black")
  vdiffr::write_svg(fig,path,"probe")
  original = tools::md5sum(path)
  for (i in seq_len(2)) expect_flexplot_snapshot(reference,"probe",fig,
    cran=TRUE,baseline_dir=baseline,candidate_dir="")
  expect_identical(tools::md5sum(path),original)
  expect_failure(expect_flexplot_snapshot(reference,"probe",function() graphics::plot(1:3,col="red"),
    cran=TRUE,baseline_dir=baseline,candidate_dir=""),"SVG differs")
  expect_identical(tools::md5sum(path),original)
  expect_identical(list.files(baseline),reference)
})

test_that("candidate creation is explicit and cannot overwrite approved references", {
  skip_if_not_installed("vdiffr")
  baseline = withr::local_tempdir()
  candidates = withr::local_tempdir()
  reference = "probe.svg"
  fig = function() graphics::plot(1:3)
  expect_failure(expect_flexplot_snapshot(reference,"probe",fig,
    cran=TRUE,baseline_dir=baseline,candidate_dir=candidates),"Missing SVG baseline")
  expect_false(file.exists(file.path(baseline,reference)))
  expect_true(file.exists(file.path(candidates,reference)))
  file.copy(file.path(candidates,reference),file.path(baseline,reference))
  original = tools::md5sum(file.path(baseline,reference))
  expect_error(expect_flexplot_snapshot(reference,"probe",fig,
    cran=TRUE,baseline_dir=baseline,candidate_dir=baseline),"outside the approved baseline")
  expect_identical(tools::md5sum(file.path(baseline,reference)),original)
})

test_that("the inventory rejects automatic baseline APIs without evaluating tests", {
  directory = withr::local_tempdir()
  writeLines('vdiffr::expect_doppelganger("probe", stop("Must not execute"))',
             file.path(directory,"test-probe.R"))
  expect_error(flexplot_visual_cases(directory),"Use expect_flexplot_snapshot")
})
