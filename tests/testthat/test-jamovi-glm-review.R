test_that("GLM column-wide notes use matching sequential letters and retain only nonobvious explanations", {
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),
    family="binomial",graphic=FALSE,comparisonGraphic=FALSE,compare=TRUE,comparisonX="x"),small)
  expect_true(analysis$run())
  check = function(table) {
    notes = Filter(function(note)grepl("^[ᵃᵇᶜᵈᵉᶠᵍʰⁱʲ] ",note$note),table$notes)
    markers = substr(vapply(notes,`[[`,character(1),"note"),1,1)
    expect_identical(unname(markers),c("ᵃ","ᵇ","ᶜ","ᵈ","ᵉ","ᶠ")[seq_along(markers)])
    titles = vapply(Filter(function(column)column$visible,table$columns),function(column)column$title,character(1))
    for (marker in markers) expect_true(any(grepl(marker,titles,fixed=TRUE)))
    for (column in table$columns) for (i in seq_len(table$rowCount)) {
      if (column$name!="intercept_threshold") expect_length(column$getCell(i)$footnotes,0L)
    }
  }
  for (name in c("coefficients","irt","probabilities","comparison")) check(analysis$results[[name]])
  table = analysis$results$comparison
  expect_null(table$notes$column_population)
  expect_false(table$getColumn("p")$visible)
  expect_null(table$notes$column_test)
  expect_true(jamovi_reporting_update(analysis,list(includeP=TRUE)))
  expect_true(table$getColumn("p")$visible)
  expect_match(table$getColumn("p")$title,"ᵇ$",fixed=FALSE)
  expect_match(table$notes$column_test$note,"^ᵇ ")
  before = analysis$results$coefficients$asDF
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x","a",c("x","a")),interactions=TRUE)))
  expect_match(analysis$results$coefficients$notes$column_interaction$note,"Interaction")
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x","a"),interactions=FALSE)))
  expect_null(analysis$results$coefficients$notes$column_interaction)
  expect_equal(analysis$results$coefficients$asDF,before)
  expect_true(jamovi_reporting_update(analysis,list(preds="x")))
  expect_null(analysis$results$coefficients$notes$column_standardized)
  expect_null(analysis$results$irt$notes$column_reference)
  expect_length(analysis$results$probabilities$notes,0L)
  expect_length(analysis$results$getRefs(),0L)
  expect_identical(analysis$results$getRefs(recurse=TRUE),c("flexplot","graphicalAnalysis"))
})

test_that("GLM common-sample notes only label additional exclusions and renumber on change", {
  data = small
  data$z[1:3] = NA_real_
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y",preds=c("x","z"),
    family="gaussian",modelTerms=list("x"),comparisonTerms=list("x","z"),compare=TRUE,
    graphic=FALSE,comparisonGraphic=FALSE,comparisonX="x",includeP=TRUE),data)
  expect_true(analysis$run())
  table = analysis$results$comparison
  expect_match(table$notes$column_population$note,"additional observations from Model 1",fixed=TRUE)
  expect_identical(table$getColumn("n")$title,"Nᵃ")
  expect_identical(table$getColumn("bayes.factor")$title,"BIC Bayes factorᵇ")
  expect_identical(table$getColumn("p")$title,"pᶜ")
  expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("x","z"))))
  expect_null(table$notes$column_population)
  expect_identical(table$getColumn("n")$title,"N")
  expect_identical(table$getColumn("bayes.factor")$title,"BIC Bayes factorᵃ")
  expect_identical(table$getColumn("p")$title,"pᵇ")
})

test_that("routine logistic overlay text is exact when no qualification is needed", {
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds="x",family="binomial",graphic=FALSE),small)
  expect_true(analysis$run())
  guide = flexplot:::jamovi_glm_plot_guide(analysis$results$plot$state,"x")
  expect_identical(guide,"<p>Logistic overlay plot: dots represent observed proportions within bins of the x-axis variable.</p>")
  expect_identical(flexplot:::jamovi_glm_plot_guide(analysis$results$plot$state,"x",style="raw"),"")
})

test_that("undefined IRT thresholds retain specific warnings without reusing header letters", {
  original = flexplot:::estimates.glm
  local_mocked_bindings(estimates.glm=function(...) {
    values = original(...)
    irt = attr(values,"irt")
    irt$intercept_threshold[1] = NA_real_
    attr(values,"irt") = irt
    values
  },.package="flexplot")
  analysis = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","z"),
    family="binomial",graphic=FALSE),small)
  expect_true(analysis$run())
  expect_identical(analysis$results$irt$getCell("intercept_threshold",rowNo=1)$symbols,"†")
  expect_match(analysis$results$irt$notes$undefined$note,"† Blank thresholds are undefined or nonunique.",fixed=TRUE)
  expect_match(analysis$results$irt$getColumn("variable")$title,"ᵃ$",fixed=FALSE)
})
