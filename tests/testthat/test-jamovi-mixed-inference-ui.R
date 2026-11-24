test_that("Mixed fixed-effect inference presentation keeps compatibility without unsupported p columns", {
  skip_if_not_installed("jmvcore")
  for (comparison in c(FALSE,TRUE)) for (intervals in c(FALSE,TRUE)) {
    analysis=jamovi_mixed_analysis(compare=comparison,modelTerms=list("x","z"),comparisonTerms=list("x"),
      includeP=TRUE,includeCI=intervals)
    expect_true(analysis$run())
    for(name in if(comparison)c("fixed","fixed2") else "fixed") {
      table=analysis$results[[name]]
      expect_false(table$getColumn("p")$visible)
      expect_true("p" %in% vapply(table$columns,function(column)column$name,character(1)))
      expect_true(all(is.na(table$asDF$p)))
      for(column in c("lower","upper")) expect_identical(table$getColumn(column)$visible,intervals)
      expect_null(table$notes$p)
    }
    expect_identical(analysis$results$fixed$notes$intervals$note,
      if(intervals) "95% normal-Wald confidence intervals." else NULL)
    for(name in c("random","random2")) expect_length(analysis$results[[name]]$notes,0L)
    numbers=function()lapply(Filter(function(item)inherits(item,"Table"),analysis$results$items),function(item) {value=item$asDF;value[names(value)!="p"]})
    values=numbers()
    p=if(comparison)analysis$results$comparison$getCell("p",rowNo=1)$value else NULL
    expect_true(jamovi_reporting_update(analysis,list(includeP=FALSE)))
    expect_equal(numbers(),values,tolerance=0)
    if(comparison)expect_identical(analysis$results$comparison$getCell("p",rowNo=1)$value,p)
    expect_false(analysis$results$fixed$getColumn("p")$visible)
    if(comparison) {
      expect_false(analysis$results$fixed2$getColumn("p")$visible)
      expect_false(analysis$results$comparison$getColumn("p")$visible)
      expect_true(jamovi_reporting_update(analysis,list(includeP=TRUE,compare=FALSE)))
      expect_true(analysis$options$includeP)
      expect_true(jamovi_reporting_update(analysis,list(compare=TRUE)))
      expect_true(analysis$results$comparison$getColumn("p")$visible)
      expect_true(any(is.finite(analysis$results$comparison$asDF$p)))
    }
  }
  options=flexplot:::mixedmodelOptions$new()
  expect_true(options$includeCI)
})
