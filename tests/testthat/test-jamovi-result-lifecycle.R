test_that("shared clearing primitives preserve schemas and adapter-owned presentation", {
  skip_if_not_installed("jmvcore")
  table = jmvcore::Table$new(columns=list(list(name="term",type="text"),
    list(name="p",type="number",format="zto,pvalue",visible=FALSE)))
  table$addRow(1,list(term="literal",p=.0123456))
  table$setNote("context","Keep until content is cleared")
  table$setVisible(FALSE)
  table$setError("Old failure")
  flexplot:::jamovi_result_reset_status(table)
  expect_identical(table$status,"complete")
  expect_identical(table$.__enclos_env__$private$.error,"")
  expect_equal(table$rowCount,1)
  expect_identical(table$notes$context$note,"Keep until content is cleared")
  columns = table$columns
  flexplot:::jamovi_result_clear(table)
  expect_equal(table$rowCount,0)
  expect_length(table$notes,0)
  expect_false(table$visible)
  expect_identical(table$columns,columns)
  expect_false(table$getColumn("p")$visible)
  expect_identical(table$getColumn("p")$format,"zto,pvalue")
  # A coefficient-only refresh deliberately preserves unrelated table notes.
  data = small
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds="x",graphic=FALSE),data=data)
  analysis$results$glmcat$setNote("context","Preserved coefficient context")
  flexplot:::jamovi_lm_coefficients(analysis$results$glmcat,lm(y~x,data))
  expect_identical(analysis$results$glmcat$notes$context$note,"Preserved coefficient context")
  html = jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(),data)$results$explanation
  flexplot:::jamovi_diagnostic(html,"Old failure",error=TRUE)
  flexplot:::jamovi_result_clear(html)
  expect_identical(html$content,"")
  expect_identical(html$status,"complete")
  expect_true(html$visible) # Visibility is chosen by the adapter, not the primitive.
})

test_that("GLM and Mixed Model resets retain their distinct image and column policies", {
  skip_if_not_installed("jmvcore")
  analyses = list(glm=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(),small),
    mixed=jamovi_mixed_analysis())
  for (name in names(analyses)) {
    results = analyses[[name]]$results
    schema = results$itemNames
    columns = lapply(Filter(function(item)inherits(item,"Table"),results$items),
      function(table)names(table$columns))
    for (item in results$items) {
      item$setVisible(TRUE)
      item$setError("Stale error")
      if (inherits(item,"Table")) {
        item$addRow(1,list())
        item$setNote("stale","Stale table note")
      }
      if (inherits(item,"Html")) item$setContent("<p>Stale content</p>")
    }
    for (plot in c("plot","comparisonplot")) {
      results[[plot]]$setState(list(stale=TRUE))
      results[[plot]]$.setPath("previous-plot.png")
    }
    reset = if (name=="glm") flexplot:::jamovi_glm_reporting_reset else flexplot:::jamovi_mixed_reset
    reset(results)
    expect_identical(results$itemNames,schema)
    expect_identical(lapply(Filter(function(item)inherits(item,"Table"),results$items),
      function(table)names(table$columns)),columns)
    for (item in results$items) {
      expect_identical(item$status,"complete")
      expect_identical(item$.__enclos_env__$private$.error,"")
      expect_identical(item$visible,item$name=="waiting")
      if (inherits(item,"Table")) {
        expect_equal(item$rowCount,0)
        expect_length(item$notes,0)
      }
      if (inherits(item,"Html") && item$name!="waiting") expect_identical(item$content,"")
    }
    for (plot in c("plot","comparisonplot")) {
      expect_null(results[[plot]]$state)
      expect_identical(results[[plot]]$filePath,if(name=="glm") NULL else "previous-plot.png")
    }
    expect_identical(results$waiting$content,if(name=="glm")
      "<p>Select an outcome and predictors, and specify at least one model term.</p>" else
      "<p>Select a numeric outcome and one cluster variable.</p>")
    if (name=="glm") for (column in results$coefficients$columns)
      expect_identical(column$visible,column$name=="term")
  }
})

test_that("LM valid-invalid-valid transitions clear table content without changing its lifecycle policy", {
  skip_if_not_installed("jmvcore")
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds=c("x","a"),graphic=FALSE,includeP=TRUE),data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=small),parent=environment(reader))
  analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  schema = analysis$results$itemNames
  expected = lapply(c("glmcat","factorMeans","diff","rsq"),function(name)analysis$results[[name]]$asDF)
  for (i in 1:2) {
    plot = analysis$results$plot$state
    expect_true(jamovi_reporting_update(analysis,list(modelTerms=list("z"))))
    expect_identical(analysis$results$glmcat$status,"error")
    for (name in c("glmcat","factorMeans","diff","rsq")) {
      expect_equal(analysis$results[[name]]$rowCount,0)
      expect_length(analysis$results[[name]]$notes,0)
    }
    expect_false(analysis$results$factorMeans$visible)
    expect_false(analysis$results$diff$visible)
    # LM's existing image-state policy is deliberately outside this extraction.
    expect_identical(analysis$results$plot$state,plot)
    expect_true(jamovi_reporting_update(analysis,list(modelTerms=NULL)))
    expect_identical(analysis$results$itemNames,schema)
    expect_equal(lapply(c("glmcat","factorMeans","diff","rsq"),function(name)analysis$results[[name]]$asDF),expected)
    jamovi_expect_lm_coefficients(analysis,lm(y~x+a,small),includeP=TRUE)
  }
})

test_that("neutral HTML escaping retains the Mixed Model compatibility entry point", {
  text = c("<script>x & y</script>","'quoted' & \"quoted\"",NA_character_,"")
  expected = c("&lt;script&gt;x &amp; y&lt;/script&gt;","'quoted' &amp; \"quoted\"",NA_character_,"")
  expect_identical(flexplot:::jamovi_html_escape(text),expected)
  expect_identical(flexplot:::jamovi_mixed_escape(text),expected)
  expect_identical(flexplot:::jamovi_html_escape(character()),character())
})
