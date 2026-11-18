test_that("CAS ghost notes follow actual directional lines and clear on option changes", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x","z"),comparisonTerms=list("z"),casGhost=TRUE)
  expect_true(analysis$run())
  note = analysis$results$comparisonPlotNote
  expected = "<p>Note. Dashed line in Model 1 represents Model 2's fixed-effects predictions.</p>"
  expect_true(note$visible)
  expect_identical(note$content,expected)
  plot = analysis$results$comparisonplot$state$displayPlot
  expect_true(attr(plot,"jamovi_cas_ghost"))
  children = attr(plot,"jamovi_cas_children")
  expect_identical(vapply(children,function(p)p$labels$title,character(1)),c("Model 1","Model 2"))
  expect_identical(vapply(children,function(p)sum(vapply(p$layers,function(layer)
    identical(layer$aes_params$linetype,"dashed"),logical(1))),integer(1)),c(1L,0L))
  items = vapply(analysis$results$items,function(item)item$name,character(1))
  expect_identical(match("comparisonPlotNote",items),match("comparisonplot",items)+1L)
  for (change in list(list(casGhost=FALSE),list(casGhost=TRUE,predictionType="population"),
      list(predictionType="cas",compare=FALSE),list(compare=TRUE,graphic=FALSE))) {
    expect_true(jamovi_reporting_update(analysis,change))
    expect_false(note$visible)
    expect_identical(note$content,"")
  }
  expect_true(jamovi_reporting_update(analysis,list(graphic=TRUE)))
  expect_true(note$visible)
  local_mocked_bindings(jamovi_mixed_apply_labels=function(...)stop("render note probe"))
  expect_false(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_false(note$visible)
  expect_identical(note$content,"")
})

test_that("an enabled ghost option does not advertise a singleton prediction as a dashed line", {
  skip_if_not_installed("jmvcore")
  # A single supported prediction point is drawable, but is not a ghost line.
  local_mocked_bindings(cas_prediction_layers=function(predictions,axis,data) {
    ggplot2::geom_point(data=predictions[1,,drop=FALSE],
      mapping=ggplot2::aes(x=.data[[axis[1]]],y=.data$prediction),inherit.aes=FALSE)
  })
  analysis = jamovi_mixed_analysis(graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x","z"),comparisonTerms=list("z"),casGhost=TRUE)
  expect_true(analysis$run())
  expect_false(is.null(analysis$results$comparisonplot$state))
  expect_false(attr(analysis$results$comparisonplot$state$displayPlot,"jamovi_cas_ghost"))
  expect_false(analysis$results$comparisonPlotNote$visible)
})

test_that("sample information occurs once immediately after the visible model specification", {
  skip_if_not_installed("jmvcore")
  data = jamovi_mixed_test_data()
  data$z[c(1,3)] = NA
  for (comparison in c(FALSE,TRUE)) for (correlations in c(FALSE,TRUE)) {
    analysis = jamovi_mixed_analysis(data,compare=comparison,graphic=FALSE,
      modelTerms=list("x","z"),comparisonTerms=list("x"),randomSlopes="x",showCorrelations=correlations)
    expect_true(analysis$run())
    results = analysis$results
    visible = vapply(Filter(function(item)isTRUE(item$visible),results$items),function(item)item$name,character(1))
    specification = if (comparison) "comparisonSpecification" else "specification"
    expect_identical(match("explanation",visible),match(specification,visible)+1L)
    expect_lt(match("explanation",visible),match("fixed",visible))
    expect_identical(results$explanation$title,"")
    expect_identical(results$explanation$content,"<p>238 complete observations; 16 groups; 2 observations omitted.</p>")
    text = vapply(Filter(function(item)inherits(item,"Html"),results$items),function(item)item$content,character(1))
    expect_identical(sum(grepl("complete observations;",text,fixed=TRUE)),1L)
    expect_false(results$comparisonPlotNote$visible)
  }
})
