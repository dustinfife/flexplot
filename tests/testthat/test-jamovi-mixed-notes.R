test_that("CAS ghost notes follow actual directional lines and clear on option changes", {
  skip_if_not_installed("jmvcore")
  analysis = jamovi_mixed_analysis(graphic=TRUE,compare=TRUE,predictionType="cas",plotX="x",
    modelTerms=list("x","z"),comparisonTerms=list("z"),casGhost=TRUE)
  expect_true(analysis$run())
  note = analysis$results$comparisonPlotNote
  expected = flexplot:::jamovi_plot_note_paragraph("Note. Dashed line in Model 1 represents Model 2's fixed-effects predictions.")
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
  for (change in list(list(casGhost=FALSE),list(casGhost=TRUE,comparisonPredictionType="population"),
      list(comparisonPredictionType="cas",compare=FALSE),list(compare=TRUE,comparisonGraphic=FALSE))) {
    expect_true(jamovi_reporting_update(analysis,change))
    expect_false(note$visible)
    expect_identical(note$content,"")
  }
  expect_true(jamovi_reporting_update(analysis,list(comparisonGraphic=TRUE)))
  expect_true(note$visible)
  local_mocked_bindings(jamovi_mixed_apply_labels=function(...)stop("render note probe"))
  expect_false(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_true(note$visible)
  expect_identical(note$content,"<p>Comparison plot unavailable: render note probe</p>")
  expect_false(grepl("Dashed line",note$content,fixed=TRUE))
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

test_that("sample information shares the model specification without a separate output block", {
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
    expect_lt(match(specification,visible),match("fixed",visible))
    expect_false(results$explanation$visible)
    expect_identical(results$explanation$content,"")
    expect_match(results[[specification]]$content,
      "</p><p>238 complete observations; 16 groups; 2 observations omitted.</p>",fixed=TRUE)
    text = vapply(Filter(function(item)inherits(item,"Html"),results$items),function(item)item$content,character(1))
    expect_identical(sum(grepl("complete observations;",text,fixed=TRUE)),1L)
    expect_false(results$comparisonPlotNote$visible)
  }
})

test_that("CAS layout changes preserve the original cluster data and fitted random effects", {
  skip_if_not_installed("jmvcore")
  for (comparison in c(FALSE,TRUE)) {
    analysis = jamovi_mixed_analysis(graphic=TRUE,compare=comparison,predictionType="population",plotX="x",
      modelTerms=list("x","z"),randomSlopes="x",comparisonTerms=list("x"))
    expect_true(analysis$run())
    original = if (comparison) analysis$results$comparisonplot$state else analysis$results$plot$state
    local({
      local_mocked_bindings(jamovi_mixed_fit=function(...)stop("Unexpected model refit"),
        jamovi_mixed_comparison=function(...)stop("Unexpected comparison refit"))
      change = if(comparison) list(comparisonPredictionType="cas",comparisonPanels="z") else list(predictionType="cas",plotPanels="z")
      expect_true(jamovi_reporting_update(analysis,change))
      current = if (comparison) analysis$results$comparisonplot$state else analysis$results$plot$state
      expect_identical(analysis$options$cluster,"id")
      expect_true("id" %in% names(current$data))
      if (comparison) {
        expect_identical(current$model1,original$model1)
        expect_identical(current$model2,original$model2)
      } else expect_identical(current$model,original$model)
      expect_identical(analysis$options[[if(comparison) "comparisonPanels" else "plotPanels"]],"z")
      expect_true(jamovi_reporting_update(analysis,if(comparison) list(comparisonPredictionType="cluster",comparisonPanels="id") else list(predictionType="cluster",plotPanels="id")))
    })
  }
})
