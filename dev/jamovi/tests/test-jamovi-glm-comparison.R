test_that("installed native renderers preserve named ordering and annotation addresses", {
  jmvcore:::initProtoBuf()
  records = list()
  for (compare in c(FALSE, TRUE)) {
    analyses = list(
      glmbasic=lm_quadratic_analysis(preds=c("x", "a"), compare=compare, graphic=FALSE,
        modelTerms=list("x", "a"), comparisonTerms=list("x")),
      glmgeneralized=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",
        preds=c("x", "a"), family="binomial", graphic=FALSE, compare=compare,
        comparisonX="x", modelTerms=list("x", "a"), comparisonTerms=list("x")), small),
      mixedmodel=jamovi_mixed_comparison_analysis(compare=compare))
    for (name in names(analyses)) {
      analysis = analyses[[name]]
      expect_true(analysis$run())
      # Restore real saved options, and pre-stage cached results with old ordering
      # and no newly introduced LM/GLM specification element.
      saved = analysis$options$asProtoBuf()
      # Direct R6 options have no request protobuf; serialize via native encoder.
      if (is.null(saved)) {
        saved = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions", asNamespace("jmvcore")))
        template = jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption", asNamespace("jmvcore")))
        values = lapply(analysis$options$names, analysis$options$get)
        names(values) = analysis$options$names
        saved$names = names(values)
        saved$options = lapply(values, jamovi_mixed_saved_option, option_template=template, group_template=saved)
      }
      options = get(paste0(name, "Options"), asNamespace("flexplot"))$new()
      options$fromProtoBuf(saved)
      reopened = get(paste0(name, "Class"), asNamespace("flexplot"))$new(options=options, data=NULL)
      reader = jamovi_reporting_read
      environment(reader) = list2env(list(data=if (name=="mixedmodel") jamovi_mixed_test_data() else small), parent=environment(reader))
      reopened$.setReadDatasetSource(reader)
      reopened$.setReadDatasetHeaderSource(reader)
      old = analysis$results$asProtoBuf(status=4L, includeState=FALSE)
      old$group$elements = Filter(function(item) name=="mixedmodel" || item$name!="specification", rev(old$group$elements))
      objects = reopened$results$items
      reopened$results$fromProtoBuf(old, character(), character())
      expect_identical(reopened$results$items, objects)
      expect_identical(reopened$results$itemNames, analysis$results$itemNames)
      expect_true(reopened$run())
      for (item in analysis$results$items) if (inherits(item, "Table"))
        expect_equal(reopened$results[[item$name]]$asDF, item$asDF)
      for (graphic in c(FALSE, TRUE)) {
        expect_true(jamovi_reporting_update(reopened, list(graphic=graphic)))
        # Include failed layouts: a missing plot must not reorder valid tables.
        pb = reopened$results$asProtoBuf(status=4L, includeState=FALSE)
        records[[length(records)+1L]] = list(name=name, compare=compare,
          elements=lapply(pb$group$elements, function(item) list(name=item$name,
            title=item$title, type=if (inherits(reopened$results[[item$name]], "Html")) "html" else if (inherits(reopened$results[[item$name]], "Table")) "table" else "image", visible=as.integer(item$visible),
            content=if (inherits(reopened$results[[item$name]], "Html")) reopened$results[[item$name]]$content else "")))
      }
    }
  }
  file = withr::local_tempfile(fileext=".json")
  jsonlite::write_json(records, file, auto_unbox=TRUE, null="null")
  assets = "/Applications/jamovi.app/Contents/Resources/jamovi/client/dist/assets"
  log = system2(Sys.which("node"), shQuote(c(test_path("js", "jamovi-note-layout.cjs"), assets, file)), stdout=TRUE, stderr=TRUE)
  expect_null(attr(log, "status"), info=paste(log, collapse="\n"))
  expect_match(paste(log, collapse="\n"), "native ordering, specifications and annotation addresses", fixed=TRUE)
})
