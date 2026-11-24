# Versioned presentation migration, following the adapters' existing revision options.
# A native protobuf identifies restoration; plain R constructors are new analyses.
jamovi_output_migrate = function(options, module) {
  revision = options$outputOptionsVersion
  target = if (module %in% c("glmgeneralized","mixedmodel")) 2L else 1L
  if (revision >= target) return(invisible(NULL))
  pb = options$asProtoBuf()
  if (!is.null(pb)) {
    if (revision < 1L && module %in% c("glmgeneralized", "mixedmodel"))
      jamovi_output_option(options, "comparisonGraphic", options$graphic)
    if (module=="glmgeneralized" && revision < 2L) {
      for (pair in list(c("comparisonSe", "se"), c("comparisonPlotStyle", "plotStyle"),
          c("comparisonPlotScale", "plotScale")))
        jamovi_output_option(options, pair[1], options[[pair[2]]])
    }
  }
  if (module=="mixedmodel" && revision < 2L && !is.null(pb)) {
    for (pair in list(c("comparisonX","plotX"),c("comparisonPanels","plotPanels"),
        c("comparisonGroup","plotGroup"),c("comparisonPredictionType","predictionType"),
        c("comparisonClusters","clusters"),c("comparisonPlotLabels","plotLabels"),
        c("comparisonPlotLabelStore","plotLabelStore")))
      jamovi_output_copy(options,pair[1],pair[2])
  }
  jamovi_output_option(options, "outputOptionsVersion", target)
  invisible(NULL)
}

# Update both the R option and the native response, so reopening preserves migration.
jamovi_output_option = function(options, name, value) {
  option = options$option(name)
  option$value = value
  pb = options$asProtoBuf()
  if (is.null(pb) || !length(pb$options)) return(invisible(NULL))
  values = pb$options
  index = match(name, pb$names)
  if (is.na(index)) {
    index = length(values)+1L
    pb$names = c(pb$names, name)
  }
  item = values[[1]]$clone(); item$clear()
  if (is.logical(value)) item$o = as.integer(value) else
    if (is.character(value)) item$s = value else item$d = as.numeric(value)
  values[[index]] = item
  pb$options = values
  invisible(NULL)
}

jamovi_output_visible = function(results, names, visible) {
  for (name in names) results[[name]]$setVisible(isTRUE(visible))
  invisible(NULL)
}

# Copy the existing native encoding, including nulls, variable arrays and label rows.
jamovi_output_copy = function(options, destination, source) {
  option = options$option(destination)
  option$value = options[[source]]
  pb = options$asProtoBuf()
  if (is.null(pb)) return(invisible(NULL))
  from = match(source,pb$names)
  if (is.na(from)) return(invisible(NULL))
  values = pb$options
  to = match(destination,pb$names)
  if (is.na(to)) { to = length(values)+1L; pb$names = c(pb$names,destination) }
  values[[to]] = values[[from]]$clone()
  pb$options = values
  invisible(NULL)
}
