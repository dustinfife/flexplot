# Versioned presentation migration, following the adapters' existing revision options.
# A native protobuf identifies restoration; plain R constructors are new analyses.
jamovi_output_migrate = function(options, module) {
  if (options$outputOptionsVersion >= 1L) return(invisible(NULL))
  pb = options$asProtoBuf()
  if (!is.null(pb) && module %in% c("glmgeneralized", "mixedmodel"))
    jamovi_output_option(options, "comparisonGraphic", options$graphic)
  jamovi_output_option(options, "outputOptionsVersion", 1L)
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
  if (is.logical(value)) item$o = as.integer(value) else item$d = as.numeric(value)
  values[[index]] = item
  pb$options = values
  invisible(NULL)
}

jamovi_output_visible = function(results, names, visible) {
  for (name in names) results[[name]]$setVisible(isTRUE(visible))
  invisible(NULL)
}
