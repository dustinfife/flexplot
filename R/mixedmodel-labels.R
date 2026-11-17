# Presentation metadata for the jamovi Mixed Model; never recode model variables.
jamovi_mixed_label_row = function(role, variable, value="", display) {
  key = paste0(nchar(role), ":", role, nchar(variable), ":", variable, nchar(value), ":", value)
  list(key=key, role=role, variable=variable, value=value, display=display, label="")
}

jamovi_mixed_label_catalog = function(plot, layout) {
  x = layout$x
  if (is.null(x)) x = jamovi_mixed_axis_key(plot,"x")
  rows = list(jamovi_mixed_label_row("x", paste(x,collapse=""), display="X-axis label"),
    jamovi_mixed_label_row("y", jamovi_mixed_axis_key(plot,"y"), display="Y-axis label"))
  group = layout$group
  if (isTRUE(attr(plot,"jamovi_mixed_comparison"))) {
    rows = c(rows,list(jamovi_mixed_label_row("legend","model",display="Model legend title")))
    models = unique(unlist(lapply(plot$layers,jamovi_mixed_layer_models)))
    for (i in seq_along(models)) rows = c(rows,list(jamovi_mixed_label_row("legendLevel","model",models[i],paste0("Legend: Model ",i))))
  }
  if (length(group) && !identical(plot$theme$legend.position, "none")) {
    column = if (paste0(group,"_binned") %in% names(plot$data)) paste0(group,"_binned") else group
    rows = c(rows,list(jamovi_mixed_label_row("legend",column,display=paste0("Color / line / symbol title: ",group))))
    values = plot$data[[column]]
    levels = if (is.factor(values)) levels(values)[levels(values) %in% as.character(values)] else unique(as.character(values[!is.na(values)]))
    for (level in levels) rows = c(rows,list(jamovi_mixed_label_row("legendLevel",column,level,
      paste0("Legend: ",group," = ",level))))
  }
  for (direction in c("rows","cols")) {
    for (variable in names(plot$facet$params[[direction]])) {
      title = sub("_binned$","",variable)
      role = if (direction == "rows") "rowTitle" else "columnTitle"
      rows = c(rows,list(jamovi_mixed_label_row(role,variable,display=paste0(
        if (direction == "rows") "Row-panel title: " else "Column-panel title: ",title))))
      values = plot$data[[variable]]
      levels = if (is.factor(values)) levels(values)[levels(values) %in% as.character(values)] else unique(as.character(values[!is.na(values)]))
      for (level in levels) rows = c(rows,list(jamovi_mixed_label_row("facetLevel",variable,level,
        paste0("Panel: ",title," = ",level))))
    }
  }
  rows
}

jamovi_mixed_remember_labels = function(store, current) {
  remembered = list()
  for (entry in c(store,current)) {
    if (!is.list(entry) || !is.character(entry$key) || length(entry$key) != 1L) next
    remembered[[entry$key]] = entry
  }
  unname(remembered)
}

jamovi_mixed_sync_labels = function(options, plot=NULL, layout=NULL) {
  store = jamovi_mixed_remember_labels(options$plotLabelStore,options$plotLabels)
  catalog = if (is.null(plot)) list() else jamovi_mixed_label_catalog(plot,layout)
  keys = vapply(store,`[[`,character(1),"key")
  for (i in seq_along(catalog)) {
    index = match(catalog[[i]]$key,keys)
    if (!is.na(index)) catalog[[i]]$label = store[[index]]$label
  }
  jamovi_mixed_publish_labels(options,"plotLabelStore",jamovi_mixed_remember_labels(store,catalog))
  jamovi_mixed_publish_labels(options,"plotLabels",catalog)
  invisible(NULL)
}

# The labeller receives only display values; predictions and facet membership stay intact.
jamovi_mixed_strip_labels = function(labels) {
  mapping = get("mapping",envir=environment(sys.function()),inherits=FALSE)
  result = custom.labeler(labels)
  for (i in seq_along(labels)) {
    variable = names(labels)[i]
    entries = mapping[vapply(mapping,`[[`,character(1),"variable") == variable]
    title = sub("_binned$","",variable)
    values = as.character(labels[[i]])
    for (entry in entries) {
      if (!nzchar(trimws(entry$label))) next
      if (entry$role %in% c("rowTitle","columnTitle")) title = entry$label
      if (entry$role == "facetLevel") values[values == entry$value] = entry$label
    }
    result[[i]] = paste0(title,":\n",values)
  }
  result
}

jamovi_mixed_apply_labels = function(plot, entries) {
  entries = Filter(jamovi_mixed_nonblank_label,entries)
  if (!length(entries)) return(plot)
  plot$scales = plot$scales$clone()
  aesthetics = unique(c(names(plot$mapping),unlist(lapply(plot$layers,jamovi_mixed_layer_aesthetics))))
  titles = list()
  for (entry in entries) {
    if (entry$role %in% c("x","y")) titles[[entry$role]] = entry$label
    if (entry$role == "legend") {
      for (aesthetic in intersect(c("colour","fill","shape","linetype"),aesthetics)) titles[[aesthetic]] = entry$label
    }
  }
  if (length(titles)) plot = plot + do.call(ggplot2::labs,titles)
  levels = Filter(jamovi_mixed_legend_label,entries)
  if (length(levels)) {
    # Use the existing scale types/palettes, including ordered and manual scales.
    built = ggplot2::ggplot_build(plot)$plot
    for (aesthetic in c("colour","fill","shape","linetype")) {
      scale = built$scales$get_scales(aesthetic)
      if (is.null(scale) || !scale$is_discrete()) next
      scale = scale$clone()
      breaks = scale$get_breaks()
      labels = scale$get_labels(breaks)
      for (entry in levels) labels[as.character(breaks) == entry$value] = entry$label
      scale$labels = stats::setNames(labels,breaks)
      # Replacing the scale object directly avoids redundant-scale messages.
      index = which(vapply(plot$scales$scales,jamovi_mixed_scale_matches,logical(1),aesthetic=aesthetic))
      if (length(index)) plot$scales$scales[[index[1]]] = scale else plot$scales$add(scale)
    }
  }
  if (any(vapply(entries,jamovi_mixed_facet_label,logical(1)))) {
    labeller = jamovi_mixed_strip_labels
    environment(labeller) = list2env(list(mapping=entries),parent=environment(labeller))
    facet = plot$facet
    plot$facet = ggplot2::ggproto(NULL,facet)
    plot$facet$params$labeller = labeller
  }
  plot
}

jamovi_mixed_nonblank_label = function(entry) is.list(entry) && is.character(entry$label) &&
  length(entry$label) == 1L && !is.na(entry$label) && nzchar(trimws(entry$label))
jamovi_mixed_legend_label = function(entry) identical(entry$role,"legendLevel")
jamovi_mixed_facet_label = function(entry) entry$role %in% c("rowTitle","columnTitle","facetLevel")
jamovi_mixed_scale_matches = function(scale, aesthetic) aesthetic %in% scale$aesthetics

# jamovi sends the original options protobuf back to the UI. Changing an R
# OptionArray alone does not update that response. Clone the existing protocol
# messages to publish these string-only editor rows through the same response.
jamovi_mixed_publish_labels = function(options, name, rows) {
  option = options$option(name)
  option$value = rows
  pb = options$asProtoBuf()
  if (is.null(pb)) return(invisible(NULL)) # Direct R6 calls have no host request.
  index = match(name,pb$names)
  values = pb$options
  if (!length(values)) return(invisible(NULL))
  if (is.na(index)) {
    index = length(values)+1L
    pb$names = c(pb$names,name)
  }
  template = values[[1]]
  array = pb$clone(); array$clear(); array$hasNames = FALSE
  groups = list()
  for (row in rows) {
    group = pb$clone(); group$clear(); group$hasNames = TRUE; group$names = names(row)
    strings = list()
    for (value in row) {
      item = template$clone(); item$clear(); item$s = as.character(value)
      strings[[length(strings)+1L]] = item
    }
    group$options = strings
    item = template$clone(); item$clear(); item$c = group
    groups[[length(groups)+1L]] = item
  }
  array$options = groups
  item = template$clone(); item$clear(); item$c = array
  values[[index]] = item
  pb$options = values
  invisible(NULL)
}

jamovi_mixed_layer_aesthetics = function(layer) names(layer$mapping)

jamovi_mixed_axis_key = function(plot, axis) {
  mapping = plot$mapping[[axis]]
  if (is.null(mapping)) return("")
  rlang::as_label(mapping)
}

jamovi_mixed_layer_models = function(layer) {
  if (!is.data.frame(layer$data) || !"model" %in% names(layer$data)) return(character())
  as.character(layer$data$model)
}
