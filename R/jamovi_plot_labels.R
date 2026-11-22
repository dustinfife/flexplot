# Shared display metadata; never recode model variables or prediction coordinates.
jamovi_plot_label_row = function(role, variable, value="", display, context=NULL) {
  key = paste0(nchar(role), ":", role, nchar(variable), ":", variable, nchar(value), ":", value)
  if (!is.null(context)) key = paste0(jamovi_plot_label_prefix(context),key)
  list(key=key, role=role, variable=variable, value=value, display=display, label="")
}

jamovi_plot_label_catalog = function(plot, layout, context=NULL) {
  if (!is.null(context)) return(jamovi_plot_scoped_catalog(plot,layout,context))
  x = layout$x
  if (is.null(x)) x = jamovi_plot_axis_key(plot,"x")
  rows = list(jamovi_plot_label_row("x", paste(x,collapse=""), display="X-axis label"),
    jamovi_plot_label_row("y", jamovi_plot_axis_key(plot,"y"), display="Y-axis label"))
  group = layout$group
  if (isTRUE(layout$comparison)) {
    rows = c(rows,list(jamovi_plot_label_row("legend","model",display="Model legend title")))
    models = unique(unlist(lapply(plot$layers,jamovi_plot_layer_models)))
    for (i in seq_along(models)) rows = c(rows,list(jamovi_plot_label_row("legendLevel","model",models[i],paste0("Legend: Model ",i))))
  }
  if (length(group) && !identical(plot$theme$legend.position, "none")) {
    column = if (paste0(group,"_binned") %in% names(plot$data)) paste0(group,"_binned") else group
    rows = c(rows,list(jamovi_plot_label_row("legend",column,display=paste0("Color / line / symbol title: ",group))))
    values = plot$data[[column]]
    levels = if (is.factor(values)) levels(values)[levels(values) %in% as.character(values)] else unique(as.character(values[!is.na(values)]))
    for (level in levels) rows = c(rows,list(jamovi_plot_label_row("legendLevel",column,level,
      paste0("Legend: ",group," = ",level))))
  }
  for (direction in c("rows","cols")) {
    for (variable in names(plot$facet$params[[direction]])) {
      title = sub("_binned$","",variable)
      role = if (direction == "rows") "rowTitle" else "columnTitle"
      rows = c(rows,list(jamovi_plot_label_row(role,variable,display=paste0(
        if (direction == "rows") "Row-panel title: " else "Column-panel title: ",title))))
      values = plot$data[[variable]]
      levels = if (is.factor(values)) levels(values)[levels(values) %in% as.character(values)] else unique(as.character(values[!is.na(values)]))
      for (level in levels) rows = c(rows,list(jamovi_plot_label_row("facetLevel",variable,level,
        paste0("Panel: ",title," = ",level))))
    }
  }
  rows
}

jamovi_plot_remember_labels = function(store, current) {
  remembered = list()
  for (entry in c(store,current)) {
    if (!is.list(entry) || !is.character(entry$key) || length(entry$key) != 1L) next
    remembered[[entry$key]] = entry
  }
  unname(remembered)
}

jamovi_plot_sync_labels = function(options, plot=NULL, layout=NULL, context=NULL,
                                    rows="plotLabels", retained="plotLabelStore") {
  store = jamovi_plot_remember_labels(options[[retained]],options[[rows]])
  catalog = if (is.null(plot)) list() else if (is.null(context)) jamovi_plot_label_catalog(plot,layout) else
    tryCatch(jamovi_plot_label_catalog(plot,layout,context),error=function(error) list())
  keys = vapply(store,`[[`,character(1),"key")
  for (i in seq_along(catalog)) {
    index = match(catalog[[i]]$key,keys)
    if (!is.na(index)) catalog[[i]]$label = store[[index]]$label
  }
  jamovi_plot_publish_labels(options,retained,jamovi_plot_remember_labels(store,catalog))
  jamovi_plot_publish_labels(options,rows,catalog)
  invisible(NULL)
}

# The labeller receives only display values; predictions and facet membership stay intact.
jamovi_plot_strip_labels = function(labels) {
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

jamovi_plot_apply_labels = function(plot, entries, context=NULL, layout=NULL) {
  if (!is.null(context)) return(jamovi_plot_apply_scoped_labels(plot,entries,layout,context))
  entries = Filter(jamovi_plot_nonblank_label,entries)
  if (!length(entries)) return(plot)
  plot$scales = plot$scales$clone()
  aesthetics = unique(c(names(plot$mapping),unlist(lapply(plot$layers,jamovi_plot_layer_aesthetics))))
  titles = list()
  for (entry in entries) {
    if (entry$role %in% c("x","y")) titles[[entry$role]] = entry$label
    if (entry$role == "legend") {
      for (aesthetic in intersect(c("colour","fill","shape","linetype"),aesthetics)) titles[[aesthetic]] = entry$label
    }
  }
  if (length(titles)) plot = plot + do.call(ggplot2::labs,titles)
  levels = Filter(jamovi_plot_legend_label,entries)
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
      index = which(vapply(plot$scales$scales,jamovi_plot_scale_matches,logical(1),aesthetic=aesthetic))
      if (length(index)) plot$scales$scales[[index[1]]] = scale else plot$scales$add(scale)
    }
  }
  if (any(vapply(entries,jamovi_plot_facet_label,logical(1)))) {
    labeller = jamovi_plot_strip_labels
    environment(labeller) = list2env(list(mapping=entries),parent=environment(labeller))
    facet = plot$facet
    plot$facet = ggplot2::ggproto(NULL,facet)
    plot$facet$params$labeller = labeller
  }
  plot
}

jamovi_plot_nonblank_label = function(entry) is.list(entry) && is.character(entry$label) &&
  length(entry$label) == 1L && !is.na(entry$label) && nzchar(trimws(entry$label))
jamovi_plot_legend_label = function(entry) identical(entry$role,"legendLevel")
jamovi_plot_facet_label = function(entry) entry$role %in% c("rowTitle","columnTitle","facetLevel")
jamovi_plot_scale_matches = function(scale, aesthetic) aesthetic %in% scale$aesthetics

# jamovi sends the original options protobuf back to the UI. Changing an R
# OptionArray alone does not update that response. Clone the existing protocol
# messages to publish these string-only editor rows through the same response.
jamovi_plot_publish_labels = function(options, name, rows) {
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

jamovi_plot_layer_aesthetics = function(layer) names(layer$mapping)

jamovi_plot_axis_key = function(plot, axis) {
  mapping = plot$mapping[[axis]]
  if (is.null(mapping)) return("")
  rlang::as_label(mapping)
}

jamovi_plot_layer_models = function(layer) {
  if (!is.data.frame(layer$data) || !"model" %in% names(layer$data)) return(character())
  as.character(layer$data$model)
}

# Length-prefixed fields keep literal names and unusual text unambiguous.
jamovi_plot_label_prefix = function(context) {
  values = unlist(context[c("output","outcome","meaning")],use.names=FALSE)
  paste0("plot:",paste0(nchar(values),":",values,collapse=""),":")
}

jamovi_plot_label_context = function(model, formula, output="model", meaning="response") {
  roles = flexplot_axis_given(formula)
  list(context=list(output=output,outcome=all.vars(stats::formula(model))[1],meaning=meaning),
    layout=list(x=roles$axis[1],group=roles$axis[-1]))
}

jamovi_plot_label_variable = function(mapping) {
  if (is.null(mapping)) return("")
  expression = rlang::get_expr(mapping)
  if (is.symbol(expression)) return(as.character(expression))
  if (is.call(expression) && length(expression)==3L &&
      as.character(expression[[1]]) %in% c("$","[[") && identical(expression[[2]],as.name(".data"))) {
    variable = expression[[3]]
    if (is.symbol(variable) || is.character(variable)) return(as.character(variable))
  }
  rlang::as_label(mapping)
}

# Discover guides by their mapped variable/expression, not by matching level text.
# A colour guide and an unrelated shape guide may contain identical level names.
jamovi_plot_label_guides = function(plot, built) {
  if (identical(plot$theme$legend.position,"none")) return(list())
  guides = list()
  for (aesthetic in c("colour","fill","shape","linetype")) {
    expressions = character()
    for (layer in plot$layers) {
      if (identical(layer$show.legend,FALSE) || !is.null(layer$aes_params[[aesthetic]])) next
      mapping = layer$mapping[[aesthetic]]
      if (is.null(mapping) && isTRUE(layer$inherit.aes)) mapping = plot$mapping[[aesthetic]]
      if (!is.null(mapping)) expressions = union(expressions,jamovi_plot_label_variable(mapping))
    }
    scale = built$plot$scales$get_scales(aesthetic)
    if (length(expressions)!=1L || is.null(scale) || identical(scale$guide,"none")) next
    id = expressions[1]
    if (is.null(guides[[id]])) {
      title = scale$name
      if (!is.character(title) || length(title)!=1L) title = id
      guides[[id]] = list(variable=id,aesthetics=character(),title=title,
        values=if (scale$is_discrete()) as.character(scale$get_breaks()) else character())
    }
    guides[[id]]$aesthetics = union(guides[[id]]$aesthetics,aesthetic)
  }
  unname(guides)
}

jamovi_plot_scoped_catalog = function(plot, layout, context) {
  row = function(role,variable,value="",display)
    jamovi_plot_label_row(role,variable,value,display,context)
  rows = list(row("title","plot",display="Plot title"),row("subtitle","plot",display="Plot subtitle"),
    row("x",if (length(layout$x)) layout$x else jamovi_plot_label_variable(plot$mapping$x),display="X-axis label"),
    row("y",context$outcome,display="Y-axis label"))
  built = suppressWarnings(ggplot2::ggplot_build(plot))
  for (guide in jamovi_plot_label_guides(plot,built)) {
    rows = c(rows,list(row("legend",guide$variable,display=paste0("Legend title: ",guide$title))))
    for (value in guide$values[!is.na(guide$values)]) rows = c(rows,list(
      row("legendLevel",guide$variable,value,paste0("Legend: ",guide$title," = ",value))))
  }
  for (direction in c("rows","cols")) for (variable in names(plot$facet$params[[direction]])) {
    title = sub("_binned$","",variable)
    role = if (direction=="rows") "rowTitle" else "columnTitle"
    rows = c(rows,list(row(role,variable,display=paste0(if (direction=="rows") "Row-panel title: " else "Column-panel title: ",title))))
    values = built$layout$layout[[variable]]
    values = if (is.factor(values)) levels(droplevels(values)) else unique(as.character(values))
    for (value in values[!is.na(values)]) rows = c(rows,list(row("facetLevel",variable,value,paste0("Panel: ",title," = ",value))))
  }
  rows
}

jamovi_plot_apply_scoped_labels = function(plot, entries, layout, context) {
  entries = Filter(jamovi_plot_nonblank_label,entries)
  if (!length(entries)) return(plot)
  catalog = jamovi_plot_scoped_catalog(plot,layout,context)
  keys = vapply(catalog,`[[`,character(1),"key")
  entries = Filter(function(entry)entry$key %in% keys,entries)
  if (!length(entries)) return(plot)
  titles = list()
  for (entry in entries) if (entry$role %in% c("title","subtitle","x","y")) titles[[entry$role]] = entry$label
  if (length(titles)) plot = plot + do.call(ggplot2::labs,titles)
  plot$scales = plot$scales$clone()
  built = ggplot2::ggplot_build(plot)
  # Explicit scale names (notably the logit axis) take precedence over labs().
  for (axis in intersect(c("x","y"),names(titles))) {
    scale = plot$scales$get_scales(axis)
    if (is.null(scale)) next
    scale = scale$clone()
    scale$name = titles[[axis]]
    index = which(vapply(plot$scales$scales,jamovi_plot_scale_matches,logical(1),aesthetic=axis))
    plot$scales$scales[[index[1]]] = scale
  }
  for (guide in jamovi_plot_label_guides(plot,built)) {
    selected = Filter(function(entry)entry$variable==guide$variable && entry$role %in% c("legend","legendLevel"),entries)
    if (!length(selected)) next
    for (aesthetic in guide$aesthetics) {
      original = built$plot$scales$get_scales(aesthetic)
      breaks = original$get_breaks()
      labels = original$get_labels(breaks)
      scale = original$clone()
      for (entry in selected) if (entry$role=="legend") scale$name = entry$label
      if (scale$is_discrete()) {
        for (entry in selected) if (entry$role=="legendLevel") labels[as.character(breaks)==entry$value] = entry$label
        scale$labels = stats::setNames(labels,breaks)
      }
      index = which(vapply(plot$scales$scales,jamovi_plot_scale_matches,logical(1),aesthetic=aesthetic))
      if (length(index)) plot$scales$scales[[index[1]]] = scale else plot$scales$add(scale)
    }
  }
  if (any(vapply(entries,jamovi_plot_facet_label,logical(1)))) {
    labeller = jamovi_plot_strip_labels
    environment(labeller) = list2env(list(mapping=entries),parent=environment(labeller))
    facet = plot$facet
    plot$facet = ggplot2::ggproto(NULL,facet)
    plot$facet$params$labeller = labeller
  }
  plot
}
