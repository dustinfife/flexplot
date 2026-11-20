# Construct real saved-option messages with jamovi's installed protocol types.
jamovi_mixed_saved_option = function(value, option_template, group_template) {
  item = option_template$clone(); item$clear()
  if (is.null(value)) { item$o = 2L; return(item) }
  if (is.logical(value) && length(value)==1L) { item$o=as.integer(value); return(item) }
  if (is.character(value) && length(value)==1L) { item$s=value; return(item) }
  if (is.numeric(value) && length(value)==1L) { item$d=value; return(item) }
  group = group_template$clone();group$clear()
  group$hasNames = !is.null(names(value))
  if (!is.null(names(value))) group$names=names(value)
  values=list()
  for (entry in value) values[[length(values)+1L]]=jamovi_mixed_saved_option(entry,option_template,group_template)
  group$options=values;item$c=group
  item
}
