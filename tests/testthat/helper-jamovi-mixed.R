jamovi_mixed_test_data = function() {
  set.seed(925)
  data = data.frame(id=factor(rep(seq_len(16), each=15)),
    x=rep(seq(-1.5,1.5,length.out=15),16), z=rnorm(240),
    a=ordered(rep(c("low","middle","high"),80),levels=c("middle","low","high")))
  intercept = rnorm(16,sd=.8)
  slope = .25*intercept+rnorm(16,sd=.45)
  data$y = .3+.6*data$x+.2*data$z+.4*(data$a=="high")+.3*data$x*(data$a=="high")+
    .15*data$x^2+intercept[data$id]+slope[data$id]*data$x+rnorm(240,sd=.4)
  data
}

jamovi_mixed_analysis = function(data=jamovi_mixed_test_data(), ...) {
  options = do.call(flexplot:::mixedmodelOptions$new, utils::modifyList(
    list(out="y",preds=c("x","z","a"),cluster="id",graphic=FALSE),list(...),keep.null=TRUE))
  flexplot:::mixedmodelClass$new(options=options,data=data)
}

jamovi_mixed_capture = function(...) {
  capture$plot = original(...)
  capture$plot
}

jamovi_mixed_recorder = function(capture) {
  recorder = jamovi_mixed_capture
  environment(recorder) = list2env(list(capture=capture,original=flexplot:::jamovi_mixed_plot),
    parent=environment(recorder))
  recorder
}


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

