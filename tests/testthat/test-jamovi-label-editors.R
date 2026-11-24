editor_text = function(grob) {
  text = if (is.character(grob$label)) grob$label else character()
  for (child in c(grob$grobs,as.list(grob$children))) text = c(text,editor_text(child))
  text
}
editor_edit = function(rows,role,label,variable="model",value=NULL) {
  index = which(vapply(rows,function(row)row$role==role && row$variable==variable &&
    (is.null(value) || row$value==value),logical(1)))[1]
  stopifnot(!is.na(index));rows[[index]]$label=label;rows
}

test_that("legend descriptions use actual displayed labels, stable keys and committed custom text", {
  skip_if_not_installed("jmvcore")
  data = data.frame(x=rep(1:4,2),y=1:8,model=rep(c("model1 (lm)","model2 (lm)"),each=4))
  plot = ggplot2::ggplot(data,ggplot2::aes(x,y,colour=model)) + ggplot2::geom_point() +
    ggplot2::scale_colour_manual(name="Model",values=c("red","blue"),labels=c("Model 1","Model 2"))
  options = flexplot:::glmgeneralizedOptions$new()
  context = list(output="comparison",outcome="y",meaning="response")
  sync = function(plot) flexplot:::jamovi_plot_sync_labels(options,plot,list(x="x"),context)
  sync(plot);rows=options$plotLabels
  levels = Filter(function(row)row$role=="legendLevel",rows)
  expect_identical(vapply(levels,`[[`,character(1),"display"),c("Legend entry: Model 1","Legend entry: Model 2"))
  expect_identical(vapply(levels,`[[`,character(1),"value"),c("model1 (lm)","model2 (lm)"))
  keys = vapply(rows,`[[`,character(1),"key")
  edited = editor_edit(editor_edit(rows,"legend","Candidates"),"legendLevel","Preferred",value="model1 (lm)")
  option = options$option("plotLabels");option$value=edited
  sync(plot)
  expect_identical(vapply(options$plotLabels,`[[`,character(1),"key"),keys)
  expect_true("Legend title: Candidates" %in% vapply(options$plotLabels,`[[`,character(1),"display"))
  expect_true("Legend entry: Candidates = Preferred" %in% vapply(options$plotLabels,`[[`,character(1),"display"))
  labeled = flexplot:::jamovi_plot_apply_labels(plot,options$plotLabels,context,list(x="x"))
  expect_true(all(c("Candidates","Preferred","Model 2") %in% editor_text(ggplot2::ggplotGrob(labeled))))
  expect_equal(ggplot2::ggplot_build(labeled)$data,ggplot2::ggplot_build(plot)$data)
  # A temporarily absent guide retains edits, and returning restores the same keys.
  sync(plot+ggplot2::theme(legend.position="none"))
  expect_false(any(vapply(options$plotLabels,function(row)row$role=="legendLevel",logical(1))))
  sync(plot);expect_true(any(vapply(options$plotLabels,function(row)row$label=="Preferred",logical(1))))
  option = options$option("plotLabels");option$value=editor_edit(editor_edit(options$plotLabels,"legend",""),"legendLevel","",value="model1 (lm)")
  sync(plot)
  expect_identical(vapply(Filter(function(row)row$role=="legendLevel",options$plotLabels),`[[`,character(1),"display"),
    c("Legend entry: Model 1","Legend entry: Model 2"))
})

test_that("independent guides with repeated levels remain distinguishable without recoding values", {
  skip_if_not_installed("jmvcore")
  data=expand.grid(x=1:3,a=factor(c("A","B")),b=factor(c("A","B")));data$y=data$x
  plot=ggplot2::ggplot(data,ggplot2::aes(x,y,colour=a,shape=b))+ggplot2::geom_point()+
    ggplot2::labs(colour="Treatment",shape="Cohort")
  options=flexplot:::glmgeneralizedOptions$new();context=list(output="model",outcome="y",meaning="response")
  flexplot:::jamovi_plot_sync_labels(options,plot,list(x="x"),context)
  descriptions=vapply(options$plotLabels,`[[`,character(1),"display")
  expect_true(all(c("Legend entry: Treatment = A","Legend entry: Cohort = A") %in% descriptions))
  rows=editor_edit(options$plotLabels,"legendLevel","Arm A",variable="a",value="A")
  labeled=flexplot:::jamovi_plot_apply_labels(plot,rows,context,list(x="x"))
  built=ggplot2::ggplot_build(labeled)$plot
  expect_true("Arm A" %in% built$scales$get_scales("colour")$get_labels())
  expect_false("Arm A" %in% built$scales$get_scales("shape")$get_labels())
})

test_that("legacy metadata discovery preserves RNG and Mixed model label keys", {
  data=data.frame(x=1:8,y=1:8,a=factor(rep(c("A","B"),4)))
  plot=ggplot2::ggplot(data,ggplot2::aes(x,y,colour=a))+ggplot2::geom_jitter()+
    ggplot2::scale_colour_discrete(labels=c("Arm A","Arm B"))
  set.seed(754);before=.Random.seed
  rows=flexplot:::jamovi_plot_label_catalog(plot,list(x="x",group="a"))
  expect_identical(.Random.seed,before)
  expect_true("Legend entry: Arm A" %in% vapply(rows,`[[`,character(1),"display"))
  row=Filter(function(row)row$role=="legendLevel" && row$value=="A",rows)[[1]]
  expect_identical(row$key,flexplot:::jamovi_plot_label_row("legendLevel","a","A",display="old wording")$key)
})
