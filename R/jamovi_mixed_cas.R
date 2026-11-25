# Two model-specific adjustments on one validated estimation population.
jamovi_mixed_cas_comparison = function(state, x, panels=NULL, group=NULL, ghost=FALSE) {
  models = list(state$model1,state$model2)
  formula = jamovi_model_layout(models,state$data,x,panels,group)
  x = flexplot_axis_given(formula)$axis[1]
  if (identical(x,state$cluster)) stop("Use a model predictor on x; place the cluster variable in panels or color / symbol.",call.=FALSE)
  reference = state$data
  frames = lapply(seq_along(models),function(i) {
    model = models[[i]]
    fitted = cas_model_frame(model)
    index = cas_external_rows(reference,fitted)
    if (nrow(reference) != nrow(fitted) || anyNA(index) || anyDuplicated(index)) {
      stop("CAS comparison requires identical fitted observations for both models.",call.=FALSE)
    }
    adjusted = compute_cas_from_model(model,adjust_re="all")
    adjusted = adjusted[index,,drop=FALSE]
    for (variable in setdiff(names(reference),names(adjusted))) adjusted[[variable]] = reference[[variable]]
    adjusted
  })
  # Choose flexplot's usual category order once from the common observed outcomes.
  if (!is.numeric(reference[[x]]) && !is.ordered(reference[[x]])) {
    template = flexplot(formula,reference,suppress_smooth=TRUE,raw.data=FALSE)
    order = levels(template$data[[x]])
    for (i in seq_along(frames)) frames[[i]][[x]] = ordered(frames[[i]][[x]],levels=order)
  }
  plots = lapply(seq_along(models),function(i) tryCatch(
    cas_render_model(formula,models[[i]],frames[[i]]),
    error=function(error) stop("Model ",i," CAS: ",conditionMessage(error),call.=FALSE)))
  for (i in seq_along(plots)) for (layer in plots[[i]]$layers) {
    if (!is.data.frame(layer$data) || !"prediction" %in% names(layer$data)) next
    if (inherits(layer$geom,"GeomLine")) {
      layer$mapping$linetype = NULL
      layer$aes_params$linetype = "solid"
    }
  }
  curves = lapply(plots,attr,which="cas_prediction")
  if (isTRUE(ghost)) {
    layers = cas_prediction_layers(curves[[2]],flexplot_axis_given(formula)$axis,plots[[1]]$data)
    if (inherits(layers,"Layer")) layers = list(layers)
    layers = Filter(cas_is_prediction_layer,layers)
    for (j in seq_along(layers)) {
      layers[[j]]$mapping$linetype = NULL
      layers[[j]]$aes_params$alpha = .55
      layers[[j]]$show.legend = FALSE
      if (inherits(layers[[j]]$geom,"GeomLine")) layers[[j]]$aes_params$linetype = "dashed"
    }
    plots[[1]] = plots[[1]] + layers
  }
  # Coordinates preserve all rows and summary calculations while sharing coverage.
  builds = lapply(plots,ggplot2::ggplot_build)
  y = unlist(lapply(builds,function(build) unlist(lapply(build$data,function(data) {
    unlist(data[intersect(c("y","ymin","ymax","lower","upper"),names(data))])
  }))))
  outcome = paste0(all.vars(formula)[1],"_adj")
  limits = range(c(y,unlist(lapply(frames,`[[`,outcome))),finite=TRUE)
  for (i in seq_along(plots)) {
    plots[[i]]$coordinates = ggplot2::coord_cartesian(
      xlim=if (is.numeric(plots[[i]]$data[[x]])) range(reference[[x]],finite=TRUE) else NULL,ylim=limits)
    plots[[i]] = plots[[i]] + ggplot2::labs(title=paste("Model",i),subtitle=NULL,caption=NULL)
  }
  result = jamovi_mixed_cas_compose(plots)
  # Use built coordinates so an empty or singleton path cannot advertise a line.
  attr(result,"jamovi_cas_ghost") = isTRUE(ghost) && any(vapply(seq_along(plots[[1]]$layers),function(i) {
    layer = plots[[1]]$layers[[i]]
    if (!inherits(layer$geom,"GeomLine") || !identical(layer$aes_params$linetype,"dashed")) return(FALSE)
    data = builds[[1]]$data[[i]]
    data = data[is.finite(data$x) & is.finite(data$y),,drop=FALSE]
    any(table(interaction(data$PANEL,data$group,drop=TRUE)) >= 2L)
  },logical(1)))
  result
}

jamovi_mixed_cas_compose = function(plots) {
  result = patchwork::wrap_plots(plots,nrow=1,guides="collect")
  attr(result,"jamovi_cas_children") = plots
  result
}
