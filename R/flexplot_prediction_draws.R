# Draw identity is metadata, never a model label or aesthetic scale.
flexplot_validate_prediction_draws = function(prediction, draw, alpha, formula) {
  if (!inherits(formula,"formula") || length(formula)!=3L || !is.symbol(formula[[2]]) ||
      !compare_fits_plain_expression(formula))
    stop("Prediction draws require an untransformed visualization formula with literal x/group/panel names.",call.=FALSE)
  if (any(all.vars(formula)[-1] %in% c("model","prediction")))
    stop("Predictor names model and prediction are reserved for prediction metadata.",call.=FALSE)
  if (!is.character(draw) || length(draw) != 1L || is.na(draw) || !nzchar(draw) ||
      draw %in% c("model","prediction",all.vars(formula))) {
    stop("prediction.draw must name a separate draw-identifier column, not a plotting variable, model, or prediction.",call.=FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("prediction.alpha must be a number between zero and one.",call.=FALSE)
  }
  if (!is.data.frame(prediction) || !all(c(draw,"prediction") %in% names(prediction)) || !nrow(prediction)) {
    stop("Supply a nonempty prediction data frame with prediction and draw-identifier columns.",call.=FALSE)
  }
  if (!is.atomic(prediction[[draw]]) || is.matrix(prediction[[draw]]) || anyNA(prediction[[draw]]) ||
      (is.numeric(prediction[[draw]]) && any(!is.finite(prediction[[draw]])))) {
    stop("Prediction draw identifiers must be nonmissing scalar values.",call.=FALSE)
  }
  if (!is.numeric(prediction$prediction) || is.matrix(prediction$prediction) || any(!is.finite(prediction$prediction))) {
    stop("Prediction draws require one finite numeric prediction per row.",call.=FALSE)
  }
  if (!"model" %in% names(prediction)) prediction$model = "Prediction"
  if (!is.atomic(prediction$model) || is.matrix(prediction$model) || anyNA(prediction$model) || length(unique(prediction$model)) != 1L) {
    stop("Prediction draw mode currently represents one model. Use model for model identity and prediction.draw for draw identity.",call.=FALSE)
  }
  if (length(all.vars(formula)) < 2L) stop("Prediction draws require an x predictor, not a distribution plot.",call.=FALSE)
  prediction
}

flexplot_draw_prediction_layers = function(prediction, axis, data, given, draw, alpha, use_magnet=FALSE) {
  axis = flexplot_object_axis(data,axis)
  x = axis[1]
  group = if (length(axis)>1) axis[2] else NULL
  panels = flexplot_magnet_display_given(data,given)
  panels = panels[!is.na(panels) & nzchar(panels)]
  keys = unique(c("model",draw,group,panels))
  coordinates = unique(c(keys,x))
  if (!all(coordinates %in% names(prediction)) || anyNA(prediction[coordinates])) {
    stop("Prediction draw coordinates must map to the displayed x, group, and panel levels.",call.=FALSE)
  }
  for (variable in c(group,panels)) {
    if (any(!as.character(prediction[[variable]]) %in% as.character(unique(data[[variable]]))))
      stop("Prediction draw groups and panels must use observed display levels; unknown levels cannot receive native positions.",call.=FALSE)
  }
  if (anyDuplicated(prediction[coordinates])) {
    stop("Supply one prediction per draw and displayed x/group/panel coordinate. Use prepare_flexplot()$grid, or summarize upstream; draw values are never averaged by the renderer.",call.=FALSE)
  }
  if (use_magnet && any(prediction$prediction < 0 | prediction$prediction > 1)) {
    stop("Magnet prediction draws must be probabilities between zero and one.",call.=FALSE)
  }
  path = tail(make.unique(c(names(prediction),".flexplot_draw_path")),1)
  prediction[[path]] = dplyr::group_indices(dplyr::group_by(prediction,dplyr::across(dplyr::all_of(keys))))
  mapping = ggplot2::aes(x=.data[[x]],y=.data$prediction,group=.data[[path]])
  position = ggplot2::position_identity()
  if (!is.null(group)) {
    mapping$colour = ggplot2::aes(colour=.data[[group]])$colour
    if (length(unique(data[[group]]))<=6) mapping$linetype = mapping$colour
    if (!is.numeric(data[[x]])) position = flexplot_magnet_summary_position(data,group)
  }
  lines = prediction |>
    dplyr::group_by(.data[[path]]) |>
    dplyr::filter(dplyr::n_distinct(.data[[x]])>1L) |>
    dplyr::ungroup()
  layers = list(ggplot2::geom_line(data=lines,mapping=mapping,position=position,
    alpha=alpha,inherit.aes=FALSE,show.legend=FALSE))
  points = prediction
  if (is.numeric(data[[x]])) points = prediction |>
    dplyr::group_by(.data[[path]]) |>
    dplyr::filter(dplyr::n_distinct(.data[[x]])==1L) |>
    dplyr::ungroup()
  if (nrow(points)) {
    point_mapping = mapping
    point_mapping$linetype = NULL
    layers[[2]] = ggplot2::geom_point(data=points,mapping=point_mapping,
      position=position,alpha=alpha,inherit.aes=FALSE,show.legend=FALSE)
  }
  if (is.null(group)) for (layer in layers) layer$aes_params$colour = "#bf0303"
  layers
}
