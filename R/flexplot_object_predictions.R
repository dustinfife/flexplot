flexplot_prediction_layer = function(prediction, axis, data,
                                     use_magnet=FALSE, given=NA) {
  
  xvar = axis[1]
  
  if (use_magnet) {
    return(flexplot_magnet_prediction_layer(prediction, axis, data, given))
  }
  
  axis = flexplot_object_axis(data, axis)
  group = if (length(axis) >= 2 && !is.na(axis[2])) axis[2] else "model"

  if (!is.numeric(data[[xvar]])) {
    return(list(
      ggplot2::geom_point(
        data=prediction,
        mapping=ggplot2::aes(
          x=.data[[xvar]],
          y=.data[["prediction"]],
          color=.data[[group]]
        ),
        position=ggplot2::position_dodge(width=0.4),
        inherit.aes=FALSE
      ),
      ggplot2::geom_line(
        data=prediction,
        mapping=ggplot2::aes(
          x=.data[[xvar]],
          y=.data[["prediction"]],
          linetype=.data[[group]],
          group=.data[[group]],
          color=.data[[group]]
        ),
        position=ggplot2::position_dodge(width=0.4),
        inherit.aes=FALSE
      )
    ))
  }
  
  if (length(axis) >= 2 && !is.na(axis[2])) {
    return(ggplot2::geom_line(
      data=prediction,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=.data[["prediction"]],
        linetype=.data[[axis[2]]],
        color=.data[[axis[2]]],
        group=.data[[axis[2]]]
      ),
      inherit.aes=FALSE
    ))
  }
  
  return(list(
    ggplot2::geom_line(
      data=prediction,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=.data[["prediction"]],
        linetype=.data[["model"]],
        color=.data[["model"]],
        group=.data[["model"]]
      ),
      inherit.aes=FALSE
    ),
    ggplot2::scale_linetype_manual(
      values=c("solid", "dotdash")
    )
  ))
}

flexplot_magnet_comparison = function(prediction, axis, data, given=NA) {
  result = list(prediction=prediction, facet=NULL)
  if (length(axis) < 2 || is.na(axis[2])) return(result)
  if (length(unique(prediction$model)) < 2) return(result)

  # Only predictions receive the model key. ggplot repeats the other layers
  # across model panels while retaining their conditioning keys.
  variables = make.unique(c(names(data), names(prediction), "Model"))
  model_variable = variables[length(variables)]
  prediction[[model_variable]] = prediction$model
  conditioning = flexplot_magnet_display_given(data, given)
  conditioning = conditioning[!is.na(conditioning) & nzchar(conditioning)]

  list(
    prediction=prediction,
    facet=ggplot2::facet_grid(
      rows=ggplot2::vars(!!!rlang::syms(conditioning)),
      cols=ggplot2::vars(!!rlang::sym(model_variable)),
      labeller=custom.labeler
    )
  )
}

flexplot_magnet_prediction_layer = function(prediction, axis, data, given=NA) {
  axis = flexplot_object_axis(data, axis)
  xvar = axis[1]
  group = if (length(axis) > 1 && !is.na(axis[2])) axis[2] else NULL
  position = ggplot2::position_identity()
  mapping = ggplot2::aes(x=.data[[xvar]], y=.data$prediction, shape=.data$model)

  if (!is.null(group)) {
    if (is.factor(data[[group]])) {
      prediction[[group]] = factor(prediction[[group]], levels=levels(data[[group]]),
                                    ordered=is.ordered(data[[group]]))
    }
    mapping$colour = ggplot2::aes(colour=.data[[group]])$colour
    mapping$group = mapping$colour
    # Use the same slots as the internal fits, independently of model shape.
    position = flexplot_magnet_summary_position(data, group)
  }

  models = levels(factor(prediction$model))
  shapes = if (is.null(group)) {
    rep(c(17, 15, 18, 3, 7, 8), length.out=length(models))
  } else {
    rep(17, length(models))
  }
  list(
    ggplot2::geom_point(
      data=prediction,
      mapping=mapping,
      position=position,
      size=3,
      inherit.aes=FALSE,
      show.legend=FALSE
    ),
    ggplot2::scale_shape_manual(
      values=shapes,
      guide="none"
    )
  )
}
