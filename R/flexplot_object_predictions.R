flexplot_prediction_layer = function(prediction, axis, data,
                                     use_magnet=FALSE) {
  
  xvar = axis[1]
  
  if (use_magnet) {
    return(ggplot2::geom_point(
      data=prediction,
      mapping=ggplot2::aes(
        x=.data[[xvar]],
        y=.data[["prediction"]],
        color=.data[["model"]]
      ),
      size=3,
      inherit.aes=FALSE
    ))
  }
  
  if (!is.numeric(data[[xvar]])) {
    return(list(
      ggplot2::geom_point(
        data=prediction,
        mapping=ggplot2::aes(
          x=.data[[xvar]],
          y=.data[["prediction"]],
          color=.data[["model"]]
        ),
        position=ggplot2::position_dodge(width=0.4),
        inherit.aes=FALSE
      ),
      ggplot2::geom_line(
        data=prediction,
        mapping=ggplot2::aes(
          x=.data[[xvar]],
          y=.data[["prediction"]],
          linetype=.data[["model"]],
          group=.data[["model"]],
          color=.data[["model"]]
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