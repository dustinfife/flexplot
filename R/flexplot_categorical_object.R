# Native ggplot renderer for categorical predictors and a numeric outcome.
flexplot_categorical_summary = function(z, spread="quartiles") {
  if (identical(spread, "quartiles")) {
    return(data.frame(
      y=stats::median(z),
      ymin=stats::quantile(z, .25, na.rm=TRUE),
      ymax=stats::quantile(z, .75, na.rm=TRUE)
    ))
  }

  center = mean(z)
  interval = stats::sd(z)
  if (identical(spread, "sterr")) {
    interval = 1.96 * interval / sqrt(length(z) - 1)
  }

  data.frame(y=center, ymin=center - interval, ymax=center + interval)
}

flexplot_categorical_summary_layers = function(spread="quartiles", group=NULL) {
  dodge = if (identical(spread, "quartiles")) .4 else .5
  layers = list(
    ggplot2::stat_summary(fun.data=flexplot_categorical_summary,
                          fun.args=list(spread=spread), geom="point", size=3,
                          position=ggplot2::position_dodge(width=dodge)),
    ggplot2::stat_summary(fun.data=flexplot_categorical_summary,
                          fun.args=list(spread=spread), geom="errorbar",
                          linewidth=1.25, width=.2,
                          position=ggplot2::position_dodge(width=dodge))
  )

  if (is.null(group)) {
    layers[[1]]$aes_params$colour = "#bf0303"
    layers[[2]]$aes_params$colour = "#bf0303"
    return(layers)
  }

  line_dodge = if (identical(spread, "stdev")) .5 else .4
  c(layers, list(ggplot2::stat_summary(
    mapping=ggplot2::aes(group=.data[[group]]),
    fun.data=flexplot_categorical_summary,
    fun.args=list(spread=spread), geom="line",
    position=ggplot2::position_dodge(width=line_dodge)
  )))
}

flexplot_categorical_object = function(data, outcome, axis, plot.type="histogram",
                                        spread="quartiles", suppress_smooth=FALSE,
                                        alpha=.5, jitter=NULL, sample=Inf,
                                        raw.data=TRUE) {
  jitter = match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)
  x = axis[1]
  group = if (length(axis) > 1) axis[2] else NULL

  if (is.null(group)) {
    mapping = ggplot2::aes(x=.data[[x]], y=.data[[outcome]])
    points = geom_jitterd(data=points_data, alpha=raw.alph.func(raw.data, alpha),
                          width=jitter[1], height=jitter[2])
  } else {
    mapping = ggplot2::aes(
      x=.data[[x]], y=.data[[outcome]],
      color=.data[[group]], shape=.data[[group]], linetype=.data[[group]]
    )
    points = ggplot2::geom_point(
      data=points_data, alpha=raw.alph.func(raw.data, alpha),
      position=position_jitterdodged(
        jitter.width=jitter[1], jitter.height=jitter[2], dodge.width=.5
      )
    )
  }

  p = ggplot2::ggplot(data, mapping) + points + ggplot2::theme_bw() +
    ggplot2::theme(text=ggplot2::element_text(size=14))

  if (!is.null(group)) {
    if (suppress_smooth) return(p)
    return(p + flexplot_categorical_summary_layers(spread, group))
  }

  if (identical(plot.type, "boxplot")) {
    p = p + ggplot2::geom_boxplot(alpha=.1)
  } else if (identical(plot.type, "violin")) {
    p = p + ggplot2::geom_violin(alpha=.1)
  } else if (identical(plot.type, "line")) {
    p = p + ggplot2::geom_line()
  } else if (!suppress_smooth) {
    p = p + flexplot_categorical_summary_layers(spread)
  }

  p
}
