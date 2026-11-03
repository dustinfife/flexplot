# Native ggplot renderer for a single categorical predictor and numeric outcome.
flexplot_categorical_summary_layers = function(spread="quartiles") {
  if (identical(spread, "stdev")) {
    center = mean
    lower = function(z) mean(z) - stats::sd(z)
    upper = function(z) mean(z) + stats::sd(z)
    dodge = .5
  } else if (identical(spread, "sterr")) {
    center = mean
    lower = function(z) mean(z) - 1.96 * stats::sd(z) / sqrt(length(z) - 1)
    upper = function(z) mean(z) + 1.96 * stats::sd(z) / sqrt(length(z) - 1)
    dodge = .5
  } else {
    center = stats::median
    lower = function(z) stats::quantile(z, .25, na.rm=TRUE)
    upper = function(z) stats::quantile(z, .75, na.rm=TRUE)
    dodge = .4
  }

  list(
    ggplot2::stat_summary(fun=center, geom="point", size=3,
                          position=ggplot2::position_dodge(width=dodge), color="#bf0303"),
    ggplot2::stat_summary(fun=center, fun.min=lower, fun.max=upper,
                          geom="errorbar", linewidth=1.25, width=.2,
                          position=ggplot2::position_dodge(width=dodge), color="#bf0303")
  )
}

flexplot_categorical_object = function(data, outcome, axis, plot.type="histogram",
                                        spread="quartiles", suppress_smooth=FALSE,
                                        alpha=.5, jitter=NULL, sample=Inf,
                                        raw.data=TRUE) {
  jitter = match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)

  p = ggplot2::ggplot(data, ggplot2::aes(
    x=.data[[axis]], y=.data[[outcome]]
  )) + geom_jitterd(data=points_data, alpha=raw.alph.func(raw.data, alpha),
                     width=jitter[1], height=jitter[2])

  if (identical(plot.type, "boxplot")) {
    p = p + ggplot2::geom_boxplot(alpha=.1)
  } else if (identical(plot.type, "violin")) {
    p = p + ggplot2::geom_violin(alpha=.1)
  } else if (identical(plot.type, "line")) {
    p = p + ggplot2::geom_line()
  } else if (!suppress_smooth) {
    p = p + flexplot_categorical_summary_layers(spread)
  }

  p + ggplot2::theme_bw() + ggplot2::theme(text=ggplot2::element_text(size=14))
}
