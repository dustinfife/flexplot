
flexplot_related_object = function(data, jitter=NULL,
                                   plot.type="histogram",
                                   spread="quartiles",
                                   alpha=.99977,
                                   raw.data=TRUE,
                                   sample=Inf,
                                   suppress_smooth=FALSE) {
  
  levs = attr(data, "levels")
  
  x_label = "Difference"
  if (!is.null(levs) && length(levs) >= 2) {
    x_label = paste0(
      "Difference (", levs[2], "-", levs[1], ")"
    )
  }
  
  jitter = match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)
  
  p = ggplot2::ggplot(
    data,
    ggplot2::aes(
      x=1,
      y=.data[["Difference"]]
    )
  ) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(
      yintercept=0,
      color="lightgray"
    ) +
    ggplot2::labs(x=x_label) +
    ggplot2::theme(
      axis.text.x=ggplot2::element_blank(),
      axis.ticks.x=ggplot2::element_blank()
    ) +
    geom_jitterd(
      data=points_data,
      alpha=raw.alph.func(raw.data, alpha),
      width=jitter[1] * .5,
      height=jitter[2] * .5
    )
  
  if (identical(plot.type, "boxplot")) {
    return(p + ggplot2::geom_boxplot(alpha=.1))
  }
  
  if (identical(plot.type, "violin")) {
    return(p + ggplot2::geom_violin(alpha=.1))
  }
  
  if (suppress_smooth) return(p)
  
  p +
    flexplot_categorical_summary_layers(spread) +
    ggplot2::coord_cartesian(xlim=c(.75, 1.25))
}
