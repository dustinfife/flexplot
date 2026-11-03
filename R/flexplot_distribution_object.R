flexplot_distribution_object = function(data, outcome,
                                        plot.type="histogram", bins=3) {
  
  p = ggplot2::ggplot(data)
  
  # Categorical outcomes use bar charts
  if (!is.numeric(data[[outcome]])) {
    return(
      p +
        ggplot2::geom_bar(ggplot2::aes(x=.data[[outcome]])) +
        ggplot2::theme_bw() +
        ggplot2::labs(x=outcome)
    )
  }
  
  if (identical(plot.type, "qq")) {
    return(
      p +
        ggplot2::stat_qq(ggplot2::aes(sample=.data[[outcome]])) +
        ggplot2::stat_qq_line(ggplot2::aes(sample=.data[[outcome]])) +
        ggplot2::theme_bw() +
        ggplot2::labs(x=outcome)
    )
  }
  
  if (identical(plot.type, "density")) {
    return(
      p +
        ggplot2::geom_density(ggplot2::aes(x=.data[[outcome]])) +
        ggplot2::theme_bw() +
        ggplot2::labs(x=outcome)
    )
  }
  
  if (identical(plot.type, "boxplot")) {
    return(
      p +
        ggplot2::geom_boxplot(ggplot2::aes(x=.data[[outcome]])) +
        ggplot2::theme_bw() +
        ggplot2::coord_flip() +
        ggplot2::theme(
          axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank()
        )
    )
  }
  
  levels = length(unique(data[[outcome]]))
  bins = calculate_bins_for_histograms(bins, levels)
  
  return(
    p +
      ggplot2::geom_histogram(
        ggplot2::aes(x=.data[[outcome]]),
        fill="lightgray",
        color="black",
        bins=bins
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x=outcome)
  )
}