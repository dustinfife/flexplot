flexplot_association_object = function(data, outcome, axis) {
  if (length(axis) > 1) {
    stop("Association plots with a second-axis predictor are not supported.")
  }

  for (variable in attr(data, "association_ordered")) {
    data[[variable]] = ordered(data[[variable]], levels=levels(data[[variable]]))
  }
  xvar = axis[1]
  ggplot2::ggplot(data, ggplot2::aes(
    x=.data[[xvar]], y=.data[["Proportion"]], fill=.data[[outcome]]
  )) +
    ggplot2::geom_bar(stat="identity", position="dodge") +
    # Legacy table() output uses an unordered discrete fill palette.
    ggplot2::scale_fill_discrete() +
    ggplot2::labs(x=xvar, y="Proportion", fill=outcome) +
    ggplot2::theme_bw() +
    ggplot2::theme(text=ggplot2::element_text(size=14))
}
