#' Produce a bunch of univariate plots
#'
#' @param names A vector containing the names of the variables you want to plot
#' @param data A dataset
#'
#' @return A list of plots
#' @export
#'
#' @examples
#' univariate_lists(c("a", "b", "z"), small)
univariate_list = function(names, data) {
  if (!all(names %in% names(data))) stop("One or more of your variable names isn't in your dataset")
  plot_list = list()
  for (i in 1:length(names)) {
    f = formula(paste0(names[i], "~1"))
    plot_list[[i]] = flexplot(f, data=data)
  }
  plot_list
}
