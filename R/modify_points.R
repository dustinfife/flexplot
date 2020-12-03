#' Modify the points in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param shape The new shape desired. Defaults to 19 (circles). 
#' @param colour The new colour desired. Defaults to "black". 
#' @param size The new size of the points desired. Defaults to 1.5. 
#'
#' @return A new plot with the modifications
#' @export
modify_points = function(p, shape=19, colour="black", size=1.5) {
  c = ggplot2::ggplot_build(p)
  
  # make modifications
  c$data[[1]]$size = size
  c$data[[1]]$colour = colour
  c$data[[1]]$shape = shape
  c = ggplot2::ggplot_gtable(c)
  
  # return plot
  plot(c)
}