#' Modify the points in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param shape The new shape desired. Defaults to 19 (circles). 
#' @param colour The new colour desired. Defaults to "black". 
#' @param size The new size of the points desired. Defaults to 1.5. 
#'
#' @return A new plot with the modifications
#' @export
#' @importFrom graphics plot
modify_points = function(p, shape=19, colour="black", size=1.5) {
  c = ggplot2::ggplot_build(p)
  
  # make modifications
  c$data[[1]]$size = size
  c$data[[1]]$colour = colour
  c$data[[1]]$shape = shape
  c = ggplot2::ggplot_gtable(c)
  
  # return plot
  c = graphics::plot(c)
  return(c)
}

#' Modify the labels in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param y The new label for the variable on the y axis
#' @param x The new label for the variable on the x axis
#' @param colour The new label for the variable in the legend. 
#'  (This will also modify the underlying shape/line arguments)
#' @param row_panels The new labels for the variable in row panels
#' @param col_panels The new labels for the variable in column panels
#'
#' @return
#' @export
#'
#' @examples
modify_labels = function(p, y=NULL, x=NULL, color=NULL, row_panels=NULL, col_panels=NULL) {
  
  if (!is.null(y)) p$labels$y = y
  if (!is.null(x)) p$labels$x = x
  if (!is.null(color)) { 
    p$labels$colour = color
    p$labels$linetype = color
    p$labels$shape = color
  }
  
  ## if there's panels
  if (!is.null(row_panels)) names(p$facet$params$rows) = row_panels
  if (!is.null(col_panels)) names(p$facet$params$cols) = col_panels
  
  return(p)
}  

#' Modify the fitted line for a flexplot graphic
#'
#' @param p A flexplot graphic
#' @param method Type of smoothing (fitted) funciton used. Can be 
#' rlm, poisson, loess, Gamma, polynomial, cubic, or lm
#' @param se Should standard errors (confidence bands) be displayed
#'
#' @return
#' @export
#'
#' @examples
modify_smooth = function(p, method="lm", se=F, color=NULL) {

  # delete existing smoothing layers
  p = remove_geom(p, "GeomAbline")
  p = remove_geom(p, "GeomSmooth")
  
  # add geom for color
  added_geom = return_color_manual_geom(p, color)
  
  # convert method into something ggplot can understand
  method_call = smooth_method_check(method)
  
  # change the color
  if (!is.null(color) & length(color) == 1) method_call = gsub("),", ",", paste0(method_call, ", colour = '", color, "')"), fixed=T)
  p + suppressMessages(eval(parse(text=method_call))) + added_geom
}

return_color_manual_geom = function(p, color=NULL) {
  
  #return empty geom if they don't provide a color or if the plot doesn't have color
  if (is.null(color) | !("colour" %in% names(p$mapping))) return(geom_blank())
  
  # make sure they have a vector the same length as data
  variable_name = quo_name(p$mapping$colour)
  levels_of_group = levels(p$data[[variable_name]])

  if (length(color) != length(levels_of_group)) {
    stop(paste0("Your grouping variable (", variable_name, ") contains ", length(levels_of_group), " levels.
                Your color vector needs to be the same length"))
  }
  
  return(scale_color_manual(values=color))
}


remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) NULL else  x
  })
  
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

smooth_method_check = function(method=NULL) {
  if (is.null(method)) return ('geom_smooth(method="loess", se=se, formula = y~x)')
  if (method=="rlm") return('geom_smooth(method = "rlm", se = se, formula = y~x)')
  if (method=="poisson") return('geom_smooth(method = "glm", method.args = list(family = "poisson"), se = se, formula = y~x)')
  if (method=="Gamma") return('geom_smooth(method = "glm", method.args = list(family = "Gamma"), se = se, formula = y~x)')  
  if (method=="polynomial" | method == "quadratic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))')
  if (method=="cubic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))')
  if (method=="lm") return('stat_smooth(method="lm", se=se, formula = y~x)')
}
