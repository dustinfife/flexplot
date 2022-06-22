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
#' @example 
#' flexplot(weight.loss~motivation, data=d) %>% modify_points(shape = 12, colour="green", size=2")
modify_points = function(p, shape=19, colour="black", size=1.5) {
  c = ggplot2::ggplot_build(p)
  
  # make modifications
  c$data[[1]]$size = size
  c$data[[1]]$colour = colour
  c$data[[1]]$shape = shape
  c = ggplot2::ggplot_gtable(c)
  
  # return plot
  c = graphics::plot(c)
  # return(c)
}

#' Modify the labels in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param x The label for the x axis
#' @param y The label for the y axis
#' @param colors The label for the variable encoded as a color
#' @param panel_rows The label for the variable encoded in row panels
#' @param panel_cols The label for the variable encoded in column panels
#' @return A new plot with the modifications
#' @export
#' @importFrom graphics plot
#' @example 
#' flexplot(weight.loss~motivation + therapy.type | gender + muscle.gain, data=d) %>%
#'    modify_labels(x="Motivation", y="Weight Loss", 
#'        color = "Therapy Type", panel_rows = "Gender", panel_cols = "Muscle Gain")
modify_labels = function(p, x=NULL, y=NULL, colors=NULL, panel_rows=NULL, panel_cols=NULL) {

  p$labels$x = ifelse(is.null(x), p$labels$x, x)
  p$labels$y = ifelse(is.null(y), p$labels$y, y)
  
  if (!is.null(colors)) {
    p$labels$colour = colors
    p$labels$linetype = colors
    p$labels$shape = colors
  }
  
  if (!is.null(panel_rows)) names(p$facet$params$rows) = panel_rows
  if (!is.null(panel_cols)) names(p$facet$params$cols) = panel_cols
  
  return(p)
}


#' Modify the fitted line in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param color The color of the line
#' @param size The size (thickness) of the line
#' @param linetype The type of line as a string (e.g., "dotted")
#' @param method The type of smoothing function applied. Can be lm, quadratic, cubic, rlm, logistic,
#' poisson, or loess
#' @param se Standard errors. If TRUE, a band will be displayed showing the standard errors
#' @return A new plot with the modifications
#' @export
#' @importFrom graphics plot
#' @example 
#' flexplot(weight.loss~motivation, data=d) %>%
#'    modify_smooth(color="green", size=2, linetype="dashed", method="lm", se=F)
modify_smooth = function(p, color=NULL, size=NULL, linetype=NULL, method=NULL, se=NULL) {
  ## check if there's a smoothing parameter
  geoms = sapply(p$layers, function(x) class(x$geom)[1])
  which_geom_is_smooth = which(geoms == "GeomSmooth")
  if (sum(which_geom_is_smooth)==0) stop("Sorry, I can't modify a geom in your plot because you don't have a smoothing geom.")
  
  # change the smoothing parameters
  # idenfity whether they have a previous se
  if (is.null(se)) se = p$layers[[which_geom_is_smooth[1]]]$computed_stat_params$se
  # if they modify the soothing parameter, I need to erase the original geom then replace with something else
  if (!is.null(method)) p$layers[[which_geom_is_smooth[1]]] = identify_smoother(method, se)
  
  # change aesthetics
  if (!is.null(color)) p$layers[[(which_geom_is_smooth)[1]]]$aes_params$colour = color
  if (!is.null(size))  p$layers[[(which_geom_is_smooth)[1]]]$aes_params$size   = size
  if (!is.null(linetype))  p$layers[[(which_geom_is_smooth)[1]]]$aes_params$linetype  = linetype
  
  return(p)
}


identify_smoother = function(method, se=TRUE) {
  if (method == "suppress") return(NULL)
  if (method=="logistic") return(geom_smooth(method = "glm", method.args = list(family = "binomial"), formula = y~x))			
  if (method=="rlm") return(geom_smooth(method = "rlm",  se=se, formula = y~x))
  if (method=="poisson" | method=="Gamma") return(geom_smooth(method = "glm", se=se, 
                                                method.args = list(family = method), formula = y~x))
  if (method=="polynomial" | method == "quadratic") return(stat_smooth(method="lm", se=se, 
                                                formula=y ~ poly(x, 2, raw=TRUE)))
  if (method=="cubic") return(stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE)))
  if (method=="lm") return(stat_smooth(method="lm", se=se, formula = y~x))
  
  return(geom_smooth(method="loess", se=se, formula = y~x))
}

