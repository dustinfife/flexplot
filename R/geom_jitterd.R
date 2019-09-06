#' Jittered density points
#'
#' The jitterd geom adds a small amount of random
#' variation to the location of each point, but that amount
#' of noise is proportional to the density. In some ways, it's 
#' a mix between geom_jitter and geom_violin. It reduces
#' overplotting caused by discreteness in smaller datasets. This
#' function was adapted from geom_jitter within ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_jitterd
#' @seealso
#'  \link[ggplot2]{geom_point()} for regular, unjittered points,
#'  \link[ggplot2]{geom_boxplot()} for another way of looking at the conditional
#'     distribution of a variable
#'  \link[ggplot2]{geom_jitter()} for standard jittering
#' @export
#' @import tidyverse
#' @examples
#' p <- ggplot(mpg, aes(cyl, hwy))
#' p + geom_point()
#' p + geom_jitterd()
#'
#' # Add aesthetic mappings
#' p + geom_jitterd(aes(colour = class))
#'
#' # Use smaller width/height to emphasise categories
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitterd()
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitterd(width = 0.25)
#'
#' # Use larger width/height to completely smooth away discreteness
#' ggplot(mpg, aes(cty, hwy)) + geom_jitterd()
#' ggplot(mpg, aes(cty, hwy)) + geom_jitterd(width = 0.5, height = 0.5)
geom_jitterd <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "jitter",
                        ...,
                        width = NULL,
                        height = NULL,
                        quad.points=100,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (!missing(width) || !missing(height)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `width`/`height`.", call. = FALSE)
    }

position <- position_jitterd(width = width, height = height, quad.points=quad.points)
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' Jitter points based on density to avoid overplotting
#'
#' Counterintuitively adding random noise to a plot can sometimes make it
#' easier to read. Jittering is particularly useful for small datasets with
#' at least one discrete position. Jittering based on density ensures that
#' the amount of random noise only increases as the probability of overlap
#' increases.
#'
#' @family position adjustments
#' @param width,height Amount of vertical and horizontal jitter. The jitter
#'   is added in both positive and negative directions, so the total spread
#'   is twice the value specified here. However, that amount of jittering depends
#' 	 on the density. The location of highest density in the dataset will have 
#'   jittering equal to the amount the user specifies, otherwise, jittering is a 
#'   fraction of that. 
#'
#'   If omitted, defaults to 40\% of the resolution of the data: this means the
#'   jitter values will occupy 80\% of the implied bins for the highest density. Categorical data
#'   is aligned on the integers, so a width or height of 0.5 will spread the
#'   data so it's not possible to see the distinction between the categories.
#' @param seed A random seed to make the jitter reproducible.
#'   Useful if you need to apply the same jitter twice, e.g., for a point and
#'   a corresponding label.
#'   The random seed is reset after jittering.
#'   If `NA` (the default value), the seed is initialised with a random value;
#'   this makes sure that two subsequent calls start with a different seed.
#'   Use `NULL` to use the current random seed and also avoid resetting
#'   (the behaviour of \pkg{ggplot} 2.2.1 and earlier).
#' @export
#' @import tidyverse
#' @examples
#' # Jittering is useful when you have a discrete position, and a relatively
#' # small number of points
#' # take up as much space as a boxplot or a bar
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(colour = "grey50") +
#'   geom_jitterd()
#'
#' # If the default jittering is too much, as in this plot:
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitterd()
#'
#' # You can adjust it in two ways
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitterd(width = 0.1, height = 0.1)
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitterd(position = position_jitter(width = 0.1, height = 0.1))
#'
#' # Create a jitter object for reproducible jitter:
#' jitter <- position_jitterd(width = 0.1, height = 0.1)
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_point(position = jitterd) +
#'   geom_point(position = jitterd, color = "red", aes(am + 0.2, vs + 0.2))
position_jitterd <- function(width = NULL, height = NULL, quad.points=100, seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  ggproto(NULL, PositionJitterd,
    width = width,
    height = height,
    quad.points = quad.points,
    seed = seed
  )
}

#' @import tidyverse
PositionJitterd <- ggproto("PositionJitterd", Position,
  required_aes = c("x", "y"),
  setup_params = function(self, data) {
	require(tidyverse)
    list(
      width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
      height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
      seed = self$seed,
      quad.points = self$quad.points
    )
  },

  compute_layer = function(self, data, params, layout) {
    trans_x <- if (params$width > 0) function(x,y) jitterd(x, data$y, quad.points=params$quad.points, amount = params$width)
    trans_y <- if (params$height > 0) function(x) jitter(x, amount = params$height)

    with_seed_null(params$seed, ggplot2::transform_position(data, trans_x, trans_y))
  }
)

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
  code
  } else {
    withr::with_seed(seed, code)
  }
}

mp.density = function(y){
	
	if (length(y)>3){
		dens = density(y)
		#### match densities with values
		densities = as.numeric(as.character(cut(y, dens$x, labels=dens$y[-1])))
		densities = densities/max(densities)	
		return(densities)
	} else {
		densities = rep(.1, times=length(y))
	}
}

jitterd = function(x, y, quad.points, amount=NULL){
	if (is.null(amount)){
		amount = 0
	} 
	
	k = data.frame(x=x,y=y)	
	### round x to make sure it's not separating too much
	if (is.numeric(k$x)){
		k$x = round(k$x, digits=2)
	}
	

	k = k%>% group_by(as.factor(x)) %>% mutate(density=mp.density(y))
	k$x = k$x + runif(length(k$x), -amount*k$density, amount*k$density)

	return(k$x)
	
}