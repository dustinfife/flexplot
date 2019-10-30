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
#' @inheritParams ggplot2::position_jitter
#' @param quad.points The number of "quadriture points" for the density function. 
#' Higher numbers yield a more detailed jittered density plot
#' @seealso
#' \link[ggplot2]{geom_point} for regular, unjittered points,
#' \link[ggplot2]{geom_boxplot} for another way of looking at the conditional
#'   distribution of a variable
#' \link[ggplot2]{geom_jitter} for standard jittering
#' @export
#' @importFrom magrittr "%>%" 
#' @importFrom purrr "%||%"
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
#'  is added in both positive and negative directions, so the total spread
#'  is twice the value specified here. However, that amount of jittering depends
#' 	 on the density. The location of highest density in the dataset will have 
#'   jittering equal to the amount the user specifies, otherwise, jittering is a 
#'   fraction of that. 
#'
#'  If omitted, defaults to 40\% of the resolution of the data: this means the
#'  jitter values will occupy 80\% of the implied bins for the highest density. Categorical data
#'  is aligned on the integers, so a width or height of 0.5 will spread the
#'  data so it's not possible to see the distinction between the categories.
#' @param quad.points The number of "quadriture points" for the density function. 
#' Higher numbers yield a more detailed jittered density plot
#' @param seed A random seed to make the jitter reproducible.
#'  Useful if you need to apply the same jitter twice, e.g., for a point and
#'  a corresponding label.
#'  The random seed is reset after jittering.
#'  If `NA` (the default value), the seed is initialised with a random value;
#'  this makes sure that two subsequent calls start with a different seed.
#'  Use `NULL` to use the current random seed and also avoid resetting
#'  (the behaviour of \pkg{ggplot} 2.2.1 and earlier).
#' @export
position_jitterd<-function(width=NULL,height=NULL,quad.points=100,seed=NA) {
if(!is.null(seed) &&is.na(seed)) {
 seed<-sample.int(.Machine$integer.max,1L)
 }
 ggproto(NULL,PositionJitterd,
 width=width,
 height=height,
 quad.points=quad.points,
 seed=seed
 )
}


PositionJitterd<- ggplot2::ggproto("PositionJitterd",ggplot2::Position,
required_aes= c("x","y"),

setup_params=function(self,data) {
  list(
  width=self$width%||% (resolution(data$x,zero=FALSE) *0.4),
  height=self$height%||% (resolution(data$y,zero=FALSE) *0.4),
  seed=self$seed,
  quad.points=self$quad.points
  )
 },

compute_layer=function(self,data,params,layout) {
 trans_x<-if(params$width>0)function(x,y) jitterd(x,data$y,quad.points=params$quad.points,amount=params$width)
 trans_y<-if(params$height>0)function(x,y) jitterd(data$y, data$x,quad.points=params$quad.points,amount=params$height)

  with_seed_null(params$seed,ggplot2::transform_position(data,trans_x,trans_y))
 }
)

#' @import withr
with_seed_null<-function(seed,code) {
if(is.null(seed)) {
code
 }else{
 withr::with_seed(seed,code)
 }
}

mp.density=function(y){
	
	if (length(y)>3){
		dens= density(y)
		#### match densities with values
		densities= as.numeric(as.character(cut(y,dens$x,labels=dens$y[-1])))
		densities=densities/max(densities)	
		return(densities)
	} else {
		densities = rep(.1, times=length(y))
	}
}
jitterd=function(x,y,quad.points,amount=NULL, reverse=F){
	if(is.null(amount)){
		amount=0
	}
	k= data.frame(x=x,y=y)
	# if (reverse){
		# k = data.frame(x=y, y=x)
	# }	
	### round x to make sure it's not separating too much
	if(is.numeric(k$x)){
		k$x= round(k$x,digits=2)
	}
	k=k%>% group_by(as.factor(x)) %>% mutate(density=mp.density(y))
	k$x=k$x+ runif(length(k$x), -amount*k$density,amount*k$density)
	return(k$x)	
}




# # 

# #' Simultaneously dodge and jitter
# #'
# #' This is primarily used for aligning points generated through
# #' `geom_point()` with dodged boxplots (e.g., a `geom_boxplot()` with
# #' a fill aesthetic supplied).
# #'
# #' @family position adjustments
# #' @param jitter.width degree of jitter in x direction. Defaults to 40\% of the
# #'   resolution of the data.
# #' @param jitter.height degree of jitter in y direction. Defaults to 0.
# #' @param dodge.width the amount to dodge in the x direction. Defaults to 0.75,
# #'   the default `position_dodge()` width.
# #' @inheritParams position_jitterd
# #' @export
# #' @examples
# #' dsub <- diamonds[ sample(nrow(diamonds), 1000), ]
# #' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
# #'   geom_boxplot(outlier.size = 0) +
# #'   geom_point(pch = 21, position = position_jitterdodge())
# position_jitterddodge <- function(jitter.width = NULL, jitter.height = 0,
                                 # dodge.width = 0.75, seed = NA) {
  # if (!is.null(seed) && is.na(seed)) {
    # seed <- sample.int(.Machine$integer.max, 1L)
  # }

  # ggproto(NULL, PositionJitterddodge,
    # jitter.width = jitter.width,
    # jitter.height = jitter.height,
    # dodge.width = dodge.width,
    # seed = seed
  # )
# }

# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export
# PositionJitterddodge <- ggproto("PositionJitterddodge", Position,
  # jitter.width = NULL,
  # jitter.height = NULL,
  # dodge.width = NULL,

  # required_aes = c("x", "y"),

  # setup_params = function(self, data) {
    # flipped_aes <- has_flipped_aes(data)
    # data <- flip_data(data, flipped_aes)
    # width <- self$jitter.width %||% (resolution(data$x, zero = FALSE) * 0.4)
    # # Adjust the x transformation based on the number of 'dodge' variables
    # dodgecols <- intersect(c("fill", "colour", "linetype", "shape", "size", "alpha"), colnames(data))
    # if (length(dodgecols) == 0) {
      # stop("`position_jitterdodge()` requires at least one aesthetic to dodge by", call. = FALSE)
    # }
    # ndodge    <- lapply(data[dodgecols], levels)  # returns NULL for numeric, i.e. non-dodge layers
    # ndodge    <- length(unique(unlist(ndodge)))

    # list(
      # dodge.width = self$dodge.width,
      # jitter.height = self$jitter.height,
      # jitter.width = width / (ndodge + 2),
      # seed = self$seed,
      # flipped_aes = flipped_aes
    # )
  # },

  # compute_panel = function(data, params, scales) {
    # data <- flip_data(data, params$flipped_aes)
    # data <- collide(data, params$dodge.width, "position_jitterddodge", pos_dodge,
      # check.width = FALSE)

    # trans_x <- if (params$jitter.width > 0) function(x) jitterd(x,data$y,quad.points=params$quad.points,amount=params$width)
    # trans_y <- if (params$jitter.height > 0) function(x) jitterd(data$y, data$x,quad.points=params$quad.points,amount=params$height)

    # data <- with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    # flip_data(data, params$flipped_aes)
  # }
# )