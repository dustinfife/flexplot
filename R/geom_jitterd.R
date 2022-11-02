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
#' @return a geom of jittered data
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
#' @return a geom of jittered data
position_jitterd<-function(width=NULL,height=NULL,quad.points=100,seed=NA) {
if(!is.null(seed) &&is.na(seed)) {
 seed<-sample.int(.Machine$integer.max,1L)
 }
 ggplot2::ggproto(NULL,PositionJitterd,
 width=width,
 height=height,
 quad.points=quad.points,
 seed=seed
 )
}

#' @importFrom purrr "%||%"
#' @import ggplot2
PositionJitterdodged <- ggproto("PositionJitterdodged", ggplot2::Position,
                                jitter.width = .2,
                                jitter.height = NULL,
                                dodge.width = NULL,
                                
    required_aes = c("x", "y"),
    
    setup_params = function(self, data) {
      flipped_aes <- has_flipped_aes(data)
      data <- flip_data(data, flipped_aes)
      width <- self$jitter.width %||% (resolution(data$x, zero = FALSE) * 0.4)
      #Adjust the x transformation based on the number of 'dodge' variables
      dodgecols <- intersect(c("fill", "colour", "linetype", "shape", "size", "alpha"), colnames(data))
      if (length(dodgecols) == 0) {
        stop("`position_jitterdodged()` requires at least one aesthetic to dodge by", call. = FALSE)
      }
      ndodge    <- lapply(data[dodgecols], levels)  #returns NULL for numeric, i.e. non-dodge layers
      ndodge    <- length(unique(unlist(ndodge)))
      
      list(
        dodge.width = self$dodge.width,
        jitter.height = self$jitter.height,
        jitter.width = width / (ndodge + 2),
        seed = self$seed,
        flipped_aes = flipped_aes
      )
    },
    
    compute_panel = function(data, params, scales) {
      data <- flip_data(data, params$flipped_aes)
      data <- collide(data, params$dodge.width, "position_jitterdodged", ggplot2:::pos_dodge,
                      check.width = FALSE)
      trans_x <- if (params$jitter.width > 0) function(x) jitterd(x,data$y,quad.points=params$quad.points,amount=params$jitter.width)
      trans_y <- if (params$jitter.height > 0) function(x) jitterd(data$y, data$x,quad.points=params$quad.points,amount=params$jitter.height)
      data <- with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
      flip_data(data, params$flipped_aes)
    }
)

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

  # get unique values of y (multiply by 100 and round to get rid of rounding errors)
  unique_y = length(unique(round(y*100)))
	if (length(y)>3 & unique_y>1){
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




#' Simultaneously dodge and jitter
#'
#' This is primarily used for aligning points generated through
#' `geom_point()` with dodged boxplots (e.g., a `geom_boxplot()` with
#' a fill aesthetic supplied).
#'
#' @family position adjustments
#' @param jitter.width degree of jitter in x direction. Defaults to 40\% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param dodge.width The degree of dodging
#' @param seed Random seed. 
#' @return a geom of jittered data
#' 
#' @export
position_jitterdodged <- function(jitter.width = .2, jitter.height = 0,
                                 dodge.width = 0.75, seed = NA) {
  
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggplot2::ggproto(NULL, PositionJitterdodged,
    jitter.width = jitter.width,
    jitter.height = jitter.height,
    dodge.width = dodge.width,
    seed = seed
  )
  
}




#' Utilities for working with bidirecitonal layers
#'
#' These functions are what underpins the ability of certain geoms to work
#' automatically in both directions. See the *Extending ggplot2* for how they
#' are used when implementing `Geom`, `Stat`, and `Position` classes.
#'
#' `has_flipped_aes()` is used to sniff out the orientation of the layer from
#' the data. It has a range of arguments that can be used to finetune the
#' sniffing based on what the data should look like. `flip_data()` will switch
#' the column names of the data so that it looks like x-oriented data.
#' `flipped_names()` provides a named list of aesthetic names that corresponds
#' to the orientation of the layer.
#'
#' @section Controlling the sniffing:
#' How the layer data should be interpreted depends on its specific features.
#' `has_flipped_aes()` contains a range of flags for defining what certain
#' features in the data correspond to:
#'
#' - `main_is_orthogonal`: This argument controls how the existence of only a `x`
#'   or `y` aesthetic is understood. If `TRUE` then the exisiting aesthetic
#'   would be then secondary axis. This behaviour is present in [stat_ydensity()]
#'   and [stat_boxplot()]. If `FALSE` then the exisiting aesthetic is the main
#'   axis as seen in e.g. [stat_bin()], [geom_count()], and [stat_density()].
#' - `range_is_orthogonal`: This argument controls whether the existance of
#'   range-like aesthetics (e.g. `xmin` and `xmax`) represents the main or
#'   secondary axis. If `TRUE` then the range is given for the secondary axis as
#'   seen in e.g. [geom_ribbon()] and [geom_linerange()]. `FALSE` is less
#'   prevalent but can be seen in [geom_bar()] where it may encode the span of
#'   each bar.
#' - `group_has_equal`: This argument controls whether to test for equality of
#'   all `x` and `y` values inside each group and set the main axis to the one
#'   where all is equal. This test is only performed if `TRUE`, and only after
#'   less computationally heavy tests has come up empty handed. Examples are
#'   [stat_boxplot()] and [stat_ydensity]
#' - `ambiguous`: This argument tells the function that the layer, while
#'   bidirectional, doesn't treat each axis differently. It will circumvent any
#'   data based guessing and only take hint from the `orientation` element in
#'   `params`. If this is not present it will fall back to `FALSE`. Examples are
#'   [geom_line()] and [geom_area()]
#' - `main_is_continuous`: This argument controls how the test for discreteness
#'   in the scales should be interpreted. If `TRUE` then the main axis will be
#'   the one which is not discrete-like. Conversely, if `FALSE` the main axis
#'   will be the discrete-like one. Examples of `TRUE` is [stat_density()] and
#'   [stat_bin()], while examples of `FALSE` is [stat_ydensity()] and
#'   [stat_boxplot()]
#'
#' @param data The layer data
#' @param params The parameters of the `Stat`/`Geom`. Only the `orientation`
#'   parameter will be used.
#' @param main_is_orthogonal If only `x` or `y` are present do they correspond
#'   to the main orientation or the reverse. E.g. If `TRUE` and `y` is present
#'   it is not flipped. If `NA` this check will be ignored.
#' @param range_is_orthogonal If `xmin`/`xmax` or `ymin`/`ymax` is present do
#'   they correspond to the main orientation or reverse. If `NA` this check will
#'   be ignored.
#' @param group_has_equal Is it expected that grouped data has either a single
#'   `x` or `y` value that will correspond to the orientation.
#' @param ambiguous Is the layer ambiguous in its mapping by nature. If so, it
#'   will only be flipped if `params$orientation == "y"`
#' @param main_is_continuous If there is a discrete and continuous axis, does
#'   the continuous one correspond to the main orientation?
#' @param flip Logical. Is the layer flipped.
#'
#' @return `has_flipped_aes()` returns `TRUE` if it detects a layer in the other
#' orientation and `FALSE` otherwise. `flip_data()` will return the input
#' unchanged if `flip = FALSE` and the data with flipped aesthetic names if
#' `flip = TRUE`. `flipped_names()` returns a named list of strings. If
#' `flip = FALSE` the name of the element will correspond to the element, e.g.
#' `flipped_names(FALSE)$x == "x"` and if `flip = TRUE` it will correspond to
#' the flipped name, e.g. `flipped_names(FALSE)$x == "y"`
#'
#' @keywords internal
#' @name bidirection 
#' @return a boolean
#'
has_flipped_aes <- function(data, params = list(), main_is_orthogonal = NA,
                            range_is_orthogonal = NA, group_has_equal = FALSE,
                            ambiguous = FALSE, main_is_continuous = FALSE) {
  # Is orientation already encoded in data?
  if (!is.null(data$flipped_aes)) {
    not_na <- which(!is.na(data$flipped_aes))
    if (length(not_na) != 0) {
      return(data$flipped_aes[[not_na[1L]]])
    }
  }
  
  # Is orientation requested in the params
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }
  
  # Does a single x or y aesthetic corespond to a specific orientation
  if (!is.na(main_is_orthogonal) && sum(c("x", "y") %in% names(data)) + sum(c("x", "y") %in% names(params)) == 1) {
    return(("x" %in% names(data) || "x" %in% names(params)) == main_is_orthogonal)
  }
  
  has_x <- !is.null(data$x)
  has_y <- !is.null(data$y)
  
  # Does a provided range indicate an orientation
  if (!is.na(range_is_orthogonal)) {
    if (any(c("ymin", "ymax") %in% names(data))) {
      return(!range_is_orthogonal)
    }
    if (any(c("xmin", "xmax") %in% names(data))) {
      return(range_is_orthogonal)
    }
  }
  
  # If ambiguous orientation = NA will give FALSE
  if (ambiguous && (is.null(params$orientation) || is.na(params$orientation))) {
    return(FALSE)
  }
  
  # Is there a single actual discrete position
  y_is_int <- is.integer(data$y)
  x_is_int <- is.integer(data$x)
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }
  
  # Does each group have a single x or y value
  if (group_has_equal) {
    if (has_x) {
      x_groups <- vapply(split(data$x, data$group), function(x) length(unique(x)), integer(1))
      if (all(x_groups == 1)) {
        return(FALSE)
      }
    }
    if (has_y) {
      y_groups <- vapply(split(data$y, data$group), function(x) length(unique(x)), integer(1))
      if (all(y_groups == 1)) {
        return(TRUE)
      }
    }
  }
  
  # give up early
  if (!has_x && !has_y) {
    return(FALSE)
  }
  
  # Both true discrete. give up
  if (y_is_int && x_is_int) {
    return(FALSE)
  }
  # Is there a single discrete-like position
  y_is_int <- if (has_y) isTRUE(all.equal(data$y, round(data$y))) else FALSE
  x_is_int <- if (has_x) isTRUE(all.equal(data$x, round(data$x))) else FALSE
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }
  # Is one of the axes a single value
  if (all(data$x == 1)) {
    return(main_is_continuous)
  }
  if (all(data$y == 1)) {
    return(!main_is_continuous)
  }
  # If both are discrete like, which have most 0 or 1-spaced values
  y_diff <- diff(sort(data$y))
  x_diff <- diff(sort(data$x))
  
  if (y_is_int && x_is_int) {
    return((sum(x_diff <= 1) < sum(y_diff <= 1)) != main_is_continuous)
  }
  
  y_diff <- y_diff[y_diff != 0]
  x_diff <- x_diff[x_diff != 0]
  
  # If none are discrete is either regularly spaced
  y_is_regular <- if (has_y && length(y_diff) != 0) all((y_diff / min(y_diff)) %% 1 < .Machine$double.eps) else FALSE
  x_is_regular <- if (has_x && length(x_diff) != 0) all((x_diff / min(x_diff)) %% 1 < .Machine$double.eps) else FALSE
  if (xor(y_is_regular, x_is_regular)) {
    return(y_is_regular != main_is_continuous)
  }
  # default to no
  FALSE
}
#' @rdname bidirection
#' @export
flip_data <- function(data, flip = NULL) {
  flip <- flip %||% data$flipped_aes[1] %||% FALSE
  if (flip) {
    names(data) <- switch_orientation(names(data))
  }
  data
}


collide <- function(data, width = NULL, name, strategy,
                    ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width
  
  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$xmin, data$group), ]
  } else {
    data <- data[order(data$xmin, -data$group), ]
  }
  
  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]
  
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }
  
  if (!is.null(data$ymax)) {
    plyr::ddply(data, "xmin", strategy, ..., width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- plyr::ddply(data, "xmin", strategy, ..., width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}


# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide_setup <- function(data, width = NULL, name, strategy,
                          check.width = TRUE, reverse = FALSE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }
    
    # Width determined from data, must be floating point constant
    widths <- unique(data$xmax - data$xmin)
    widths <- widths[!is.na(widths)]
    
    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(widths))) {
    #       warning(name, " requires constant width: output may be incorrect",
    #         call. = FALSE)
    #     }
    width <- widths[1]
  }
  
  list(data = data, width = width)  
}