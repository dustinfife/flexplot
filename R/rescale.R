##' Transform a variable to have a particular mean and standard deivation
##'
##' This function will take a variable and transform it such that it has a particular mean and variance
##'	
##' @param x A vector containing the data the user wishes to transform
##' @param new.mean The desired mean of the dataset
##' @param new.sd The desired sd of the dataset
##' @author Dustin Fife
##' @export
##' @examples
##' x = rnorm(100, 0, 1)
##' ## transform to have a mean of 20 and sd of 5
##' x.new = rescale(x, 50, 5)
##' mean(x.new); sd(x.new)
rescale = function(x, new.mean, new.sd){
  m2 = new.mean+(x-mean(x, na.rm=T))*(new.sd/sd(x, na.rm=T))
  m2
}

#' Replace minimum value with a specified floor
#'
#' @param x A numeric vector
#' @param min.val The minimum value of the dataset
#' @param max.val The maximum value of the dataset
#'
#' @return A new vector where the values < min.val are replaced with min.val (and
#' likewise for the max values)
#' @export
#'
#' @examples
#' testval = c(-1, 0, 0, 60, 100, 101)
#' floor_ceiling(testval, min.val=-1, max.val=100)
floor_ceiling = function(x, min.val=NULL, max.val=NULL) {
  if (!is.null(min.val)) x[x<min.val] = min.val 
  if (!is.null(max.val)) x[x>max.val] = max.val 
  return(x)
}

