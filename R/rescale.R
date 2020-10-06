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
