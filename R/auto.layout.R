##' Given a particular number of plots, \code{auto.layout} will automatically determine the arrangement of each
##' plot using the \code{layout} function. See examples. 
##'
##' @title Automatically select the layout.
##' @param n the number of plots
##' @param layout should the fuction return a preallocated layout object? If \code{FALSE}, it returns a matrix
##' @return either a matrix or a layout object
##' @author Dustin Fife
##' @export
##' @examples
##' ## plot six plots
##' auto.layout(6)
##' for (i in 1:6){
##' 	plot(rnorm(100), rnorm(100))	
##' }
##' ## same as mar(mfrow=c(3,2))
##' par(mfrow=c(3,2))
##' for (i in 1:6){
##' 	plot(rnorm(100), rnorm(100))	
##' }
##' ## default for odd number of plots using mfrow looks terrible
##' par(mfrow=c(3,2))
##' for (i in 1:5){
##' 	plot(rnorm(100), rnorm(100))	
##' }
##' ## much better with auto.layout
##' auto.layout(5)
##' for (i in 1:5){
##' 	plot(rnorm(100), rnorm(100))	
##' }
auto.layout = function(n, layout=T){
	### figure out how many rows
	sq = sqrt(n)
	rws = round(sq)
	
	#### if it's a perfect square, fill the matrix
	if (sqrt(n) == round(sqrt(n))){
		numbs = sort(rep(1:n, times=2))
		m = matrix(numbs, nrow=sq, byrow=T)
	} else {

		#### repeat twice the numbers that fit nicely
		topNum = trunc(n/rws)*rws
		numbs = sort(rep(1:topNum, times=2))
		if (topNum==n){
			m = matrix(numbs, nrow=rws, byrow=T)
		} else {
			#### get the rest figured out
			rem = n-topNum  ### remaining numbers
			rest = sort(rep((topNum+1):n, times=2))
			cols = (topNum/rws)*2
			rest = c(rep(0, times=(cols-length(rest))/2), rest, rep(0, times=(cols-length(rest))/2))
			m = matrix(c(numbs, rest), nrow=rws+1, byrow=T)
		}
	}
	
	if (layout){
		layout(m)
	} else {
		m
	}
}