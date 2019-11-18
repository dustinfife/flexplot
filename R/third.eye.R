#' Visualize flexplot with the "third eye"
#'
#' Sometimes with multivariate data, it is important to be able to view the graphics from different
#' perspectives (e.g., swapping the axis with a paneled variable). One could simply modify the formula in \code{"flexplot()"}, but that's tedious. 
#' It is much better to hire a psychic to do the modifications for you. That's where \code{"third.eye()"} comes in.
#' With this simple function, the user only needs to specify a formula, and it will permute the different combinations ofr
#' formulae to generate different views. So, put on your psychic lenses and prepare for a wild ride. 
#'
#' @param plot A \code{"flexplot()"} image. Sometimes it's just nice to have a graphic you KNOW you want to display and just look at other views, eh? That's what this is for.
#' But it's not necessary. I don't care much. We're good. 
#' @param formula A \code{"flexplot()"}-style formula (e.g., \code{"y~x1 + x2 | x3 + x4"})
#' @param data A dataset. 
#' @param fixed.positions a vector of booleons that is as long as the number of variables specified. This will specify which variables ought to remain fixed in
#' place. For example, if the formula is \code{"y~x1 + x2 | x3 + x4"} and third.eye is \code{"c(F,F,T,T)"}, the function will only
#' vary the positions of x3 and x4. 
#' @param which.perms a vector of numbers. Normally \code{"third.eye()"} will randomly select from the possible permutations of the equation to generate new views. The user
#' can specify a vector of numbers to specify which of the permutations to display. How do you know which permutations you want, you ask? Hell if I know. Just put some numbers
#' in and choose what suits your fancy. 
#' @param ... Other parameters passed to flexplot. 
#' @seealso \code{\link{flexplot}}
#'
#' @return a Graphic
#'
#' @examples
#' data(exercise_data)
#' third.eye(weight.loss~gender + motivation | therapy.type + health, data=exercise_data)
#' @export
third.eye = function(formula, data, fixed.positions=NULL, which.perms=NULL, plot=NULL, ...){
	
	### make sure the permutations are possible
	# if (!is.null(which.perms)){
	# 	all.possible = rotate.view(formula, fixed.positions, return.perms=T)		
	# 	if (!all(which.perms %in% 1:nrow(all.possible))){
	# 		msg = paste0("\n Well snizzzy-izap! There's a problem. It looks like at least one of your values in permutations is larger than the number of possible permutations (", nrow(all.possible), "). Make sure all values you specify are smaller than that number.\nOr maybe you should take a nap? You look tired. ")
	# 		stop(msg)
	# 	}
	# }
	# 
	# 
	# ### if permutations is null, choose to plot four images
	# num.plots = ifelse(is.null(which.perms), 4, length(which.perms))
	# 
	# ### loop through and flexplot that shiz a number of times
	# for (i in 1:num.plots){
	# 	plot = flexplot(rotate.view(formula, fixed.positions, which.perms=which.perms[i]), data=data)
	# 	assign(paste0("f", i, collapse=""), value = plot)
	# }
	# 
	# #### return the plot(s)
	# if (!is.null(plot)){
	# 	suppressMessages(cowplot::plot_grid(plotlist=mget(paste0("f", 1:num.plots))))	
	# } else {
	# 	suppressMessages(cowplot::plot_grid(plotlist=c(plot, (mget(paste0("f", 1:num.plots))))))	
	# }
}