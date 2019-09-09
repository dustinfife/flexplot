##' Plot univariate distribution of a variable
##'
##' A function that automatically plots either a barchart or a histogram (depending on data type)
##'	
##' Ideally, the user will provide the data type for this function (by saying numeric = T, or numeric = F)
##' Omitting that, the function will decide for the user. If the variable is coded as numeric AND there are more than five unique values of the variable,
##' it will generate a histogram. If the variable is a factor OR it has five or less unique values, it will output a barchart. 
##' @param variable A string indicating the name of the variable to plot
##' @param d The name of the dataset (which contains the variable the user wishes to plot)
##' @param numeric Logical. Is the variable of interest numeric? (Meaning, should a histogram be plotted?)
##' @return A plot
##' @author Dustin Fife
##' @export
##' @import ggplot2
##' @examples
##' distress = sample(1:10, size=22, replace=T)
##' uni.plot(distress, numeric=T, d=NULL)
uni.plot = function(variable, d=NULL, numeric=NULL){
	
	#### first try to find the variable
		if (is.null(d) & is.character(variable)){
			stop("You must specify a dataset if you surround the variable name in quotes")
		} else if (!is.null(d)){
			#### try finding that variable in the dataset
			tryCatch(d[,variable], error=function(error){paste0("I couldn't find '", variable, "' in your dataset")})
			out = tryCatch(length(d[,variable])>0, error=function(error){paste0("I couldn't find '", variable,"' in your dataset"); return(FALSE)})			
			if (!out){
				stop(paste0("I couldn't find either '", x, "' or '", y , "' in your dataset"))
			}			
		} else if (is.null(d[,variable]) & !is.character(variable)){
			d = data.frame(variable); names(d) = deparse(substitute(variable))
			variable = deparse(substitute(variable))
		}



	levels = length(unique(d[,variable]))	
		
	#### specify conditions
	if (!is.null(numeric)){
		if (numeric==T){
			condition = "numeric"
		} else if (numeric == F){
			condition = "categorical"
		} 
	} else {
		if (is.numeric(d[,variable]) & levels>5){
			condition = "numeric"
		} else if (is.factor(variable)){
			condition = "categorical"
		} else {
			condition = "categorical"
		}
	}
		

		#### if they specified a numeric variable...
	if (condition == "numeric"){
		p = ggplot(data=d, aes_string(variable)) + geom_histogram(fill="lightgray", col="black", bins=min(30, round(levels/2))) + theme_bw() + labs(x=variable)

		
		### now create the code that created it
		output = paste0("R Code to generate plots: \n\n ggplot(data=", deparse(substitute(d)), ", aes(", variable, ")) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x='", variable, "')")
		#cat(output)

		return(p)

	} else {
		p = ggplot(data=d, aes_string(variable)) + geom_bar() + theme_bw() + labs(x=variable)
		output = paste0("R Code to generate plots: \n\n ggplot(data=", deparse(substitute(d)), ", aes(", variable, ")) + geom_bar() + theme_bw() + labs(x='", variable, "')")
		#cat(output)		
		return(p)			
	} 

}