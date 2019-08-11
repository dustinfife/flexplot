#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @import ggplot2 
#' @export
visualize = function(object, plot=c("all", "residuals", "bivariate"),formula=NULL,...){
	UseMethod("visualize")
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @export
visualize.default = function(object, plot=c("all", "residuals", "bivariate"),formula=NULL,...){
	class(object) = "visualize"
	plot(object)
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param ... Other arguments passed to flexplot
#' @export
visualize.lm = function(object, plot=c("all", "residuals", "bivariate"), formula = NULL,...){
	

	plot = match.arg(plot, c("all", "residuals", "bivariate"))
	
	
	terms = attr(terms(object), "term.labels")
	
	#### get dataset
	d = object$model
	
	#### identify factors
	if (length(terms)>1){
		factors = names(which(unlist(lapply(d[,terms], is.factor))));
		numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
	} else {
		factors = terms[which(is.factor(d[,terms]))]
		numbers = terms[which(is.numeric(d[,terms]))]
	}

		#### figure out what is numeric
	levels = apply(d, 2, FUN=function(x) length(unique(x)))
	#### if there's too few levels and it's not categorical
	factors = !sapply(d, is.factor)
	if (any(levels<5 & factors)){
		cat("Note: one or more of your variables has less than 5 values, yet they're treated as numeric.\n\n")
	}
	
	#### extract names
	x.names = names(d)[-1] 
	y.name = names(d)[1]
	
	#### export residuals
	d$residuals = residuals(object)
	d$abs.res = abs(d$residuals)
	d$fitted = fitted(object)

	#### plot residuals
	histo = ggplot2::ggplot(data=d, aes(x=residuals)) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x="Residuals", title="Histogram of Residuals")
	if (length(numbers)>0){
		#res.dep = ggplot2::ggplot(data=d, aes(x=fitted, y=residuals)) + geom_point() + geom_smooth(method="loess", se=F) + 
		#theme_bw() + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
		res.dep = flexplot(residuals~fitted, data=d) + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
		
	} else {
		res.dep = NULL
	}
	if (length(unique(d$fitted))<7){
	sl = flexplot(abs.res~fitted, data=d, method="lm", jitter=c(.2, 0))+ labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")			
	} else {
	sl = flexplot(abs.res~fitted, data=d, method="lm")+ labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")			
	}
	#sl = flexplot(abs.res~fitted, data=d, method="lm")+ labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")	#ggplot2::ggplot(data=d, aes(y=abs.res, x=fitted)) + geom_point(alpha=.5, size=.75) + geom_smooth(method= "loess") +
			#theme_bw() + labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")	
	
	
	#### use flexplot to visualize
	if ((plot=="all" | plot == "bivariate" ) & is.null(formula)){
		
		#### generate formula as best we can
		#### get dataset
		data = object$model
		
		#### extract the terms from each model
		terms = attr(terms(object), "term.labels")
	
			
		##### extract variable names
		variables = all.vars(formula(object))
	    outcome = variables[1]
	    predictors = variables[-1]
	    
	    ##### look for interactions and remove them
		if (length(grep(":", terms))>0){
			terms = terms[-grep(":", terms)]
		}
		
		##### look for polynomials and remove them
		if (length(grep("^2", terms, fixed=T, value=T))>0){
			terms = terms[-grep("^2", terms, fixed=T)]
		}
	
		#### figure out variable types
		if (length(terms)>1){
			numb = names(which(unlist(lapply(data[,terms], is.numeric))))
			cat = names(which(!(unlist(lapply(data[,terms], is.numeric)))))
		} else {
			numb = ifelse(is.numeric(data[,terms]), terms, NA)
			cat = ifelse(is.factor(data[,terms]), terms, NA)		
		}
	    
		#### now decide where things go
		if (length(terms)>4){
			cat("Note: I can't plot more than four variables")
		} 
		
		#### if both numeric and factor, put numeric on x axis and factor as color/line
		if ((!is.na(cat[1]) | length(cat[1])!=0) & (!is.na(cat[1]) | length(cat[1])!=0)){
			### remove terms with first numb and first cat
			t2 = terms[-which(terms==numb[1] | terms==cat[1])]
			t2 = c(numb[1],cat[1], t2)
		#### otherwise, if length is greater than 
		} else {
			t2 = terms[1:min(length(terms), 4)]
		}
	
		#### now create formula
		x = c(outcome, "~",t2[1], t2[2], "|", t2[3], t2[4])
		x = x[-which(is.na(x))]
		x = paste0(x, collapse="+")
		x = gsub("+|+", "|", x, fixed=T);x = gsub("+~+", "~", x, fixed=T)
		x = gsub("+|", "", x, fixed=T)
		f = as.formula(x)		

		step3 = flexplot(f, data=data, ...)
		
	} else if (plot=="all" | plot=="bivariate"){
		step3 = flexplot(formula, data=d, ...)
	}

	#### return the plots
	if (plot=="bivariate"){
		return(step3)
	} else if (plot=="residuals"){
		if (length(numbers)>0){
			top.row = cowplot::plot_grid(histo, res.dep,ncol=2)
			bottom.row = cowplot::plot_grid(NULL, sl, NULL, ncol=3, rel_widths=c(.25, .5, .25))
			cowplot::plot_grid(top.row, bottom.row, ncol=1)
		} else {
			cowplot::plot_grid(histo, sl, ncol=1)
		}
	} else {
		cowplot::plot_grid(step3, histo, res.dep, sl)
}
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param ... Other arguments passed to flexplot
#' @param formula A flexplot-style formula
#' @export
visualize.lmerMod = function(object, plot=c("residuals", "all", "bivariate"), formula=NULL, ...){
	

	plot = match.arg(plot, c("all", "residuals", "bivariate"))
	#### figure out what is numeric
	d = object@frame
	levels = apply(d, 2, FUN=function(x) length(unique(x)))
	
	#### if there's too few levels and it's not categorical
	factors = !sapply(d, is.factor)
	if (any(levels<5 & factors)){
		cat("Note: one or more of your variables has less than 5 values, yet they're treated as numeric.\n\n")
	}
	
	#### extract names
	x.names = names(d)[-1] 
	y.name = names(d)[1]
	
	#### export residuals
	d$residuals = residuals(object)
	d$abs.res = abs(d$residuals)
	d$fitted = fitted(object)
	
	#### plot residuals
	histo = ggplot2::ggplot(data=d, aes(x=residuals)) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x="Residuals", title="Histogram of Residuals")
	res.dep = ggplot2::ggplot(data=d, aes(x=fitted, y=residuals)) + geom_point(alpha = .35, size=.75) + geom_smooth(method="loess") + 
		theme_bw() + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
	sl = ggplot2::ggplot(data=d, aes(y=abs.res, x=fitted)) + geom_point(alpha=.35, size=.75) + geom_smooth(method= linetype) +
			theme_bw() + labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")	
	

	#### use flexplot to visualize
	if (plot=="all" | plot=="bivariate" & is.null(formula)){
		warning("You must provide a formula argument to plot the data. I'm just returning the residual plots.")
	} else if (plot=="all"){
		step3 = flexplot(formula, data=d, ...)
	}

	#### return the plots
	if (plot=="bivariate"){
		return(step3)
	} else if (plot=="residuals"){
		top.row = plot_grid(histo, res.dep,ncol=2)
		bottom.row = plot_grid(NULL, sl, NULL, ncol=3, rel_widths=c(.25, .5, .25))
		cowplot::plot_grid(top.row, bottom.row, ncol=1)
	} else {
		cowplot::plot_grid(step3, histo, res.dep, sl)
	}
}
