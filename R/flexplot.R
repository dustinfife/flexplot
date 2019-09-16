##' Create a "flexible plot" or flexplot
##'
##' Create a flexible plot 
##'	
##' Formula takes the form of y~x + a | b + z. Everything on the right of | will occur in panels (in this case, b will be in the columns and z will be in the rows). Any numeric
##' variables between ~ and | will fall on the x axis and categorical predictors will be color-coded/symbol-coded/line-coded (if user specifies). If more than one numeric variable is specified
##' after ~, the second (and third) will be binned. No more than four variables are allow (otherwise, cognitive load is too high). If the user wishes to include another variable,
##'  I recommend they create separate plots, one for each level of the new variable (or a binned level of a numeric variable). 
##' @param formula A formula of the form  y~x + a | b + z
##' @param data The dataset
##' @param related Are variables related? If so, it will show a plot of difference scores, rather than the raw scores (for a related t test). 
##' @param bins The preferred number of bins used when putting quantitative variables into categories. A list can be used if different amounts are wanted for different variables. However, sometimes flexplot cannot 
##'		have that number of bins, but it will try to make it work. 
##' @param labels A list of the same length as bins
##' @param breaks The breaks to be used for the bins.
##' @param method The method to be used to draw the lines. Defaults to loess, but it could be "lm", "logistic", "polynomial", "cubic", "rlm", or "glm".
##' @param se Should standard errors be drawn?
##' @param ghost.line Color of the ghost line. Default is NULL (in which case, the ghost line isn't shown). 
##' @param spread Specifies how the "whiskers" of the plots should be draw. Can be "quartiles", "stdev", or "sterr." Defaults to quartiles
##' @param jitter Can be either T/F, or a vector of length two, specifying the degree of jitter for the x and y axis, respectively. 
##' @param raw.data Should raw data be plotted?
##' @param sample Should a sample of the data be plotted? Defaults to Inf (for all variables). 
##' @param ghost.reference What groups should be referenced for the ghost line? See examples. 
##' @param prediction Predicted values for a prediction line. Defaults to NULL
##' @param suppress_smooth Should a fitted line be repressed? Defaults to FALSE
##' @param alpha The transparency of the datapoints. 
##' @param data_output Should the data be outputted?
##' @param silent Should all messages be suppressed? Defaults to F.
##' @param third.eye Should the "third eye" be employed? The third eye will 
##' @author Dustin Fife
##' @import tibble ggplot2
##' @export
##' @examples
#' data(exercise_data)
#' d = exercise_data
#'
#'	# # #### histograms and barcharts
##' flexplot(income~1, data=d)
##' flexplot(gender~1, data=d)
#'
#'	# ### scatter plot
##' flexplot(weight.loss~motivation, data=d)	
##' flexplot(weight.loss~motivation, data=d, method="lm", se=FALSE)	
##' 	#### with regression line and without standard error	
#'
#'	# ### mean plots
##' flexplot(weight.loss~therapy.type, data=d)
##' flexplot(weight.loss~therapy.type, data=d, raw.data=FALSE)		
##' 	## without raw data
#'
#'	# ### CHI SQUARE PLOT
##' flexplot(therapy.type~gender, data=d)	
#'			
#'	# ### INTERACTION PLOT			
##' flexplot(weight.loss~therapy.type + gender, data=d)
##' flexplot(weight.loss~therapy.type + gender, data=d, sample=50)	
##'  #### sampling 50 people instead (to make it less noisy)
#'
#'	# #### ANCOVA PLOT
##' flexplot(weight.loss~motivation + gender, data=d, se=FALSE)	### remove se
#'
#'	# #### 2N PLOT (2 NUMERIC VARIABLE PLOTS)
##' flexplot(weight.loss~motivation + income, data=d, se=FALSE, method="lm")
##' flexplot(weight.loss~motivation + income, data=d, se=FALSE, method="lm", 
##' 	breaks = list(c(95000, 100000, 105000)),
##' 	labels=list(c("<95K", "<100K", "<105K", ">105K")))		
##' 		### change labels for income
#'
#'	# #### 3N plot
##' flexplot(weight.loss~motivation + income + health, data=d, se=FALSE, method="lm")	
##' 		## different lines for income
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm")	
##' 		## different panels for income
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", 
##' 	breaks = list(c(95000, 100000, 105000)),
##' 	labels=list(c("<95K", "<100K", "<105K", ">105K")))	
##' 		## relabel income
flexplot = function(formula, data, related=F,
		bins = 3, labels=NULL, breaks=NULL,
		method="loess", se=T, 
		ghost.line=NULL, ghost.reference=NULL,
		spread=c('quartiles', 'stdev', 'sterr'), jitter=NULL, raw.data=T,
		sample=Inf, 
		prediction = NULL, suppress_smooth=F, alpha=.99977, data_output=F, silent=F,
		third.eye=NULL){
			
	#d = exercise_data
	##### use the following to debug flexplot
	#formula = formula(weight.loss~therapy.type + rewards); data=d; 
	#bins = 3; labels=NULL; breaks=NULL; method="loess"; se=T; spread=c('stdev'); jitter=NULL; raw.data=T; ghost.line=NULL; ghost.reference=NULL; sample=Inf; prediction = NULL; suppress_smooth=F; alpha=.2; related=F; silent=F; third.eye=NULL
	#data(exercise_data)
	#d = exercise_data
	#formula = formula(weight.loss~rewards+gender|income+motivation); data=d; 
	#ghost.reference = list(income=90000)


	#### create an empty plot to avoid the 'Error in UseMethod("depth") : no applicable method for 'depth' applied to an object of class "NULL"' error
	df = data.frame()
	ggplot2::ggplot(df) + ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, colour = NA))
	
	### if they supply tibble, change to a data frame (otherwise the referencing screws things up)
	if (tibble::is_tibble(data)){
		data = as.data.frame(data)
	}
	

	
	

	spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))


		
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	
	
	#### make sure all names are in the dataset
	if (!all(variables %in% names(data))){
		not.there = variables[which(!(variables %in% names(data)))]
		stop(paste0("So...we've got a lil' problem. You specified one or more variable in the formula that is not in your dataset (specifically ", paste0(not.there, collapse=", "), "). Let's get that fixed and try again.\n"))
	}
	
		# #### extract outcome, predictors, and given variables
	# if (!is.null(third.eye) & length(predictors)>2){
		# formula = rotate.view(formula = formula, third.eye= third.eye)
		# variables = all.vars(formula)
		# outcome = variables[1]
		# predictors = variables[-1]		
	# }
		
	given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))
	given = gsub(" ", "", given)		
	given = unlist(strsplit(given, "+", fixed=T))	
	axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
	axis = gsub(" ", "", axis)			
	axis = unlist(strsplit(axis, "+", fixed=T))	

	#### identify which variables are numeric and which are factors
	if (length(predictors)>0){
		numbers = names(which(unlist(lapply(data[,predictors], is.numeric))))
		categories = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))
	}

		### remove missing values
	if (length(predictors)>0){
		if (length(unlist(apply(data[,variables], 2, function(x){(which(is.na(x)))})))>0){
			delete.me = as.numeric(unlist(apply(data[,variables], 2, function(x){(which(is.na(x)))})))
			data = data[-delete.me,]
		}
	}
		
	#### get the breaks for the needed variables
	break.me = predictors[-1][unlist(lapply(data[,predictors[-1]], FUN=is.numeric)) & ((predictors[-1] %in% given) | (axis[2] %in% predictors[-1]))]	

	#### did they provide a named break?
	if (!is.null(breaks)) {
		named.breaks = names(breaks)
	} else {
		named.breaks = NA
	}	

		#### create a list of breaks
	if (length(break.me)>0 ){
		
		#### bark at them if they forgot to name their breaks
		if (is.null(named.breaks)){	
			stop("You must name your breaks if you provide them. Be sure to do that. (e.g., breaks = list(variable1=c(5, 10, 15)), variable2=c(0,1,2))")
		
		#### make sure they spelled breaks right and such
		} else if (!is.na(named.breaks) & !(named.breaks %in% break.me)){
			stop("I can't find ", named.breaks, " in your list of variables to be binned (", paste0(break.me, collapse=","), "). Did you spell everything right?")
		}
		
		#### make an empty list if they don't provide breaks
		if (is.null(breaks)){
			breaks = rep(list(NULL),length(break.me))
			#names(breaks) = break.me
		}


			#### now make the breaks and convert the data
		for (i in 1:length(break.me)){

			if ((i-1)<length(breaks)){
				#### if they don't name the breaks, bark at them
				if (is.null(names(breaks)[[i]]) & is.null(named.breaks)){
					stop("It looks like you forgot to name your breaks. Be sure to do that. (e.g., breaks = list(variable1=c(5, 10, 15)), variable2=c(0,1,2))")
				}
			}
						
			breaks[[break.me[i]]] = prep.breaks(variable=break.me[i], data, breaks=breaks[[break.me[i]]], bins)
		}
	} 

	
	#### if they only have a few levels on the x axis, jitter convert it to categorical
	if (length(predictors)>0){
		if (is.numeric(data[,axis[1]]) & length(unique(data[,axis[1]]))<5){
			data[,axis[1]] = factor(data[,axis[1]], ordered=T)
		}
		### do the same for the second axis
		if (length(axis)>1){
		if (is.numeric(data[,axis[2]]) & length(unique(data[,axis[2]]))<5){
			data[,axis[2]] = factor(data[,axis[2]], ordered=T)
		}		
		}
	}
	

	### BEGIN THE MEGA PLOTTING IFS!
	
	
	### PLOT UNIVARIATE PLOTS
	if (length(outcome)==1 & length(predictors)==0){

		##### reorder according to columns lengths (if it's not an ordered factor)
		if (!is.numeric(data[,outcome]) & !is.ordered(data[,outcome])){
			counts = sort(table(data[,outcome]), decreasing=T)
			names(counts)
			data[,outcome] = factor(data[,outcome], levels=names(counts))
		}
		
		
		### figure out how many levels for the variable
		levels = length(unique(data[,outcome]))	
		
		#### if numeric, do a histogram
		if (is.numeric(data[,outcome])){
			p = 'ggplot(data=data, aes_string(outcome)) + geom_histogram(fill="lightgray", col="black", bins=min(30, round(levels/2))) + theme_bw() + labs(x=outcome)'
		} else {
			p = 'ggplot(data=data, aes_string(outcome)) + geom_bar() + theme_bw() + labs(x= outcome)'		
		} 
		points = "xxxx"
		fitted = "xxxx"		

	### BIVARIATE PLOTS
	} else if (length(outcome)==1 & length(axis)==1 & !related){
		
		#### if both are categorical, do chi square
		if (!is.numeric(data[[outcome]]) & !is.numeric(data[[outcome]])){
			m = as.data.frame(table(data[,axis], data[,outcome])); names(m)[1:2] = c(axis, outcome)
			Freq = 'Freq'
			p = "ggplot(data=m, aes_string(x=axis, y=Freq, fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()"
			points = "xxxx"
			fitted = "xxxx"
		} else {

			### reorder axis and alter default alpha if categorical
			if (!is.numeric(data[,axis])){

				#### reorder if it's not already ordered
				if (!is.ordered(data[, axis[1]])){
					if (spread=="quartiles"){ fn = "median"} else {fn = "mean"}
					ord = aggregate(data[,outcome]~data[, axis], FUN=fn, na.rm=T)
					ord = ord[order(ord[,2], decreasing=T),]
					data[,axis] = factor(data[, axis], levels=ord[,1])
				}
		
				#### set default alpha
				if(alpha==.99977){
					alpha = .2
				}		
			}

			p = 'ggplot(data=data, aes_string(x=axis, y=outcome))'
			points = points.func(axis.var=axis, data=data, jitter=jitter)
			fitted = fit.function(outcome, axis, data=data, suppress_smooth=suppress_smooth, method=method, spread=spread)		

		}	
	
	### RELATED T-TEST
	} else if (related){		
		
		
		#### extract levels of the predictors
		levs = levels(data[,axis[1]])

		#### create difference scores
		g1 = data[data[, axis[1]]==levs[1], outcome]
		g2 = data[data[, axis[1]]==levs[2], outcome]				

		
		### error checking
		if (length(predictors)!=1){
			stop("Currently, the 'related' option is only available when there's a single predictor.")
		} 
		
		if (length(levs)!=2){
			stop("Sorry, I can only accept two levels of the grouping variable when related=T.")
		}
		
		if (length(g1) != length(g2)){
			stop("Sorry, the length of the two groups are not the same. I can only create difference scores when the group sizes are identical.")
		}
		
		lab = paste0("Difference (",levs[2], "-", levs[1], ')')
		d2 = data.frame(Difference=g2-g1)
		
		
		p = "ggplot(d2, aes(y=Difference, x=1)) + theme_bw()+ geom_hline(yintercept=0, col='lightgray') + labs(x=lab) + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())"
	
		#### modify default jitter
		if (is.null(jitter)){
			jitter = c(.05, 0)
		} 
		points = points.func(axis.var="Difference", data=d2, jitter=jitter)
		fitted = paste0(fit.function(outcome, "Difference", data=d2, suppress_smooth=suppress_smooth, method=method, spread=spread, categorical=T), " + coord_cartesian(xlim=c(.75, 1.25))")

	##### if they have two axis variables
	} else if (length(axis)>1){

		#### if the second variable is numeric, bin it
		if (is.numeric(data[,axis[2]])){
			binned.name = paste0(axis[2], "_binned")
			data[[binned.name]] = bin.me(axis[2], data, bins, unlist(labels), breaks[[axis[2]]])
			axis[2] = binned.name
		}
		
		### if they supply predictions, do not vary color
		if (!is.null(prediction)){
			p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2], shape=axis[2])) + labs(color=predictors[2], shape=predictors[2])'
		} else {
			p = 'ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=axis[2], linetype = axis[2], shape=axis[2])) + labs(color=predictors[2], linetype=predictors[2], shape=predictors[2])'
		}
		
		points = points.func(axis.var=axis[1], data=data, jitter=jitter)
		fitted = fit.function(outcome, predictors=axis[1], data=data, suppress_smooth=suppress_smooth, method=method, spread=spread, mean.line=TRUE)
	}

	#### all the above should take care of ALL possible plots, but now we add paneling
	
	#### add panels (if they were specified)

	if (!is.na(given[1])){
		for (i in 1:length(given)){

			binned.name = paste0(given[i], "_binned")

			if (is.numeric(data[,given[i]])){
				data[,binned.name] = bin.me(given[i], data, bins, labels[i], breaks[[given[i]]])

				
				### reorder levels of bin 2
				if (i==2){
					data[,binned.name] = forcats::fct_rev(data[,binned.name])
				}
			} else {
				### duplicate categorical variables and give a new name for binned ones
				data[,binned.name] = data[,given[i]]
			}
		}

		#### prep the given variables to be stringed together
		given2 = given
		if (length(break.me)>0){
			given2[given2%in%break.me] = paste0(given2[given2%in%break.me], "_binned")
		}	
		given.as.string = ifelse(length(given)>1 & !is.na(given2[1]),paste0(rev(given2), collapse="~"), paste0("~",given2))
		
		#### make a custom labeller that removes "_binned"
		custom.labeler = function(x){
			lapply(names(x),function(y){
			paste0(gsub("_binned", "", y),": ", x[[y]])
			})
		}
		facets = paste0('facet_grid(as.formula(', given.as.string, '),labeller = custom.labeler)')			
	} else {
		facets = "xxxx"
	}


	

	if (!is.null(ghost.line)){ # with help from https://stackoverflow.com/questions/52682789/how-to-add-a-lowess-or-lm-line-to-an-existing-facet-grid/52683068#52683068

		### quit if they try to do two axes
		if (!is.na(axis[2])){
			stop("Sorry. I can't plot ghost lines when there are already lines in the plot. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z)")
		}

				### bin the ghost reference if it's not null
		if (!is.null(ghost.reference)){
			for (i in 1:length(given)){

			binned.name = paste0(given[i], "_binned")
			### if the ghost reference is null, randomly fill something in
			if (is.null(ghost.reference[[given[i]]])){
				rand.val = sample(1:nrow(data), 1)
				ghost.reference[[given[i]]] = data[rand.val,binned.name]
				#location = which(names(ghost.reference)==given[i])
				#names(ghost.reference)[location] = binned.name
				if (!silent){cat(paste0("Note: You didn't choose ghost.reference values for ", given[i], ". I'm going to choose one at random.\n"))}
			#### when they supply ghost reference information
			} else {
				#k[nrow(k), given[i]] = as.numeric(ghost.reference[[given[i]]])
				#k[,binned.name] = bin.me(given[i], k, bins, labels[i], brks)

				if (is.numeric(ghost.reference[[given[i]]])){
					#brks = bin.me(given[i], data, bins, labels[i], breaks[[given[i]]], return.breaks=T)
					ghost.reference[[given[i]]] = bin.me(variable=given[i], data=ghost.reference, bins=bins, labels=labels[i], breaks=breaks[[given[i]]], check.breaks=F)
				} else {
					ghost.reference[[given[i]]] = unlist(ghost.reference)
				}
	
				#location = which(names(ghost.reference)==given[i])
				#names(ghost.reference)[location] = binned.name						
				}
			}
		} else {
						
			#### if they don't specify any reference group, choose one

			ghost.reference=list()
			for (b in 1:length(given)){
				given.bin = paste0(given[b], "_binned")
				### format given as a binned variable
				l = data[,given.bin]

				ghost.reference[[given[b]]]=sample(l, 1)
			}

			if (!silent){cat(paste0("Note: You didn't specify a reference for the ghost line. I'm going to choose it at random.\n"))}

		#### if they specify a reference group for some of them, but not all
		}
		
		
		##### select those columns in d specified
		k = data
		s=1
		for (s in 1:length(given)){
			binned.name = paste0(given[s], "_binned")
			k = k[(k[,binned.name])==unlist(ghost.reference[[given[s]]]),]				

		}				

		#### is k gone???
		if (nrow(k)==0){
			stop("there was an error in generating the ghost line. Please email the developer: fife.dustin@gmail.com")
		}


		### create ggplot object to extract the fit for the ghost line
		g0 = paste0('ggplot(data=k, aes_string(x=axis[1], y=outcome))+', fitted)				
		g0 = gsub("+xxxx", "", g0, fixed=T)
		g0 = eval(parse(text=g0))
		d_smooth = ggplot_build(g0)$data[[1]]; 

		### rename columns
		names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 


		## add line to existing plot   
		ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome), color=ghost.line)'
		
	} else {
		ghost = "xxxx"
	}	


	### add prediction lines
	if (!is.null(prediction)){
	
		### see how many models are being compared
		num.models = unique(prediction$model)
	
		if (!is.na(axis[2]) & length(num.models)>1){
			stop("Sorry. I can't plot the model(s) lines when there are already lines in the plot. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z), or choose to display only one model")
		}


		#### bin the predictions, where needed
		if (length(break.me)>0){
			for (i in 1:length(break.me)){
				### find that variable in the model and bin it
				prediction[[break.me[i]]] = bin.me(break.me[i], prediction, bins, labels[i], breaks[[break.me[i]]])
				
				### bin it if it's binned in the original

				names(prediction)[names(prediction)==break.me[i]] = paste0(break.me[i], "_binned")
			}
							### now average fit within bin
				groups = c("model", paste0(break.me, "_binned"), predictors[-which(predictors%in%break.me)])
				prediction = prediction %>% group_by_at(groups) %>% summarize(prediction = mean(prediction)) %>% as.data.frame

		}

		#### check if first variable is a continuous predictor
		if (is.numeric(data[[predictors[1]]])){
					
			##### if they specify an axis[2], modify the "fitted" string
			if (!is.na(axis[2])){
				pred.line = 'geom_line(data= prediction, aes_string(linetype=axis[2], y="prediction", colour=axis[2]), size=1)' 				
				fitted = "xxxx"
			} else {
				pred.line = 'geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1) + scale_linetype_manual(values=c("solid", "dotdash"))' 				
			}
			                         
		} else {

			pred.line = 'geom_point(data=prediction, aes(y=prediction, color=model), position=position_dodge(width=.2)) + geom_line(data=prediction, aes(y=prediction, linetype=model, group=model, color=model), position=position_dodge(width=.2))'

		}
		
		
		#### remove linetype from the plot
		
		
	} else {
		pred.line = "xxxx"
	}

	##### do third eye if they choose to
	#if (third.eye)

	theme = "theme_bw()"

	#### evaluate the plot

	total.call = paste0(p, "+",points, "+",fitted, "+", facets, "+", ghost, "+", pred.line, "+", theme)
	### remove +xxxx (happens when I've made an element blank)
	total.call = gsub("+xxxx","",total.call, fixed=T)
	final = eval(parse(text=total.call))
	
	return(final)
}	