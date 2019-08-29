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
##' @param color Color values to be used for the categorical variables
##' @param symbol The symbols to be used for the categorical variables
##' @param linetype The linetype to be used for the categorical variables
##' @param bins The number of bins used when putting quantitative variables into categories. A list can be used if different amounts are wanted for different variables
##' @param labels A list of the same length as bins
##' @param breaks The breaks to be used for the bins
##' @param method The method to be used to draw the lines. Defaults to loess
##' @param se Should standard errors be drawn?
##' @param ghost.line Should a ghost line be drawn? If so, user must specify a color. Default is NULL (in which case, the ghost line isn't shown). 
##' @param spread How should standard errors be drawn? Defaults to quartiles
##' @param jitter Should values be jittered?
##' @param raw.data Should raw data be plotted?
##' @param sample Should a sample of the data be plotted? Defaults to Inf (for all variables). 
##' @param ghost.reference What groups should be referenced for the ghost line? See examples. 
##' @param prediction Predicted values for a prediction line. Defaults to NULL
##' @param suppress_smooth Should a fitted line be repressed? Defaults to FALSE
##' @param alpha The transparency of the datapoints. 
##' @param data_output Should the data be outputted?
##' @author Dustin Fife
##' @import ggplot2
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
		color=NULL, symbol=NULL, linetype=NULL, 
		bins = 4, labels=NULL, breaks=NULL,
		method="loess", se=T, 
		ghost.line=NULL, ghost.reference=NULL,
		spread=c('quartiles', 'stdev', 'sterr'), jitter=FALSE, raw.data=T,
		sample=Inf, 
		prediction = NULL, suppress_smooth=F, alpha=.99977, data_output=F){
			

	##### use the following to debug flexplot
	#formula = formula(weight.loss~therapy.type + rewards); related=T; data=d; 
	#color=NULL; symbol=NULL; linetype=NULL; bins = 4; labels=NULL; breaks=NULL; method="loess"; se=T; spread=c('stdev'); jitter=FALSE; raw.data=T; ghost.line=NULL; sample=Inf; prediction = NULL; suppress_smooth=F; alpha=1


	spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
	
		#### extract outcome, predictors, and given variables
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))


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
	

		#### identify the non given variables
	axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
	axis = unlist(strsplit(axis, " + ", fixed=T))
	
		
	#### identify the correct line
	if (suppress_smooth){
		gm = theme_bw()
	} else if (method=="logistic") {

		#### make sure there's only two levels
		if (length(unique(data[,outcome]))!=2){
			stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
		}
		
		#### convert outcome to numeric (if necessary)
		if (!is.numeric(data[,outcome])){
			data[,outcome] = as.numeric(data[,outcome])-1
		}
		
		#### specify the curve
		gm = geom_smooth(method = "glm", method.args = list(family = "binomial"), se = se)
	} else if (method=="poisson" | method=="Gamma") {
		#### specify the curve
		gm = geom_smooth(method = "glm", method.args = list(family = method), se = se)
	} else {
		gm = geom_smooth(method=method, se=se)
	}


	

	#if (is.na(given)){given=NULL}
	

	

	
	### create custom function to sample data
	sample.subset = function(sample, data){
		if (sample!=Inf){
			m = data[sample(1:nrow(data), size=sample),]
		} else {
			m = data
		}
	}

	### if they don't want raw data, just make alpha = 0
	raw.alph.func = function(raw.data,alpha=1){
		if (raw.data){
			alpha.raw = alpha
		} else {
			alpha.raw = 0
		}	
	}

	if (!is.null(jitter)){
			if (jitter[1]==T & !is.numeric(jitter)[1]){
				jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=.2)
			} else if (jitter[1] == F & !is.numeric(jitter)[1]){
				jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
			} else {
				jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=jitter[1], height=jitter[2])
			}
			
		} else {
			jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
	}

	if (spread=="stdev"){
		summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.2, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="mean", position=position_dodge(width=.2))
	} else if (spread=="sterr"){	
		summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), geom='errorbar', fun.ymin = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="mean", position=position_dodge(width=.2)) 	
	} else if (spread == "quartiles"){	
		summary1 = stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="median", position=position_dodge(width=.2)) 	
	}		
		



	### BEGIN THE MEGA PLOTTING IFS!
	### PLOT UNIVARIATE PLOTS
		### if there's no predictors, use the "uni.plot" function
	if (length(outcome)==1 & length(predictors)==0){
		
		##### reorder according to columns lengths (if it's not an ordered factor)
		if (!is.numeric(data[,outcome]) & !is.ordered(data[,outcome])){
			counts = sort(table(data[,outcome]), decreasing=T)
			names(counts)
			data[,outcome] = factor(data[,outcome], levels=names(counts))
		}
		p = uni.plot(outcome, d=data)
		
	### related t plot
	} else if (related){
		if (length(predictors)!=1){
			stop("Currently, the 'related' option is only available when there's a single predictor.")
		} 
		
		#### extract levels of the predictors
		levs = levels(data[,predictors])
		
		if (length(levs)!=2){
			stop("Sorry, I can only accept two levels of the grouping variable when related=T.")
		}

		#### create difference scores
		g1 = data[data[,predictors]==levs[1], outcome]
		g2 = data[data[,predictors]==levs[2], outcome]		
		
		if (length(g1) != length(g2)){
			stop("Sorry, the length of the two groups are not the same. I can only create difference scores when the group sizes are identical.")
		}
		
		lab = paste0("Difference (",levs[2], "-", levs[1], ')')
		d2 = data.frame(Difference=g2-g1)
		
		if (spread == "stdev"){ summary2 = stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=mean, size = 1.25, width=.12, position=position_dodge(width=.2))}
		if (spread == "sterr"){ summary2 = stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.ymax = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, fun.y=mean, color=rgb(1,0,0,.25), width=.12, size = 1.25, position=position_dodge(width=.2))}
		if (spread == "quartiles"){ summary2 = stat_summary(geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.12, position=position_dodge(width=.2))}				
												#stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, color='red', width=.2)
		
		p = ggplot(d2, aes(y=Difference, x=1)) +
			geom_jitter(data=sample.subset(sample, d2), alpha=raw.alph.func(raw.data, .15), width=.05) +
			summary1 + summary2 + 
			gm +
			geom_hline(yintercept=0, col="lightgray") +
			labs(x=lab) +
			theme_bw() +
			theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
			coord_cartesian(xlim=c(.75, 1.25))
	#### SCATTERPLOT	
	} else if (length(outcome)==1 & length(predictors)==1 & is.na(given) & (is.numeric(data[,predictors]) & is.numeric(data[,outcome]))){				
		

		p = ggplot(data=data, aes_string(x=predictors, y=outcome))+
			jit + #geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
			gm +
			theme_bw()						

	##### MEAN PLOT
	} else if (length(outcome)==1 & length(predictors)==1 & is.na(given) & (is.numeric(data[,predictors]) | is.numeric(data[,outcome]))){		
		
		
		#### reorder if it's not already ordered
		if (!is.ordered(data[,predictors[1]])){
			if (spread=="quartiles"){ fn = "median"} else {fn = "mean"}
			ord = aggregate(data[,outcome]~data[,predictors], FUN=fn, na.rm=T)
			ord = ord[order(ord[,2], decreasing=T),]
			data[,predictors] = factor(data[,predictors], levels=ord[,1])
		}

		#### set default alpha
		if(alpha==.99977){
			alpha = .2
		}
		p = ggplot(data=data, aes_string(x=predictors, y=outcome)) +
			geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha), size=.75, width=.05) + 
			summary1 + summary2 + 
			theme_bw()						
	##### logistic regression plot
	# } else if (){
		
	# }
	##### CHI SQUARE PLOT

	} else if (length(outcome) == 1 & length(predictors)==1 & is.na(given) & !is.numeric(data[[outcome]]) & !is.numeric(data[[outcome]])){
		m = as.data.frame(table(data[,predictors], data[,outcome])); names(m)[1:2] = c(predictors, outcome)
		Freq = 'Freq'
		p = ggplot(data=m, aes_string(x=predictors, y=Freq, fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()

	##### INTERACTION PLOT
	} else if (length(outcome)==1 & length(predictors)==2 & (is.character(data[,predictors[1]]) | is.factor(data[,predictors[1]])) & (is.character(data[,predictors[2]]) | is.factor(data[,predictors[2]]))){


					#### set default alpha
		if(alpha==.99977){
			alpha = .2	
		}
		
				
		#### identify if given is na
		if (!is.na(given)){

			p = ggplot(data=data, aes_string(x=predictors[1], y=outcome)) +
				geom_jitter(data=sample.subset(sample, data), alpha = raw.alph.func(raw.data, alpha), size = .75, width=.2) +
				facet_wrap(as.formula(paste("~", predictors[2]))) +
				summary1 + summary2 +
				labs(x=predictors[1], y=outcome) +
				theme_bw()
		} else {
			p = ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=predictors[2], linetype=predictors[2], shape=predictors[2])) +
				geom_jitter(data=sample.subset(sample, data), alpha = raw.alph.func(raw.data, alpha), size=.75,  position= position_jitterdodge(jitter.width=.2, dodge.width=.2)) +
				summary1 + summary2 + 
				sum.line + 
				labs(x=predictors[1], y=outcome) +
				theme_bw()			
		}

	#### ANCOVA PLOT		
	# } else if (length(predictors)==2 & length(categories)==1 & length(given)<1){
		# ### if they're supplying a prediction, put the covariate in the "given" area
		# if (!is.null(prediction)){
			# given.as.string = paste0("~", categories)
			# p = ggplot(data=data, aes_string(x=numbers, y=outcome))+
				# geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
				# gm +
				# facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both)) +
				# theme_bw()	
				
		# } else {	
			# p = ggplot(data=d, aes_string(x=numbers, y=outcome, group=categories, linetype=categories, color=categories)) +
				# geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
				# gm +
				# theme_bw()							
		# }
	###### MULTIWAY DOT PLOT FOR THREE CATEGORICAL PREDICTORS
	} else if (length(outcome)==1 & length(predictors)==3 & length(categories)==3 & is.na(given)){

	
		#### figure out which variable has the largest effect
		mod = lm(formula, data=data)

		order = order(anova(mod)[-length(coef(mod)),"F value"], decreasing=F)
		ordered.predictors = row.names(anova(mod))[order]
		new.formula = make.formula(outcome, ordered.predictors)
		
		#### reorder the means
		means = aggregate(new.formula, data=data, FUN=mean)
		names(means)[ncol(means)]="mean"
		ser = aggregate(new.formula, data=data, FUN=function(x){sd(x)})
		means$ser = ser[,outcome]
		means$lower = means$mean - means$ser
		means$upper = means$mean + means$ser

		#### combine with previous dataset
		d = merge(data, means, by=ordered.predictors)
		if (se){
			p = ggplot(data=d, aes_string(x= ordered.predictors[1], y=outcome, col=ordered.predictors[1])) + 
				geom_jitter(data=sample.subset(sample, d), alpha = raw.alph.func(raw.data, .15)) +
				geom_point(aes_string(y="mean")) + 
				geom_errorbar(aes_string(ymin="lower", ymax= "upper", col=ordered.predictors), width=.2) +
				coord_flip() + 
				facet_grid(as.formula(paste0(ordered.predictors[2]," +", ordered.predictors[3],"~."))) +
				theme_bw()
		} else {
			p = ggplot(data=d, aes_string(x= ordered.predictors[1], y=outcome, col=ordered.predictors[1])) + 
				geom_jitter(data=sample.subset(sample, d), alpha = raw.alph.func(raw.data, .15)) +
				geom_point(aes_string(y="mean")) + 
				coord_flip() + 
				facet_grid(as.formula(paste0(ordered.predictors[2]," +", ordered.predictors[3],"~."))) +
				theme_bw()			
		}				
		
	##### FOR VARAIBLES THAT WILL BE BINNED...
	} else {

		##### only allow two "given" variables
		if (length(given)>2){
			stop("Only two 'given' variables are allowed.")
		}

		#### modify given (if needed)
		if (length(given)>0 & !is.na(given)){
		if (regexpr("+", given)){
			given = unlist(strsplit(given, " + ", fixed=T))
		}
		}		
		
		#### see if more than two variables are shown at the left of the given sign
		if ((length(predictors) - length(given))>2){
			stop("Only two 'axis' variables are allowed.")
		}
			
		
		
		#### make given variables ggplot friendly (for facet_grid)
		given.as.string = ifelse(length(given)>1 & !is.na(given),paste0(rev(given), collapse="~"), paste0("~",given))

		#### if a category is "given", leave it alone
		# if (length(which(given %in% categories))>0){
			# given = given[-which(given %in% categories)]
		# }


		
			
		### identify the number of binned variables we need
		if (length(axis)>1 & axis[2] %in% numbers){ 
			binned.vars = c(axis[2], numbers[which((numbers) %in% given)])
		} else{
			binned.vars = predictors[which((predictors) %in% given)]
		}
		
		#### identify which binned variables are categorical
		binned.numeric = numbers[which((numbers) %in% given)]

		if (length(binned.numeric)>0){
			msg = paste0("The following variables are going to be binned: ", paste0(binned.vars, collapse=", "), "\n")
			cat(msg)
		}
		
		### repeat the bins the number of bins there are
		if (length(bins) != length(binned.vars) & length(bins)>1){
			warning("You haven't specified enough bins to cover all the binned variables. I'm making a guess for the rest of the variables")
			bins = matrix(bins, nrow=1, ncol=length(binned.vars))
		}
		
		if (length(bins)==1){
			bins = rep(bins, times=length(binned.vars))
		}

		#### bin the binned variables
		if (length(binned.vars)>0){
			for (i in 1:length(binned.vars)){
				
				### check length of binned variables. If <= breaks, treat as categorical
				if (length(unique(data[,binned.vars[i]]))<=bins[i]){
					data[,binned.vars[i]] = factor(data[,binned.vars[i]], ordered=T)
				}

				if (is.numeric(data[,binned.vars[i]])){
					break.current = unlist(breaks[i])
					if (!is.null(unlist(labels[i])) & length(unlist(labels[i])) != bins[i]){
						stop(paste0("The label vectors (", paste0(unlist(labels[i]), collapse=", "), ") is not the same length as the bin length (", bins[i], ")", sep=""))
					}
					
					### if they supply the breaks...
					
					#if (data[,binned.vars[i]])
					if (!is.null(break.current)){
						#### give min as breaks, if the user doesn't
						if (min(break.current)>min(data[,binned.vars[i]])){
							break.current = c(-Inf, break.current)
						}
						if (max(break.current,na.rm=T)<max(data[,binned.vars[i]])){
							break.current = c(break.current, Inf)
						}
						
						quants = unlist(break.current)
					} else {
						quants = quantile(data[,binned.vars[i]], seq(from=0, to=1, length.out=bins[i]+1), na.rm=T)
						quants = quants[!duplicated(quants)]
					}
		
					data[,paste0(binned.vars[i])] = cut(data[,binned.vars[i]], quants, labels= unlist(labels[i]), include.lowest=T, include.highest=T)
	
					#### if they're making a ghost reference, bin that too
					if (!is.null(ghost.reference) & binned.vars[i] %in% names(ghost.reference)){
						val = as.numeric(ghost.reference[binned.vars[i]])
						ghost.reference[binned.vars[i]] = cut(val, quants, labels=unlist(labels[i]), include.lowest=T, include.highest=T)
					}
										
					if (!is.null(prediction)){
						prediction[,binned.vars[i]] = cut(prediction[,binned.vars[i]], quants, labels= unlist(labels[i]), include.lowest=T, include.highest=T, na.rm=T)
					}
				}

			}

			if (!is.null(prediction)){
							#### average the predictions within bin
				f = make.formula("prediction.fit", c("model",
														predictors[-which(predictors==binned.vars[i])],
														binned.vars[i]
														)
									)			
				prediction = aggregate(f, data=prediction, FUN=median, na.rm=T)

			}			
			
		}


				#### add code for "given" variable
		if (!is.na(given[1])){
			giv = facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both))
		} else {
			giv = theme_bw()
		}
		


		if (!is.null(jitter)){
			if (jitter[1]==T & !is.numeric(jitter)[1]){
				jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=.2)
			} else if (jitter[1] == F & !is.numeric(jitter)[1]){
				jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
			} else {
				jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=jitter[1], height=jitter[2])
			}
			
		} else {
			jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
		}
	
		
		if (length(axis)>1){
			
			if (!is.numeric(data[,axis[1]])){
				p = ggplot(data=data, aes_string(x=axis[1], y=outcome, shape=axis[2], linetype=axis[2], color=axis[2]))+
					gm + 
					jit + 
					giv + 
					summary1 + summary2 + 
					sum.line +
					theme_bw()
			


			} else {

			p = ggplot(data=data, aes_string(x=axis[1], y=outcome, shape=axis[2], linetype=axis[2], color=axis[2]))+
					gm + 
					jit + 
					giv + 
					theme_bw()
			}		
		} else {
			if (!is.numeric(data[,axis[1]])){
				p = ggplot(data=data, aes_string(x=axis[1], y=outcome))+
					gm + 
					jit + 
					giv + 
					summary1 + summary2 + 
					sum.line +
					theme_bw()
			


			} else {

				p = ggplot(data=data, aes_string(x=axis[1], y=outcome))+
					jit + 
					gm +
					giv + 
					theme_bw()	
				}
		
		}


		if (!is.null(ghost.line)){ # with help from https://stackoverflow.com/questions/52682789/how-to-add-a-lowess-or-lm-line-to-an-existing-facet-grid/52683068#52683068

			### quit if they try to do two axes
			if (!is.na(axis[2])){
				stop("Sorry. I can't plot a second variable on the x axis. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z)")
			}
			

					#### if they don't specify a reference group, choose one
			if (is.null(ghost.reference)){
				ghost.reference=list()
				for (b in 1:length(given)){
					l = data[1,given[b]]					
					ghost.reference[[given[b]]]=l
				}
			} 

			### make sure the reference groups are all in the data
			if (sum(names(ghost.reference) %in%  variables) != length(ghost.reference)){
				missing.var = names(ghost.reference[!(names(ghost.reference) %in% variables)])
				msg = paste0("Sorry. One of your variables (", missing.var, ") is not in your formula.")
				stop(msg)
			}



				# if (length(ghost.reference)!=length(given)){
					# stop("When referencing a 'ghost line,' you must specify the value for each 'given' variable.")
				# }
	
	
			ghost.names = names(ghost.reference)
			##### select those columns in d specified
			k = data
			for (s in 1:length(ghost.reference)){
				if (!is.numeric(data[,ghost.names[s]])){
					k = k[as.character(k[,ghost.names[s]])==unlist(ghost.reference[s]),]				
				} else {
					k = k[k[,ghost.names[s]]==unlist(ghost.reference[s]),]					
				}
			}				
		

			### identify which variables are in the given category
			ghost.given = which(ghost.names %in% given)
			if (is.numeric(k[,axis[1]])){
				g0 = ggplot(data=k, aes_string(x=axis[1], y=outcome))+gm				
			} else {
				g0 = ggplot(data=k, aes_string(x=axis[1], y=outcome))+summary1 + summary2 + 
					sum.line				
			}

			d_smooth = ggplot_build(g0)$data[[1]]; 

			### rename columns
			names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 


			## add line to existing plot   
			p = p + geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome), color=ghost.line)
			
		}		

		
	

	}	

	if (!is.null(prediction)){	
		
		#### check if first variable is a continuous predictor
		if (is.numeric(data[,predictors[1]])){
		
			p = p + geom_line(data= prediction, aes(linetype=model, y=prediction.fit, color=model), size=2)			
		} else {
			
			p = p + geom_point(data=prediction, aes(y=prediction.fit, color=model), position=position_dodge(width=.2)) + geom_errorbar(data=prediction, aes(y=NULL, ymin=lower, ymax=upper, color=model, width=.4))
		
		}
		if (data_output){
			ret = list(plot=p, data=data, gm=gm, summary1=summary1, summary2=summary2, sum.ine=sum.line)
			return(ret)
		} else {
			return(p)
			
		}			
	} else {
		if (data_output){
			ret = list(plot=p, data=data, gm=gm, summary1=summary1, summary2=summary2, sum.ine=sum.line)
			return(ret)
		} else {
			return(p)
		}
	}

}
