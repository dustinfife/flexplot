#bins = 3; labels=NULL; breaks=NULL; method="loess"; se=T; spread=c('stdev'); jitter=NULL; raw.data=T; ghost.line=NULL; ghost.reference=NULL; sample=Inf; prediction = NULL; suppress_smooth=F; alpha=.2; related=F; silent=F; third.eye=NULL
##' Create a "flexible plot" or flexplot
##'
##' Create a flexible plot. Flexplot will allow users to create histograms, barcharts, jittered density plots, paneled plots, scatterplots, etc.  
##' Much of the decision-making is automated, which is pretty freaking awesome. 
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
##' @param method The method to be used to draw the lines. Defaults to loess, but it could be "lm", "logistic", "polynomial", "cubic", "rlm", or "glm". Although you could always try something else. Sometimes
##' flexplot possesses superhuman intelligence. 
##' @param se Should standard errors be drawn? Defaults to T. 
##' @param ghost.line Color of the ghost line. Default is NULL (in which case, the ghost line isn't shown). 
##' @param spread Specifies how the "whiskers" of the plots should be draw. Can be "quartiles", "stdev", or "sterr." Defaults to quartiles
##' @param jitter Can be either T/F, or a vector of length two, specifying the degree of jitter for the x and y axis, respectively. 
##' @param raw.data Should raw data be plotted? Defaults to yeaaaaahhhhh-buddy! (i.e., T)
##' @param sample Should a sample of the data be plotted? Defaults to NOT sampling (for all variables). 
##' @param ghost.reference What groups should be referenced for the ghost line? See examples for how to specify this. 
##' @param prediction Predicted values for a prediction line. Defaults to NULL. This is generally NOT used when just using flexplot, but it's helpful with the wrapper function \link{compare.fits}
##' @param suppress_smooth Should a fitted line be repressed? Defaults to FALSE
##' @param alpha The transparency of the datapoints. 
##' @param plot.string Should the code to generate the plot be outputted? In the background, flexplot is creating a bunch of strings before it evaluates the model. 
##' Users can export the string, in case they want to modify the ggplot object
##' @param silent Should all messages be suppressed? Defaults to F.
##' @param third.eye Should the "third eye" be employed? The third eye will be implemented shortly. 
##' @author Dustin Fife
##' @import tibble ggplot2 R6
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
##' \dontrun{flexplot(weight.loss~therapy.type + gender, data=d)
##' flexplot(weight.loss~therapy.type + gender, data=d, sample=50)	
##'  #### sampling 50 people instead (to make it less noisy)
#'
#'	# #### ANCOVA PLOT
##' flexplot(weight.loss~motivation + gender, data=d, se=FALSE)	### remove se
#'
#'	# #### 2N PLOT (2 NUMERIC VARIABLE PLOTS)
##' flexplot(weight.loss~motivation + income, data=d, se=FALSE, method="lm")
##' flexplot(weight.loss~motivation + income, data=d, se=FALSE, method="lm", 
##' 	breaks = list(income = c(95000, 100000, 105000)),
##' 	labels=list(income = c("<95K", "<100K", "<105K", ">105K")))		
##' 		### change labels for income
#'
#'	# #### 3N plot
##' flexplot(weight.loss~motivation + income + health, data=d, se=FALSE, method="lm")	
##' 		## different lines for income
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm")	
##' 		## relabel income
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red",
##' 	breaks = list(income = c(95000, 100000, 105000)),
##'  	labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", 
##' 	ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", 
##' 	ghost.line="red", ghost.reference=list("health"=31))
##' flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", 
##' 	ghost.line="red", ghost.reference=list("health"=31, "income"=90000, "motivation"=10))}	
flexplot = function(formula, data=NULL, related=F,
		bins = 3, labels=NULL, breaks=NULL,
		method="loess", se=NULL, 
		ghost.line=NULL, ghost.reference=NULL,
		spread=c('quartiles', 'stdev', 'sterr'), jitter=NULL, raw.data=T,
		sample=Inf, 
		prediction = NULL, suppress_smooth=F, alpha=.99977, plot.string=F, silent=F,
		third.eye=NULL){
			
	#d = exercise_data
	##### use the following to debug flexplot
	#formula = formula(weight.loss~therapy.type + rewards); data=d; 
	#bins = 3; labels=NULL; breaks=NULL; method="loess"; se=T; spread=c('stdev'); jitter=NULL; raw.data=T; ghost.line=NULL; ghost.reference=NULL; sample=Inf; prediction = NULL; suppress_smooth=F; alpha=.2; related=F; silent=F; third.eye=NULL
	#data(exercise_data)
	#d = exercise_data
	#formula = formula(weight.loss~rewards+gender|income+motivation); data=d; 
	#ghost.reference = list(income=90000)


	if (is.null(data)){
		stop("Howdy! Looks like you forgot to include a dataset! Kinda hard to plot something with no data. Or so I've heard. What do I know? I'm just a computer. ")
	}

	##### make models into a factor if they supply predictions
	if (!is.null(prediction)){
		prediction$model = factor(prediction$model)
	}
	
	### if they supply tibble, change to a data frame (otherwise the referencing screws things up)
	if (tibble::is_tibble(data)){
		data = as.data.frame(data)
	}

	spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
	
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	
  ### extract given and axis variables
	given.axis = flexplot_axis_given(formula)
	given = given.axis$given
	axis = given.axis$axis
	
	### make sure all names are in the dataset
	flexplot_errors(variables = variables, data = data, method=method, axis=axis)
	
	### change se based on how many variables they have
	if (is.null(se)){
	  if (length(predictors)==1){
	    se=T
	  } else {
	    se = F
	  }
	}

	#### identify which variables are numeric and which are factors
	vtypes = variable_types(predictors, data, return.names=T)
	numbers = vtypes$numbers
	categories = vtypes$characters
	levels = length(unique(data[,outcome]))	### necessary for univariate plots

		### remove missing values
	data = flexplot_delete_na(data, predictors, variables)

	  ### create the lists that contain the breaks
  break.me = flexplot_break_me(data, predictors, given)
  breaks = flexplot_create_breaks(break.me = break.me, breaks, data, labels)
	
    ### convert variables with < 5 categories to ordered factors
  data = flexplot_convert_to_categorical(data, axis)
	


	### PLOT UNIVARIATE PLOTS
  bivariate = flexplot_bivariate_plot(outcome=outcome, predictors=predictors, axis=axis, 
                                      related=related, labels=labels, bins=bins, breaks=breaks, 
                                      data=data, jitter=jitter, suppress_smooth=suppress_smooth, method=method, spread=spread, alpha=alpha, prediction=prediction)
    p = bivariate$p
    points = bivariate$points
    fitted = bivariate$fitted
    data = bivariate$data
    prediction = bivariate$prediction
    alpha = bivariate$alpha

	#### all the above should take care of ALL possible plots, but now we add paneling
  panels = flexplot_panel_variables(outcome, predictors, axis, given, related, labels, bins, breaks, data, suppress_smooth=suppress_smooth, method=method, spread=spread, prediction=prediction, break.me=break.me)
    facets = panels$facets
    data = panels$data
    prediction = panels$prediction


	

	if (!is.null(ghost.line) & !is.na(given[1])){ # with help from https://stackoverflow.com/questions/52682789/how-to-add-a-lowess-or-lm-line-to-an-existing-facet-grid/52683068#52683068

				### bin the ghost reference if it's not null
    ghost.reference = create_ghost_reference(ghost.reference, given, axis, data, bins, labels, breaks)
		
		#### if they specified a prediction, extract data from prediction model
		if (!is.null(prediction)){
			k = prediction
		} else {
			k = data
		}
		s=1

		#### norrow down based on GIVEN, not the axis (because we're trying to find the right panel, then plot only the one group line)
		for (s in 1:length(given)){
			binned.name = paste0(given[s], "_binned")

			### specify k based on whether they supply a prediction
			if (is.null(prediction)){
				k = k[(k[,binned.name])==unlist(ghost.reference[[given[s]]]),]				
			} else {
				rows = which(k[,binned.name]==unlist(ghost.reference[[given[s]]]))
				k = k[rows,]

				names(k)[names(k)=="prediction"] = outcome

			}

		}

    #### is k gone???
		if (nrow(k)==0){
			stop("there was an error in generating the ghost line. Please email the developer: fife.dustin@gmail.com")
		}


		### create ggplot object to extract the fit for the ghost line
		if (!is.null(prediction)){
			
			#### ghost line needs a FITTED LINE, otherwise it generates weird zigzags. When you do a prediction line with no fitted line, that's a problem.
			#### each given variable still has multiple entries, so average across those entires
			
			#### average within the binned values
			grouped.vars = c("model", predictors[(!(predictors %in% given))])
			k = k %>% group_by_at(vars(one_of(grouped.vars))) %>% summarize_at(.vars = outcome, .funs=mean)
			g0 = paste0('ggplot(data=k, aes_string(x=axis[1], y=outcome, linetype="model", group="model"))+', fitted)							
		} else {
			g0 = paste0(gsub("data=[[:alnum:]]+,", "data=k,", p), "+",fitted)
			#g0 = paste0('ggplot(data=k, aes_string(x=axis[1], y=outcome))+', fitted)
							
		}
		g0 = gsub("+xxxx", "", g0, fixed=T)
		
		#### if they have a logistic, modify the p to reflect the data
		if (method=="logistic" & !is.numeric(k[,outcome])){
		  g0 = gsub("data=[[:alnum:]]+,", "data=factor.to.logistic(k,outcome),", g0)
		}
		
		g0 = eval(parse(text=g0))		
		d_smooth = suppressMessages(ggplot_build(g0)$data[[1]])


		### rename columns
		names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 

		## add line to existing plot 
		if (!is.null(prediction) & length(levels(prediction$model))>1){  
			d_smooth$model = factor(d_smooth$group, labels=levels(prediction$model))
			ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group="model", linetype="model"), color=ghost.line, show.legend=F)'			
		} else if (length(axis)>1){	
			#### used to be factoring d_smooth$group, but that gave different groups for each color AND line, so just making it line now
		  
		  #### if they specify BOTH references, allow it. Otherwise, pick one
		  if (length(unique(d_smooth$linetype))==1){
		    d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=ghost.reference[[axis[2]]])
		  } else {
		    d_smooth[,axis[2]] = factor(d_smooth$linetype, labels=sort(levels(factor(k[[axis[2]]]))))
		  }
		  
		      ### it seems ggplot is choosing the order based on sorting
	  
			### if the ghost line specifies a specific line to plot...
			axis2_notbinned = gsub("_binned", "",axis[2])
			if (axis2_notbinned %in% names(ghost.reference)){
				d_smooth = d_smooth[d_smooth[,axis[2]]==(ghost.reference[[axis2_notbinned]]),]
				ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group=axis[2], linetype=axis[2]), color=ghost.line, show.legend=F)'								
			} else {
				ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome, group=axis[2], linetype=axis[2]), color=ghost.line, show.legend=F)'				
			}
		} else {	
			ghost = 'geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome), color=ghost.line, show.legend=F)'
		}

	} else {
		ghost = "xxxx"
	}	

	### add prediction lines
	if (!is.null(prediction)){
	
		### see how many models are being compared
		num.models = levels(prediction$model)
	
		if (!is.na(axis[2]) & length(num.models)>1){
			stop("Sorry. I can't plot the model(s) lines when there are already lines in the plot. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z), or choose to display only one model")
		}


		#### bin the predictions, where needed
		if (length(break.me)>0){
			for (i in 1:length(break.me)){
				### find that variable in the model and bin it
				prediction[[break.me[i]]] = bin.me(break.me[i], prediction, bins, labels[i], breaks[[break.me[i]]])

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
				
				
				#### if they supply more than two models to compare...
				if (length(levels(prediction$model))>2){
					pred.line = 'geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1)' 									
				} else {
					pred.line = 'geom_line(data= prediction, aes(linetype=model, y=prediction, colour=model), size=1) + scale_linetype_manual(values=c("solid", "dotdash"))' 				
				}
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

	theme = "theme_bw() + theme(text=element_text(size=18))"

	if (is.finite(sample) & is.numeric(data[,outcome])){
		theme = paste0('theme_bw() + coord_cartesian(ylim=c(', min(data[,outcome], na.rm=T), ", ", max(data[,outcome], na.rm=T),"))")
	}
	
	
	#### if they have a logistic, modify the p to reflect the data
	if (method=="logistic" & !is.numeric(data[,outcome])){
		p = gsub("data=[[:alnum:]]+,", "data=factor.to.logistic(data,outcome),", p)
		points = gsub("data\\)", "factor.to.logistic(data,outcome))", points)		
		
		#### change the y axis labels
		theme = paste0(theme, " + scale_y_continuous(breaks = c(0,1), labels=factor.to.logistic(data,outcome, labels=T))")	
	}
	#### evaluate the plot
	total.call = paste0(p, "+",points, "+",fitted, "+", facets, "+", ghost, "+", pred.line, "+", theme)
	### remove +xxxx (happens when I've made an element blank)
	total.call = gsub("+xxxx","",total.call, fixed=T)
	final = suppressMessages(eval(parse(text=total.call)))

	
  ### suppress messages only works for print messages, but print messages actually show the plot (even when i want to store it for laster use). Thus, I need both. Weird. 
	#return(final)
	#class(final) <- c("flexplot", class(final))

	if(plot.string){
		return(total.call)
	} else {
		return(final)
	}
}	

#' Print flexplot object
#'
#' Print flexplot object
#' @aliases print.flexplots
#' @param x a flexplot object
#' @param ... ignored
#' @export
# print.flexplot <- function(x,...) {
  # suppressMessages(print.ggplot(x))
# }


# ### I am including the code below because CRAN does not permit :::
# print.ggplot = function(x){
	# function (x, newpage = is.null(vp), vp = NULL, ...) 
# {
    # set_last_plot(x)
    # if (newpage) 
        # grid::grid.newpage()
    # grDevices::recordGraphics(requireNamespace("ggplot2", quietly = TRUE), 
        # list(), getNamespace("ggplot2"))
    # data <- ggplot_build(x)
    # gtable <- ggplot_gtable(data)
    # if (is.null(vp)) {
        # grid::grid.draw(gtable)
    # }
    # else {
        # if (is.character(vp)) 
            # grid::seekViewport(vp)
        # else grid::pushViewport(vp)
        # grid::grid.draw(gtable)
        # grid::upViewport()
    # }
    # invisible(x)
# }
# }
#source("research/RPackages/flexplot/R/hidden_functions.R")
