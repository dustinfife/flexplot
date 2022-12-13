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
##'  
##'  To better enable comparisons across panels, Flexplot implements "ghost lines," which simply repeat the line from one panel to another. For more information about ghost lines, see the vignette (also
##'  available at https://psyarxiv.com/kh9c3/)
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
##' @param plot.type This argument allows the user to control the type of plot used. Flexplot defaults to histograms (for univariate variables)
##' but could also do qqplots (using "qq" as the argument) or density plots (using "density"). Also, the user can specify "boxplot" for boxplots and
##' "violin" for violin plots. 
##' @param return_data Should flexplot return the dataset? Defaults to No. This is useful if you want to recycle the bin assigments from flexplot. 
##' @param ... Other arguments passed to \code{\link[ggplot2]{aes}}
##' @author Dustin Fife
##' @import tibble ggplot2 R6
##' @export
##' @return a ggplot2 object
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
		third.eye=NULL,
		plot.type = c("histogram", "qq", "density", "boxplot", "violin", "line"), 
		return_data = F, ...){

  # modify data if they have an equation in the formula
  ff = formula_functions(formula, data)
  data = ff$data; formula =ff$formula
  
  spread    = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
  plot.type = match.arg(plot.type, c("histogram", "qq", "density", "boxplot", "violin", "line"))
  
  ### if they supply tibble, change to a data frame (otherwise the referencing screws things up)
  if (tibble::is_tibble(data)) data = as.data.frame(data)
  
  # prepare variables
  variables = all.vars(formula, unique=FALSE)
  outcome = variables[1]
  predictors = variables[-1]
  
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  
  # report errors
  flexplot_errors(variables = variables, data = data, axis=axis)
  
  #### identify which variables are numeric and which are factors
  vtypes = variable_types(predictors, data, return.names=T)
  numbers = vtypes$numbers
  categories = vtypes$characters
  if (outcome %in% categories){
    levels = length(unique(data[,outcome]))	### necessary for univariate plots
  }
  
  ### create the lists that contain the breaks
  break.me = flexplot_break_me(data, predictors, given, axis, bins)
  breaks = flexplot_create_breaks(break.me = break.me, breaks, data, labels, bins=bins)
  
  flexplot_errors(variables, data, axis)
  check_same_variables_in_prediction(formula, prediction)
  
  # extract original names of dv (for logistic, prior to making it continuous)
  method = identify_method(data, outcome, axis, method)
  
  # for logistic, it automatically makes the first alphabetical as the referent level
  # if they provide an ordered factor, make the second the referent level
  logistic_labels = return_labels_for_logistic_regression(data, outcome, method)
  

  ### make modifications to the data
  data = flexplot_modify_data(data=data, formula = formula, variables=variables, outcome=outcome, axis=axis, given=given, related=related, labels=labels, 
          break.me=break.me, breaks=breaks, bins=bins, spread=spread, method=method)
  prediction = flexplot_modify_data(data=prediction, variables=variables, outcome=outcome, axis=axis, given=given, related=related, labels=labels, 
          break.me=break.me, breaks=breaks, bins=bins, spread=spread, pred.data = TRUE)

  ##### make models into a factor if they supply predictions
	if (!is.null(prediction)){
		prediction$model = factor(prediction$model)
		### make the levels consistent between prediction/data for axis 1
		prediction = make_levels_same_for_prediction_dataset(data, prediction, axis)
	}
	
  ### report errors when necessary
  #### give an error if they try to visualize logistic with a categorical x axis
  check_error_for_logistic(variables = variables, data = data, method=method, axis=axis)
  
  ### change alpha, depending on plot type
  alpha = flexplot_alpha_default(data=data, axis = axis, alpha = alpha)
  

  ### change se based on how many variables they have
  se = check_se(se, axis)


	### PLOT UNIVARIATE PLOTS
  bivariate = flexplot_bivariate_plot(formula = NULL, data=data, prediction = prediction, outcome=outcome, predictors=predictors, axis=axis,
                                                    related=related, alpha=alpha, jitter=jitter, suppress_smooth=suppress_smooth, 
                                                    method=method, spread=spread, plot.type=plot.type, bins=bins)
    p = bivariate$p
    points = bivariate$points
    fitted = bivariate$fitted
    
	#### all the above should take care of ALL possible plots, but now we add paneling
  facets = flexplot_panel_variables(given, break.me)
  
	if (!is.null(ghost.line) & !is.na(given[1])){ # with help from https://stackoverflow.com/questions/52682789/how-to-add-a-lowess-or-lm-line-to-an-existing-facet-grid/52683068#52683068
	 
				### bin the ghost reference if it's not null
    ghost.reference = create_ghost_reference(ghost.reference=ghost.reference, data=data,
                       bins=bins, breaks=breaks, given=given, axis=axis, labels=labels)
   
        ### extract the ggplot dataset that contains the fitted information
    d_smooth = create_ghost_dataset(data=data, axis=axis, prediction=prediction, given=given, 
                ghost.reference=ghost.reference, predictors=predictors, p=p, fitted=fitted, method=method, outcome=outcome, se=se)

        ### create the text for the ghost line
    ghosttext = create_ghost_text(d_smooth=d_smooth, axis=axis, outcome=outcome, prediction=prediction, ghost.line=ghost.line, ghost.reference=ghost.reference, data=data)
      ghost = ghosttext$ghost
      d_smooth = ghosttext$d_smooth
      

	} else {
		ghost = "xxxx"
	}	
                                                        
	### add prediction lines
	if (!is.null(prediction)){
		### see how many models are being compared
		num.models = levels(prediction$model)
		prediction = flexplot_modify_prediction(prediction, axis, num.models, break.me, bins, labels, breaks, predictors)
		pred.line = flexplot_generate_prediction_lines(prediction, axis, data)
	} else {
	  pred.line = "xxxx"
	}
  

	theme = "theme_bw() + theme(text=element_text(size=14))"

	### without this, the scale of the y axis changes if the user samples
	if (is.finite(sample) & is.numeric(data[,outcome])){
		theme = paste0('theme_bw() + coord_cartesian(ylim=c(', min(data[,outcome], na.rm=T), ", ", max(data[,outcome], na.rm=T),"))")
	}

	# convert labels for Y axis for logistic
	if (!is.null(logistic_labels)){
	  theme = paste0(theme, " + scale_y_continuous(breaks = c(0,1), labels=c('", logistic_labels[1], "', '", logistic_labels[2], "')", ")")
	}
	
	### if second axis is numeric, replace axis[2] with variable that is binned
  if (length(axis)>1){
    if (is.numeric(data[,axis[2]])){
      axis[2] = paste0(axis[2], "_binned")
    }
  }
	
	#### evaluate the plot
	total.call = paste0(p, "+",points, "+",fitted, "+", facets, "+", ghost, "+", pred.line, "+", theme)
	### remove +xxxx (happens when I've made an element blank)
	total.call = gsub("+xxxx","",total.call, fixed=T)
	
	if (exists("d_smooth")) {
	  grp_name = grep("group", names(d_smooth))
	  if (length(grp_name)==1) names(d_smooth)[grp_name[1]] = "GGroup"
	  if (length(grp_name)>1) names(d_smooth)[grp_name[2]] = "GGroup"  
	}

	final = suppressMessages(eval(parse(text=total.call)))
	
	# add formula (to make it easier for marginal_plot)
	final$formula = formula

	if(plot.string){
		return(total.call)
	} 
	
	if (return_data) {
	  return(data)
	}
	
	return(final)
}	




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
