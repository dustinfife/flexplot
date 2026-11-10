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
##'   Observations are paired by their position within each of the two conditions,
##'   not by a subject identifier. Arrange the data so corresponding observations
##'   appear in the same order within their respective conditions before using
##'   \code{related=TRUE}.
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
##' @param prediction Predicted values for a prediction line. Defaults to NULL. This is generally NOT used when just using flexplot, but it's helpful with the wrapper function \link{compare_fits}
##' @param suppress_smooth Should a fitted line be repressed? Defaults to FALSE
##' @param alpha The transparency of the datapoints. 
##' @param plot.string Retained for compatibility. String-based plotting has been
##' retired; setting this to TRUE produces an error. Modify the returned ggplot
##' object directly instead.
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
		method=NULL, se=NULL, 
		ghost.line=NULL, ghost.reference=NULL,
		spread=c('quartiles', 'stdev', 'sterr'), jitter=NULL, raw.data=T,
		sample=Inf, 
		prediction = NULL, suppress_smooth=F, alpha=.99977, plot.string=F, silent=F,
		third.eye=NULL,
		plot.type = c("histogram", "qq", "density", "boxplot", "violin", "line"), 
		return_data = F, ...){

  if (isTRUE(plot.string)) {
    stop("String-based plotting has been retired; plot.string=TRUE is no longer supported. ",
         "Modify the returned ggplot object directly instead.", call.=FALSE)
  }
  comparison_predictions = isTRUE(attr(prediction, "flexplot_comparison"))
  comparison_call = attr(prediction, "flexplot_comparison_call")
  if (!is.null(comparison_call)) attr(prediction, "flexplot_comparison_call") = NULL
  
  # modify data if they have an equation in the formula
  ff = formula_functions(formula, data)
  data = ff$data; formula =ff$formula
  formula = flexplot_reverse_categorical(formula, data, prediction, related)
  
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
  
  # Determine whether a magnet plot is appropriate
  use_magnet = is_binary(data[[outcome]]) &&
    check.non.number(data[[axis[1]]]) &&
    (is.null(method) || method == "logistic")

  # Native categorical dispatch is determined by the observed outcome levels.
  if (!is.numeric(data[[outcome]]) && is_binary(data[[outcome]]) &&
      check.non.number(data[[axis[1]]])) {
    use_magnet = TRUE
  }
  
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
  create_breaks = flexplot_create_breaks
  if (use_magnet) {
    create_breaks = flexplot_magnet_breaks
  }
  breaks = create_breaks(break.me = break.me, breaks, data, labels, bins=bins)

  flexplot_errors(variables, data, axis)
  check_same_variables_in_prediction(formula, prediction)
  
  # extract original names of dv (for logistic, prior to making it continuous)
  method = identify_method(data, outcome, axis, method)
  
  # for logistic, it automatically makes the first alphabetical as the referent level
  # if they provide an ordered factor, make the second the referent level
  logistic_labels = NULL
  
  if (identical(method, "logistic") && is.factor(data[[outcome]])) {
    logistic_labels = levels(droplevels(data[[outcome]]))
  }
  
  ### make modifications to the data
  magnet_x = NULL
  if (use_magnet && is.factor(data[[axis[1]]])) {
    magnet_x = data[[axis[1]]]
  }
  data = flexplot_modify_data(data=data, formula = formula, variables=variables, outcome=outcome, axis=axis, given=given, related=related, labels=labels, 
          break.me=break.me, breaks=breaks, bins=bins, spread=spread, method=method, use_magnet = use_magnet)
  if (plot.type %in% c("boxplot", "violin") && !related &&
      !identical(axis[1], "1") && is.numeric(data[[axis[1]]])) {
    stop("plot.type='", plot.type, "' requires a categorical x variable; '",
         axis[1], "' is continuous numeric.", call.=FALSE)
  }
  if (!is.null(magnet_x)) {
    # Shared summary preprocessing may sort x by the outcome; native magnets
    # and comparisons retain the caller's factor order for every layer.
    data[[axis[1]]] = factor(data[[axis[1]]], levels=levels(magnet_x),
                             ordered=is.ordered(magnet_x))
    attr(data[[axis[1]]], "contrasts") = attr(magnet_x, "contrasts")
  }
  prediction = flexplot_modify_data(data=prediction, variables=variables, outcome=outcome, axis=axis, given=given, related=related, labels=labels, 
          break.me=break.me, breaks=breaks, bins=bins, spread=spread, pred.data = TRUE)
  
  ### more data modifications if there's a magnet plot
  if (use_magnet) {
    magnet_axis = flexplot_object_axis(data, axis)
    magnet_given = flexplot_magnet_display_given(data, given)
    data = prepare_magnet_data(
      data = data,
      outcome = outcome,
      axis = magnet_axis,
      given = magnet_given
    )
  }
  
  # make sure data and prediction have the same variable types for axis 1
  prediction = make_prediction_dataset_same_type_on_x1(data, prediction, axis[1])
  
  ##### make models into a factor if they supply predictions
	if (!is.null(prediction)){
		prediction$model = factor(prediction$model)
		### make the levels consistent between prediction/data for axis 1
		prediction = make_levels_same_for_prediction_dataset(data, prediction, axis)
	}
	
  ### report errors when necessary
  #### give an error if they try to visualize logistic with a categorical x axis
  check_error_for_logistic(variables = variables, data = data, method=method, axis=axis, use_magnet=use_magnet)
  
  ### change alpha, depending on plot type
  alpha = flexplot_alpha_default(data=data, axis = axis, alpha = alpha)
  
  ### change se based on how many variables they have
  se = check_se(se, axis)
  
  # Native magnets retain supplied probabilities and bins already prepared above.
  # Direct non-magnet predictions retain their existing bin aggregation.
  if (comparison_predictions && !use_magnet && length(axis) > 1 &&
      length(unique(prediction$model)) > 1) {
    panels = c(axis[-1], given[!is.na(given) & nzchar(given)])
    panel_formula = as.name(panels[1])
    for (variable in panels[-1]) panel_formula = call("+", panel_formula, as.name(variable))
    if (is.null(comparison_call)) {
      comparison_call = quote(compare_fits(formula=NULL, data=d, model1=model1, model2=model2))
    }
    comparison_call$formula = call("~", formula[[2]], call("|", as.name(axis[1]), panel_formula))
    stop("Sorry, two-model comparisons don't currently support a second-axis variable ",
         "(in this case, ", axis[2], "). Try placing it in panels instead:\n\n",
         paste(deparse(comparison_call, width.cutoff=100L), collapse="\n"), call.=FALSE)
  }
  if (!comparison_predictions && !is.null(prediction) && !use_magnet) {
    num.models = levels(prediction$model)
    
    prediction = flexplot_modify_prediction(
      prediction, axis, num.models, break.me,
      bins, labels, breaks, predictors
    )
  }
  
  categorical_ghost = !is.numeric(data[[axis[1]]]) && is.numeric(data[[outcome]]) &&
    !use_magnet && !related && !identical(axis[1], "1")
  ghost_supported = is.null(ghost.line) ||
    (!is.na(given[1]) &&
       (is.numeric(data[[axis[1]]]) ||
          !is.null(prediction) || categorical_ghost || use_magnet))
  

  if (!is.null(ghost.line) && !is.null(prediction)) {
    if (length(axis) > 1) {
      stop("Prediction ghost lines with a second-axis grouping variable are not yet supported. ",
           "Use one primary predictor without a second axis.", call.=FALSE)
    }
    if (is.na(given[1])) {
      stop("Prediction ghost lines require a conditioning variable to identify reference and destination panels.",
           call.=FALSE)
    }
  }

  final = flexplot_render_object(
    data=data,
    outcome=outcome,
    axis=axis,
    given=given,
    method=method,
    se=se,
    suppress_smooth=suppress_smooth,
    plot.type=plot.type,
    bins=bins,
    spread=spread,
    use_magnet=use_magnet,
    related=related,
    jitter=jitter,
    alpha = alpha,
    sample = sample,
    raw.data = raw.data,
    magnet_summary=!comparison_predictions,
    magnet_labels=logistic_labels,
    magnet_lines=!is.null(ghost.line)
  )

  facet = flexplot_panel_variables_object(given, break.me)
  if (use_magnet && !is.null(prediction)) {
    comparison = flexplot_magnet_comparison(prediction, axis, data, given)
    prediction = comparison$prediction
    if (!is.null(comparison$facet)) facet = comparison$facet
  }

  # add prediction overlay lines
  if (!is.null(prediction)) {
    final = final + flexplot_prediction_layer(
      prediction=prediction,
      axis=axis,
      data=data,
      use_magnet=use_magnet,
      given=given,
      comparison=comparison_predictions
    )
  }


  # Add ghost-line overlay
  if (!is.null(ghost.line) && ghost_supported) {
    
    ghost.reference = create_ghost_reference(
      ghost.reference=ghost.reference,
      data=as.data.frame(data),
      bins=bins,
      breaks=breaks,
      given=given,
      axis=if (use_magnet && is.null(prediction) && is.null(ghost.reference)) axis[1] else axis,
      labels=labels
    )
    
    reference_data = flexplot_ghost_reference_data(
      data=if (is.null(prediction)) data else prediction,
      given=given,
      ghost.reference=ghost.reference,
      axis=if (categorical_ghost) NULL else axis
    )
    
    if (use_magnet && is.null(prediction)) {
      
      ghost_data = flexplot_ghost_magnet_data(
        final=final,
        reference_data=reference_data,
        given=given,
        data=data,
        axis=axis
      )
      ghost_axes = attr(ghost_data, "ghost_axes")
      ghost_group = attr(ghost_data, "ghost_group")
      line_groups = c(flexplot_magnet_display_given(data, given), ghost_group)
      ghost_lines = ghost_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(line_groups))) |>
        dplyr::filter(dplyr::n_distinct(.data[[ghost_axes[1]]]) > 1) |>
        dplyr::ungroup()
      
      final = final +
        ggplot2::geom_line(
          data=ghost_lines,
          mapping=ggplot2::aes(
            x=.data[[ghost_axes[1]]],
            y=.data[[ghost_axes[2]]],
            group=.data[[ghost_group]]
          ),
          inherit.aes=FALSE,
          show.legend=FALSE,
          color=ghost.line,
          linewidth=0.8
        ) +
        ggplot2::geom_point(
          data=ghost_data,
          mapping=ggplot2::aes(
            x=.data[[ghost_axes[1]]],
            y=.data[[ghost_axes[2]]],
            group=.data[[ghost_group]]
          ),
          inherit.aes=FALSE,
          show.legend=FALSE,
          color=ghost.line,
          shape=1,
          size=3,
          stroke=1.2
        )
      
    } else {
      
      if (!is.null(prediction)) {
        ghost_data = flexplot_ghost_prediction_data(
          reference_data, axis, given, data,
          use_magnet, comparison_predictions
        )
      } else if (categorical_ghost) {
        ghost_data = flexplot_ghost_categorical_data(
          reference_data, data, outcome, axis, given,
          ghost.reference, spread, plot.type
        )
      } else {
        ghost_data = flexplot_ghost_smooth_data(
          reference_data=reference_data,
          outcome=outcome,
          axis=axis,
          given=given,
          method=method,
          plot.type=plot.type,
          bins=bins,
          spread=spread,
          jitter=jitter
        )
      }
      
      reference_groups = NULL
      
      if (!is.null(final$mapping$linetype)) {
        group_var = flexplot_object_axis(data, axis)[2]
        reference_groups = reference_data[[group_var]]
      }
      
      final = final + flexplot_ghost_layer(
        d_smooth=ghost_data,
        ghost.line=ghost.line,
        reference_groups=reference_groups
      )
    }
  }



  if (!use_magnet && !is.null(logistic_labels)) {
    final = final + ggplot2::scale_y_continuous(
      breaks=c(0, 1),
      labels=logistic_labels
    )
  }

  if (!is.null(facet)) final = final + facet
  final = flexplot_sampling_structure(final, data, outcome, axis, sample, related, use_magnet)

  final$formula = formula

  if (return_data) return(data)
  return(final)
}
