#' Add Logistic Overlay to Plot
#'
#' Adds a logistic overlay visualization to an existing flexplot or creates a new
#' plot with logistic overlay. The overlay shows binned observed proportions 
#' compared to the fitted logistic regression line, with a reference line at 0.5.
#'
#' @param plot A flexplot object to add the overlay to. If NULL, a new plot will 
#'   be created using the formula and data parameters.
#' @param formula A formula specifying the relationship between variables 
#'   (outcome ~ predictor). Required if plot is NULL.
#' @param data A data frame containing the variables specified in the formula. 
#'   Required if plot is NULL.
#' @param n_bins Integer specifying the number of bins to create along the 
#'   x-axis for summarizing the data. Default is 10.
#' @param type Character string specifying the type of overlay visualization. 
#'   Options are "dot" for point overlay or "bar" for rectangle overlay. 
#'   Default is "dot".
#' @param scale Character string specifying the scale for the y-axis. Options are 
#'   "probability" (default) or "logit" for log odds scale.
#' @param ... Additional arguments passed to flexplot() when creating a new plot.
#'
#' @return A ggplot object with the logistic overlay added. The overlay includes:
#'   \itemize{
#'     \item Binned observed proportions (as points or rectangles)
#'     \item A dashed horizontal reference line at y = 0.5
#'     \item The original fitted logistic curve
#'   }
#'
#' @details
#' This function is designed to work with binary outcome variables in logistic 
#' regression contexts. It bins the predictor variable and calculates observed 
#' proportions within each bin, allowing visual comparison with the fitted 
#' logistic regression line.
#' 
#' The function can either:
#' \itemize{
#'   \item Add an overlay to an existing flexplot object
#'   \item Create a new plot from a formula and data
#' }
#'
#' @examples
#' \dontrun{
#' # Create overlay on existing plot
#' p <- flexplot(died ~ agility, data = avengers, method = "logistic")
#' logistic_overlay(plot = p, n_bins = 15, type = "dot")
#' 
#' # Create new plot with overlay
#' logistic_overlay(formula = died ~ agility, data = avengers, 
#'                  n_bins = 8, type = "bar")
#' }
#'
#' @seealso \code{\link{flexplot}} for creating the base plots
#'
#' @export
logistic_overlay = function(formula = NULL, data=NULL, plot = NULL, n_bins = 10, type = "dot", scale = "probability", ...) {


  # if the user puts a plot without saying "plot = ", convert to a plot
  if (class(formula)[1] == "gg") {
    plot = formula
    formula = NULL
  }
  
  if (is.null(plot) && (is.null(formula) || is.null(data))) {
    stop("You must provide either a flexplot-style formula + data, or a plot.")
  }
  
  if (!is.null(formula)) {
    outcome_var = all.vars(formula)[1]
    predictor_var = all.vars(formula)[2]
    x_vals = data[[predictor_var]]
    # Build a plot (Note: I need this to get the "bins" from the data)
    plot = flexplot(formula, data, method="logistic", raw.data=FALSE, ...)
    data = plot$data
  } else {
    data = plot$data
    predictor_var = rlang::as_name(plot$mapping$x)
    outcome_var = rlang::as_name(plot$mapping$y)
    x_vals = data[[predictor_var]]
    formula = formula(plot)
  }
  
  datasets_for_plotting = convert_formula_to_logistic_bins(formula=formula, 
                                                           data = data, 
                                                           x_vals = x_vals,
                                                           n_bins = n_bins)
  summary_data = datasets_for_plotting$summary_data
  data         = datasets_for_plotting$data
  
  # for logit scale, produce a new plot
  if (scale == "logit") {
  
    # convert probabilities to logit
    summary_data$p_c = pmax(0.5/summary_data$count, 
                            pmin((summary_data$count - 0.5)/summary_data$count, 
                                 summary_data$proportion))
    summary_data$proportion = log(summary_data$p_c / (1 - summary_data$p_c))
    
    # create new flexplot based on logit
    vars = all.vars(formula)
    new.form = paste0(vars[1], "~", paste0(vars[-1], collapse="*"))
    data$predictions = glm(new.form, data=data, family=binomial) %>% predict
    flex_form = as.formula(gsub(vars[1], "predictions", deparse(formula)))
    plot = flexplot(flex_form, data=data, method="lm", raw.data = FALSE, ...) + 
      labs(y = "Log Odds")
    

  }
  
  # Build aesthetic mapping to match original plot
  base_aes = build_inherited_aesthetics(plot, summary_data)
  
  # Start with bars or dots
  observed_layer = if (type == "dot") {
    geom_jitter(
      data = summary_data,
      mapping = base_aes,
      size = 0.5,
      width = 0,    # horizontal jitter amount
      height = 0,  # vertical jitter amount  
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  } else {
    rect_aes = aes(
      xmin = xmin, xmax = xmax,
      ymin = pmin(ymin, ymax), ymax = pmax(ymin, ymax),
      alpha = alpha
    )
    # Add fill aesthetic if it exists
    if (!is.null(base_aes$fill)) {
      rect_aes$fill = base_aes$fill
    }
    
    geom_rect(
      data = summary_data,
      mapping = rect_aes,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  }
  
  plot +
    observed_layer +
    geom_hline(yintercept = ifelse(scale == "logit", 0, 0.5), linetype = "dashed")
}