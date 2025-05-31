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
#' When using "dot" type, point sizes reflect the relative frequency or 
#' importance of each bin. When using "bar" type, rectangles show the 
#' observed proportions as bars.
#'
#' @examples
#' \dontrun{
#' # Create overlay on existing plot
#' p <- flexplot(outcome ~ predictor, data = mydata, method = "logistic")
#' logistic_overlay(plot = p, n_bins = 15, type = "dot")
#' 
#' # Create new plot with overlay
#' logistic_overlay(formula = success ~ score, data = mydata, 
#'                  n_bins = 8, type = "bar")
#' }
#'
#' @seealso \code{\link{flexplot}} for creating the base plots
#'
#' @export
logistic_overlay = function( plot = NULL, formula = NULL, data = NULL, n_bins = 10, type = "dot", ...) {
  
  if (is.null(plot) && (is.null(formula) || is.null(data))) {
    stop("You must provide either a flexplot-style formula + data, or a plot.")
  }
  
  if (!is.null(formula)) {
    outcome_var = all.vars(formula)[1]
    predictor_var = all.vars(formula)[2]
    x_vals = data[[predictor_var]]
    plot = flexplot(formula, data, ...)
  } else {
    data = plot$data
    predictor_var = rlang::as_name(plot$mapping$x)
    outcome_var = rlang::as_name(plot$mapping$y)
    x_vals = data[[predictor_var]]
    formula = formula(plot)
  }
  
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  variables_to_group_by = c(axis[2], given)[!is.na(c(axis[2], given))] %>% 
                            na.omit() %>%
                            purrr::map_vec(resolve_binned_var, data=data, return.name=T)
  
  
  # Calculate bin info for the x-axis variable
  x_vals = data[[axis[1]]]
  bin_breaks = pretty(x_vals, n = n_bins)
  bin_width = diff(bin_breaks)[1]
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  
  # Add bins to data
  data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
  
  # Create grouping variables for summarization
  group_vars = c("bin", variables_to_group_by)
  group_vars = group_vars[!is.na(group_vars)]
  
  
  # Summarize data with numeric-safe outcome
  summary_data = create_logistic_summary(data, group_vars, outcome_var, bin_centers, bin_width)
  
  
  # Build base plot if not provided
  if (is.null(plot)) {
    plot = flexplot(formula, data = data, method = "logistic", raw.data = FALSE)
  }
  
  # Build aesthetic mapping to match original plot
  base_aes = build_inherited_aesthetics(plot, summary_data)
  
  # Start with bars or dots
  observed_layer = if (type == "dot") {
    geom_point(
      data = summary_data,
      mapping = modifyList(base_aes, aes(size = size)),
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
    geom_hline(yintercept = 0.5, linetype = "dashed") 
}