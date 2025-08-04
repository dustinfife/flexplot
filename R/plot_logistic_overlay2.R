#' #' Add Logistic Overlay to Plot
#' #'
#' #' Adds a logistic overlay visualization to an existing flexplot or creates a new
#' #' plot with logistic overlay. The overlay shows binned observed proportions 
#' #' compared to the fitted logistic regression line, with a reference line at 0.5.
#' #'
#' #' @param plot A flexplot object to add the overlay to. If NULL, a new plot will 
#' #'   be created using the formula and data parameters.
#' #' @param formula A formula specifying the relationship between variables 
#' #'   (outcome ~ predictor). Required if plot is NULL.
#' #' @param data A data frame containing the variables specified in the formula. 
#' #'   Required if plot is NULL.
#' #' @param n_bins Integer specifying the number of bins to create along the 
#' #'   x-axis for summarizing the data. Default is 10.
#' #' @param type Character string specifying the type of overlay visualization. 
#' #'   Options are "dot" for point overlay or "bar" for rectangle overlay. 
#' #'   Default is "dot".
#' #' @param scale Character string specifying the scale for the y-axis. Options are 
#' #'   "probability" (default) or "logit" for log odds scale.
#' #' @param ... Additional arguments passed to flexplot() when creating a new plot.
#' #'
#' #' @return A ggplot object with the logistic overlay added. The overlay includes:
#' #'   \itemize{
#' #'     \item Binned observed proportions (as points or rectangles)
#' #'     \item A dashed horizontal reference line (at y = 0.5 for probability scale, 
#' #'           or y = 0 for logit scale)
#' #'     \item The original fitted logistic curve
#' #'   }
#' #'
#' #' @details
#' #' This function is designed to work with binary outcome variables in logistic 
#' #' regression contexts. It bins the predictor variable and calculates observed 
#' #' proportions within each bin, allowing visual comparison with the fitted 
#' #' logistic regression line.
#' #' 
#' #' The function can either:
#' #' \itemize{
#' #'   \item Add an overlay to an existing flexplot object
#' #'   \item Create a new plot from a formula and data
#' #' }
#' #' 
#' #' When using "dot" type, point sizes reflect the relative frequency or 
#' #' importance of each bin. When using "bar" type, rectangles show the 
#' #' observed proportions as bars.
#' #' 
#' #' When scale="logit", both the observed proportions and fitted values are 
#' #' converted to log odds scale, and the reference line is placed at y = 0.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create overlay on existing plot (probability scale)
#' #' p <- flexplot(outcome ~ predictor, data = mydata, method = "logistic")
#' #' logistic_overlay(plot = p, n_bins = 15, type = "dot")
#' #' 
#' #' # Create new plot with overlay on log odds scale
#' #' logistic_overlay(formula = success ~ score, data = mydata, 
#' #'                  n_bins = 8, type = "bar", scale = "logit")
#' #' }
#' #'
#' #' @seealso \code{\link{flexplot}} for creating the base plots
#' #'
#' #' @export
#' logistic_overlay = function( plot = NULL, formula = NULL, data = NULL, n_bins = 10, type = "dot", scale = "probability", ...) {
#'   
#'   if (is.null(plot) && (is.null(formula) || is.null(data))) {
#'     stop("You must provide either a flexplot-style formula + data, or a plot.")
#'   }
#'   
#'   if (!is.null(formula)) {
#'     outcome_var = all.vars(formula)[1]
#'     predictor_var = all.vars(formula)[2]
#'     x_vals = data[[predictor_var]]
#'     
#'     # Create appropriate plot based on scale
#'     if (scale == "logit") {
#'       # For logit scale, we need to manually create the plot with transformed data
#'       # First fit the logistic model to get predictions
#'       logistic_model = glm(formula, data = data, family = binomial)
#'       
#'       # Get all variables from the formula
#'       all_vars = all.vars(formula)
#'       main_predictor = all_vars[2]  # First predictor (x-axis variable)
#'       
#'       # Create prediction data across the range of main predictor
#'       x_range = range(data[[main_predictor]], na.rm = TRUE)
#'       pred_x = seq(x_range[1], x_range[2], length.out = 100)
#'       
#'       # Create prediction data frame with all necessary variables
#'       # Use representative values (means/modes) for other variables
#'       pred_data = data.frame(x = pred_x)
#'       names(pred_data)[1] = main_predictor
#'       
#'       # Add other variables with representative values
#'       other_vars = all_vars[-c(1,2)]  # Exclude outcome and main predictor
#'       if (length(other_vars) > 0) {
#'         for (var in other_vars) {
#'           if (is.numeric(data[[var]])) {
#'             pred_data[[var]] = mean(data[[var]], na.rm = TRUE)
#'           } else {
#'             # For factors, use the most common level
#'             tbl = table(data[[var]])
#'             pred_data[[var]] = names(tbl)[which.max(tbl)]
#'           }
#'         }
#'       }
#'       
#'       # Get fitted probabilities and convert to log odds
#'       fitted_probs = predict(logistic_model, newdata = pred_data, type = "response")
#'       fitted_logodds = log(fitted_probs / (1 - fitted_probs))
#'       
#'       # Create the base plot manually
#'       plot = ggplot(data, aes_string(x = main_predictor)) +
#'         geom_line(data = data.frame(x = pred_x, y = fitted_logodds), 
#'                   aes(x = x, y = y), color = "blue") +
#'         labs(x = main_predictor, y = "Log Odds") +
#'         theme_bw()
#'       names(plot$data)[1] = main_predictor  # Ensure proper naming
#'       
#'     } else {
#'       # Use original data if probability scale, processed data if available
#'       if (exists("plot") && !is.null(plot$data)) {
#'         data = plot$data  # Use processed data from flexplot
#'       }
#'     }
#'   } else {
#'     data = plot$data
#'     predictor_var = rlang::as_name(plot$mapping$x)
#'     outcome_var = rlang::as_name(plot$mapping$y)
#'     x_vals = data[[predictor_var]]
#'     formula = formula(plot)
#'   }
#'   
#'   ### extract given and axis variables
#'   given.axis = flexplot_axis_given(formula)
#'   given = given.axis$given
#'   axis = given.axis$axis
#'   variables_to_group_by = c(axis[2], given)[!is.na(c(axis[2], given))] %>% 
#'     na.omit() %>%
#'     purrr::map_vec(resolve_binned_var, data=data, return.name=T)
#'   
#'   
#'   # Calculate bin info for the x-axis variable
#'   x_vals = data[[axis[1]]]
#'   bin_info = calculate_bins_for_logistic_overlay(x_vals, n_bins)
#'   bin_breaks = bin_info$bin_breaks; bin_width = bin_info$bin_width; bin_centers = bin_info$bin_centers
#'   
#'   # Add bins to data
#'   data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
#'   
#'   
#'   # Create grouping variables for summarization
#'   group_vars = c("bin", variables_to_group_by)
#'   group_vars = group_vars[!is.na(group_vars)]
#'   
#'   
#'   # Summarize data with numeric-safe outcome
#'   summary_data = create_logistic_summary(data, group_vars, outcome_var, bin_centers, bin_width)
#'   
#'   # Convert to logit scale if requested
#'   if (scale == "logit") {
#'     # First, identify the column that represents the observed proportions
#'     # Common names might be: y, proportion, observed_prop, etc.
#'     prop_col = NULL
#'     possible_names = c("y", "proportion", "observed_prop", "prop", outcome_var)
#'     
#'     for (name in possible_names) {
#'       if (name %in% names(summary_data)) {
#'         prop_col = name
#'         break
#'       }
#'     }
#'     
#'     if (is.null(prop_col)) {
#'       # Debug: print column names to help identify the issue
#'       cat("Available columns in summary_data:", paste(names(summary_data), collapse = ", "), "\n")
#'       stop("Could not identify the proportion column in summary_data. Please check the create_logistic_summary function.")
#'     }
#'     
#'     # Convert observed proportions to log odds
#'     summary_data = summary_data %>%
#'       mutate(
#'         # Handle edge cases for proportions of 0 or 1
#'         y_original = .data[[prop_col]],  # Keep original for reference
#'         !!prop_col := ifelse(y_original == 0, 
#'                              log(0.5/size),  # Use continuity correction for 0
#'                              ifelse(y_original == 1,
#'                                     log((size - 0.5)/0.5),  # Use continuity correction for 1
#'                                     log(y_original / (1 - y_original)))),  # Standard logit transform
#'         # Also need to adjust ymin/ymax for rectangles if using bar type
#'         ymin = ifelse(type == "bar" & "ymin" %in% names(.), 0, 
#'                       ifelse("ymin" %in% names(.), ymin, NA)),
#'         ymax = ifelse(type == "bar" & "ymax" %in% names(.), .data[[prop_col]], 
#'                       ifelse("ymax" %in% names(.), ymax, NA))
#'       )
#'   }
#'   
#'   # Build base plot if not provided
#'   if (is.null(plot)) {
#'     if (scale == "logit") {
#'       # For logit scale, manually create the plot
#'       logistic_model = glm(formula, data = data, family = binomial)
#'       
#'       # Get all variables and main predictor
#'       all_vars = all.vars(formula)
#'       main_predictor = all_vars[2]  # First predictor (x-axis)
#'       
#'       # Create prediction data
#'       x_range = range(data[[main_predictor]], na.rm = TRUE)
#'       pred_x = seq(x_range[1], x_range[2], length.out = 100)
#'       
#'       # Start with main predictor
#'       pred_data = data.frame(x = pred_x)
#'       names(pred_data)[1] = main_predictor
#'       
#'       # Add other variables with representative values
#'       other_vars = all_vars[-c(1,2)]  # Exclude outcome and main predictor
#'       if (length(other_vars) > 0) {
#'         for (var in other_vars) {
#'           if (is.numeric(data[[var]])) {
#'             pred_data[[var]] = mean(data[[var]], na.rm = TRUE)
#'           } else {
#'             # For factors, use the most common level
#'             tbl = table(data[[var]])
#'             pred_data[[var]] = names(tbl)[which.max(tbl)]
#'           }
#'         }
#'       }
#'       
#'       # Get fitted log odds
#'       fitted_probs = predict(logistic_model, newdata = pred_data, type = "response")
#'       fitted_logodds = log(fitted_probs / (1 - fitted_probs))
#'       
#'       # Create plot
#'       plot = ggplot(data, aes_string(x = main_predictor)) +
#'         geom_line(data = data.frame(x = pred_x, y = fitted_logodds), 
#'                   aes(x = x, y = y), color = "blue") +
#'         labs(x = main_predictor, y = "Log Odds") +
#'         theme_bw()
#'       
#'     } else {
#'       # Normal probability scale
#'       plot = flexplot(formula, data = data, method = "logistic", raw.data = FALSE)
#'     }
#'   }
#'   
#'   # Build aesthetic mapping to match original plot
#'   base_aes = build_inherited_aesthetics(plot, summary_data)
#'   
#'   # Start with bars or dots
#'   observed_layer = if (type == "dot") {
#'     geom_point(
#'       data = summary_data,
#'       mapping = modifyList(base_aes, aes(size = size)),
#'       inherit.aes = FALSE,
#'       show.legend = FALSE
#'     )
#'   } else {
#'     rect_aes = aes(
#'       xmin = xmin, xmax = xmax,
#'       ymin = pmin(ymin, ymax), ymax = pmax(ymin, ymax),
#'       alpha = alpha
#'     )
#'     # Add fill aesthetic if it exists
#'     if (!is.null(base_aes$fill)) {
#'       rect_aes$fill = base_aes$fill
#'     }
#'     
#'     geom_rect(
#'       data = summary_data,
#'       mapping = rect_aes,
#'       inherit.aes = FALSE,
#'       show.legend = FALSE
#'     )
#'   }
#'   
#'   # Add appropriate reference line based on scale
#'   reference_line = if (scale == "logit") {
#'     geom_hline(yintercept = 0, linetype = "dashed")  # 0 on logit scale = 0.5 probability
#'   } else {
#'     geom_hline(yintercept = 0.5, linetype = "dashed")  # 0.5 on probability scale
#'   }
#'   
#'   plot +
#'     observed_layer +
#'     reference_line
#' }