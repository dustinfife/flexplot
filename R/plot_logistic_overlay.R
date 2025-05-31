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