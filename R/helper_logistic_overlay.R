# Helper function to create summary data for logistic overlay
create_logistic_summary = function(data = NULL, group_vars = NULL, outcome_var = NULL, 
                                   bin_centers = NULL, bin_width = NULL, plot = NULL) {
  
  # If plot is provided, extract everything from it
  if (!is.null(plot)) {
    data = plot$data
    outcome_var = rlang::as_name(plot$mapping$y)
    ### extract given and axis variables
    given.axis = flexplot_axis_given(formula)
    given = given.axis$given
    axis = given.axis$axis
    variables_to_group_by = c(axis[2], given)[!is.na(c(axis[2], given))] %>% 
      na.omit() %>%
      purrr::map_vec(resolve_binned_var, data=data, return.name=T)
    
    
    # Calculate bin info for the x-axis variable
    x_vals = data[[axis[1]]]
    bin_info = calculate_bins_for_logistic_overlay(x_vals, n_bins)
    bin_breaks = bin_info$bin_breaks; bin_width = bin_info$bin_width; bin_centers = bin_info$bin_centers
    
    
    # Add bins to data
    data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
    
    # Create grouping variables for summarization
    group_vars = c("bin", variables_to_group_by)
    group_vars = group_vars[!is.na(group_vars)]
    # Add logic here to extract other needed info from plot if needed
  }
  
  # Validate we have what we need
  if (is.null(data) || is.null(outcome_var)) {
    stop("Must provide either 'plot' or 'data' + 'outcome_var'")
  }
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      count = n(),
      prop_died = {
        outcome = .data[[outcome_var]]
        if (is.character(outcome) || is.factor(outcome)) {
          outcome = as.numeric(factor(outcome)) - 1
        }
        mean(outcome, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    mutate(
      bin_mid = bin_centers[as.numeric(bin)],
      ymin = 0.5,
      ymax = prop_died,
      xmin = bin_mid - bin_width * 0.4,
      xmax = bin_mid + bin_width * 0.4,
      alpha = scales::rescale(count, to = c(0.2, 1)),
      size = 1.5
    )
}


# Helper function to build aesthetic mapping from original plot
build_inherited_aesthetics = function(plot, summary_data, base_x = "bin_mid", base_y = "prop_died") {
  base_aes = aes(x = !!sym(base_x), y = !!sym(base_y))
  
  # Add color/group aesthetics if they exist in the original plot
  if (!is.null(plot$mapping$colour)) {
    color_var = rlang::as_name(plot$mapping$colour)
    if (color_var %in% names(summary_data)) {
      base_aes$colour = sym(color_var)
    }
  }
  
  if (!is.null(plot$mapping$fill)) {
    fill_var = rlang::as_name(plot$mapping$fill)
    if (fill_var %in% names(summary_data)) {
      base_aes$fill = sym(fill_var)
    }
  }
  
  if (!is.null(plot$mapping$shape)) {
    shape_var = rlang::as_name(plot$mapping$shape)
    if (shape_var %in% names(summary_data)) {
      base_aes$shape = sym(shape_var)
    }
  }
  
  return(base_aes)
}

calculate_bins_for_logistic_overlay = function(x_vals, n_bins) {
  bin_breaks = quantile(x, probs = seq(from=0, to=1, length.out=15))
  bin_width = diff(bin_breaks)[1]
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  return(list(bin_breaks = bin_breaks, bin_width=bin_width, bin_centers=bin_centers))
}