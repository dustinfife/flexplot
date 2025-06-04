bin_and_summarize = function(x_vals, observed, data = NULL, n_bins = 10, 
                                   summary_fun = mean, x_summary_fun = mean) {
  
  # Handle data frame input
  if (!is.null(data)) {
    if (is.character(x_vals)) x_vals = data[[x_vals]]
    if (is.character(observed)) observed = data[[observed]]
  }
  
  # Create temporary data frame
  temp_data = data.frame(
                  x_vals = x_vals,
                  observed = observed
                  ) %>%
                    filter(!is.na(x_vals), !is.na(observed))
  
  # Calculate bins
  bin_info = calculate_bins_for_logistic_overlay(temp_data$x_vals, n_bins)
  temp_data$bin = cut(temp_data$x_vals, breaks = bin_info$bin_breaks, include.lowest = TRUE)
  
  # Summarize
  result = temp_data %>%
    group_by(bin) %>%
    summarise(
      x_center = x_summary_fun(x_vals, na.rm = TRUE),
      observed_prop = summary_fun(observed, na.rm = TRUE),
      n_obs = n(),
      bin_min = min(x_vals, na.rm = TRUE),
      bin_max = max(x_vals, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(result)
}