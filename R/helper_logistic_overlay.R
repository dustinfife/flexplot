# Helper function to create summary data for logistic overlay
create_logistic_summary = function(data = NULL, group_vars = NULL, outcome_var = NULL, 
                                   bin_centers = NULL, bin_width = NULL, plot = NULL, n_bins) {

  # If plot is provided, extract everything from it
  if (!is.null(plot)) {
    data = plot$data
    outcome_var = rlang::as_name(plot$mapping$y)
    formula = formula(plot)
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
      proportion = {
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
      ymax = proportion,
      xmin = bin_mid - bin_width * 0.4,
      xmax = bin_mid + bin_width * 0.4,
      alpha = scales::rescale(count, to = c(0.2, 1)),
      size = .5
    )
}

simulate_data = function(betas, cor, n=500, method = "logistic", nonlinear="interaction") {
  
  # Generate predictors
  x1 = rnorm(n)
  x2 = cor*x1 + rnorm(n, 0, sqrt(1-cor^2))
  
  if (nonlinear == "interaction") {
    beta_four = betas[4]*x1*x2
  } else {
    beta_four = betas[4]*x2*x2
  }
  # Create strong interaction in the logit
  if (method == "logistic") {
    logit_p = betas[1] + betas[2]*x1 + betas[3]*x2 + beta_four
    p = 1 / (1 + exp(-logit_p))
    y = rbinom(n, 1, p)
  } else {
    y = betas[1] + betas[2]*x1 + betas[3]*x2 + beta_four
    residual_variance = 1-var(y)
    y = y + rnorm(n, 0, .5)
    p = NA
  }
  
  d = data.frame(y, x1, x2, p)
  return(d)
}

# hybrid_bin_knn_smoother = function(data,
#                                    n_bins = 50,
#                                    min_bin_n = 5,
#                                    k_neighbors = 50) {
#   
#   # Step 1: identify places where count < min_bin_n
#   small_bin_size = data$count<min_bin_n
#   bin_breaks = seq(min(x1), max(x1), length.out = n_bins + 1)
#   bin_centers = head(bin_breaks, -1) + diff(bin_breaks)/2
#   bin_assignment = cut(x1, breaks = bin_breaks, include.lowest = TRUE)
#   
#   # Build lookup of X1_bin -> X1_center
#   X1_bin_levels = levels(bin_assignment)
#   X1_bin_centers_lookup = setNames(bin_centers, X1_bin_levels)
#   
#   # Step 2: summarize per X1 bin
#   summary_df = data.frame(
#     X1_bin = bin_assignment,
#     X1 = x1,
#     X2 = x2,
#     Y = y
#   ) %>%
#     group_by(X1_bin) %>%
#     summarize(
#       X1_center = X1_bin_centers_lookup[unique(X1_bin)],
#       X2_center = mean(X2),
#       mean_Y = mean(Y),
#       n_bin = n(),
#       .groups = "drop"
#     )
#   
#   # Step 3: smooth mean_Y for small-n bins using neighbors in X1 space
#   summary_df = summary_df %>%
#     mutate(
#       mean_Y_corrected = ifelse(n_bin >= min_bin_n, mean_Y,
#                                 sapply(1:n(), function(i) {
#                                   # Find neighbors in X1 space
#                                   X1_centers = summary_df$X1_center
#                                   Y_values   = summary_df$mean_Y
#                                   
#                                   distances = abs(X1_centers - X1_centers[i])
#                                   neighbor_indices = order(distances)[1:k_neighbors]
#                                   
#                                   mean(Y_values[neighbor_indices])
#                                 })
#       )
#     )
#   
#   # Return the summary_df for plotting
#   return(summary_df)
# }

# Helper function to build aesthetic mapping from original plot
build_inherited_aesthetics = function(plot, summary_data, base_x = "bin_mid", base_y = "proportion") {
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
  # need "unique" here because some of the quantiles have equal probabilities
  
  bin_breaks = quantile(x_vals, probs = seq(from=0, to=1, length.out=n_bins+1)) %>% unique
  bin_width = diff(bin_breaks)
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  return(list(bin_breaks = bin_breaks, bin_width=bin_width, bin_centers=bin_centers))
}

convert_formula_to_logistic_bins = function(formula, data, x_vals = NULL, n_bins = 10) {
  
  outcome_var = all.vars(formula)[1]
  ### extract given and axis variables
  given.axis = flexplot_axis_given(formula)
  given = given.axis$given
  axis = given.axis$axis
  variables_to_group_by = c(axis[2], given)[!is.na(c(axis[2], given))] %>% 
    na.omit() %>%
    purrr::map_vec(resolve_binned_var, data=data, return.name=T)
  
  
  # Calculate bin info for the x-axis variable
  if (is.null(x_vals)) x_vals = data[[axis[1]]]
  
  bin_info = calculate_bins_for_logistic_overlay(x_vals, n_bins)
  bin_breaks = bin_info$bin_breaks; bin_width = bin_info$bin_width; bin_centers = bin_info$bin_centers
  
  # Add bins to data
  data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
  
  # Create grouping variables for summarization
  group_vars = c("bin", variables_to_group_by)
  group_vars = group_vars[!is.na(group_vars)]
  
  # Summarize data with numeric-safe outcome
  summary_data = create_logistic_summary(data, group_vars, outcome_var, bin_centers, bin_width)
  return(list(summary_data=summary_data, data=data))
}


# d = simulate_data(c(1, .75, .25, 0), .5, n=1000)
# x = d$x1; y = d$y
# n_boot = 20
create_window_bootstrap_bins = function(x, y, n_boot = 1, k_neighbors = NULL, decorrelate = TRUE) {
  
  # Convert outcome to 0/1 if factor
  if (is.factor(y)) {
    y = as.numeric(y) - 1  # Convert factor to 0/1
  } else {
    y = as.numeric(y)  
  }
  
  # Compute bin centers
  n = length(x)
  bin_centers = seq(min(x), max(x), length.out = n)
  
  # Initialize matrix to store bootstrap results
  boot_bin_means = matrix(NA, nrow = n_boot, ncol = length(bin_centers))
  
  for (b in 1:n_boot) {
    # Resample indices with replacement
    boot_indices = sample(1:n, replace = TRUE)
    x_boot = x[boot_indices]
    y_boot = y[boot_indices]
    
    # Compute bin means for this bootstrap sample
    boot_bin_means[b, ] = sapply(bin_centers, function(center) {
      if (is.null(k_neighbors)) {
        # fallback: use fixed width bin (you can keep or remove this)
        sd_x = sd(x_boot)
        bin_width = 0.025 * sd_x
        in_bin = abs(x_boot - center) <= bin_width
        if (sum(in_bin) == 0) return(NA)
        mean(y_boot[in_bin])
      } else {
        # Use k-nearest neighbors
        distances = abs(x_boot - center)
        k_indices = order(distances)[1:k_neighbors]
        mean(y_boot[k_indices])
      }
    })
  }
  
  # Average bin means across bootstraps
  final_bin_means = if (n_boot == 1) {
    boot_bin_means[1, ]  # No averaging needed
  } else {
    colMeans(boot_bin_means, na.rm = TRUE)
  }
  
  # Fit logistic model to full data
  logistic_model = glm(y ~ x, family = binomial)
  logistic_pred = predict(logistic_model, newdata = data.frame(x = bin_centers), type = "response")
  
  # Decorrelate residuals if requested
  if (decorrelate) {
    residuals = final_bin_means - logistic_pred
    # Shuffle residuals (simple way to break autocorrelation)
    residuals_shuffled = sample(residuals)
    # Add trend back
    final_bin_means = logistic_pred + residuals_shuffled
  }
  
  # Prepare output dataframe
  return_dots = data.frame(bin_centers, bin_means = final_bin_means) %>%
    tidyr::drop_na(bin_means) %>%
    dplyr::filter(bin_means < 1 & bin_means > 0)
  
  return(return_dots)
}


# d = simulate_data(c(1, .75, 0, 0), 0, n=1000)
# bootstrapped_bins = create_window_bootstrap_bins(d$x1, d$y, n_boot=10, k_neighbors = 10)
# names(bootstrapped_bins) = c("x1", "observed")
# mod = glm(y~x1, data=d, family=binomial)
# bootstrapped_bins$fitted = predict(mod, newdata = bootstrapped_bins, type="response")
# bootstrapped_bins$residual = bootstrapped_bins$observed - bootstrapped_bins$fitted
# flexplot(y~x1, data=d, method="logistic", raw.data = F) +
#   geom_point(data=bootstrapped_bins, aes(x=x1, y = observed), alpha = .3)
# 
# flexplot(residual~fitted, data=bootstrapped_bins)
# flexplot(residual~x1, data=bootstrapped_bins)
# flexplot(fitted~x1, data=bootstrapped_bins)
# flexplot(observed~x1, data=bootstrapped_bins)
# flexplot(observed~fitted, data=bootstrapped_bins)


# now if I

# 
# 
# 
# 
# 
# 
# 
# 
# head(bootstrapped_bins)
# plot(d$x1, d$p, pch=16, col=alpha("blue", 0.3))
# 
# 
# 
# 
# 
# 
# 
# 













#
