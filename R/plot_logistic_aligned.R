#' Align Logistic Regression Curves at the Threshold
#'
#' This function extracts the data and formula from a \code{flexplot()} object,
#' fits a logistic regression with all interactions, and then plots the resulting
#' curves with their \eqn{p = 0.5} thresholds aligned on the x-axis. This improves
#' visual comparison of slopes by eliminating left/right intercept shifts.
#'
#' The alignment is done by identifying the x-value where each curve crosses
#' \eqn{p = 0.5} and shifting all predicted x-values so that each curve aligns at that
#' point. The resulting plot displays all curves horizontally centered at the
#' logistic threshold.
#'
#' @param flex_obj A \code{ggplot} object returned by \code{flexplot()} that includes
#'   both \code{formula} and \code{data} attributes (set automatically by \code{flexplot()}).
#' @param resolution The number of evenly spaced x-points to generate per group for plotting
#'   the logistic curves. Defaults to 100.
#'
#' @return A \code{ggplot} object showing threshold-aligned logistic regression curves,
#'   optionally paneled by conditioning variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d = data.frame(
#'   y = factor(rbinom(200, 1, 0.5), labels = c("no", "yes")),
#'   x = runif(200, -3, 3),
#'   g = rep(c("A", "B"), each = 100)
#' )
#' 
#' p = flexplot(y ~ x + g, data = d, method = "logistic")
#' logistic_aligned(p)
#' }
logistic_aligned = function(flex_obj, resolution = 100) {
  
  if (is.null(flex_obj$formula) || is.null(flex_obj$data)) {
    stop("This function only works with flexplot output that includes formula and data.")
  }

  formula = flex_obj$formula
  data    = flex_obj$data
  
  # Extract variable roles
  axis_given = flexplot_axis_given(formula)
  predictors = unlist(axis_given)[!is.na(unlist(axis_given))]
  axis_vars  = axis_given$axis[!is.na(axis_given$axis)]
  given_vars = axis_given$given[!is.na(axis_given$given)]
  outcome    = all.vars(formula)[1]
  x_var      = axis_vars[1]

  # Fit logistic model with full interaction
  rhs = paste(predictors, collapse = "*")
  model_formula = as.formula(paste(outcome, "~", rhs)) # use non-binned version for the model
  model = glm(model_formula, data = data, family = binomial)
  
  # identify all "binned" variables (so we know which we have to average within)
  i_am_binned = predictors %>% purrr::map_chr(.f=resolve_binned_var, data=data) %>% na.omit
  raw_unbinned_name = gsub("_binned", "", i_am_binned)
  
  # compute means of X axis for each binned variable
  binned_means <- purrr::imap(i_am_binned, function(group_var, name) {
    value_var <- raw_unbinned_name[[name]]
    
    data %>%
      group_by(.data[[group_var]]) %>%
      summarize(mean_value = mean(.data[[value_var]], na.rm = TRUE), .groups = "drop")
  })
  names(binned_means) <- purrr::map_chr(binned_means, ~ names(.)[1])
  

  # identify all variables that still need values for the grid
  unbinned_variables = predictors[!(predictors %in% raw_unbinned_name)]
  unbinned_ranges_or_unique = unbinned_variables %>% 
    purrr::map(return_ranges_or_unique, data=data) %>%
    purrr::set_names(unbinned_variables)
  binned_ranges_or_unique = binned_means %>%
    purrr::map(return_ranges_or_unique, data=data) %>%
    purrr::set_names(raw_unbinned_name)
  
  all.ranges = c(unbinned_ranges_or_unique, binned_ranges_or_unique)
  grid = expand.grid(all.ranges, KEEP.OUT.ATTRS=FALSE, stringsAsFactors = FALSE)
  condition_variables = predictors[!(predictors %in% x_var)] %>% as.vector
  conditions = expand.grid(all.ranges[condition_variables], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    conditions[[x_var]] = NA

  for (i in 1:nrow(conditions)) {
    conditions[[i,x_var]] = compute_logistic_threshold(conditions[i,], x_var, model)
  }
  # rename conditions (for joining)
  names(conditions)[names(conditions)==x_var] = "threshold"
    
    
    # merge conditions and grid before plotting
  if (!is.factor(data[[axis_vars[2]]])) {
    all_grid = dplyr::inner_join(grid, conditions, by=condition_variables) %>%
      mutate(!!sym(axis_vars[2]) := factor(round(.data[[axis_vars[2]]], digits=2)))
  } else {
    all_grid = dplyr::inner_join(grid, conditions, by=condition_variables) 
  }
  
  # predict values
  all_grid$prob = predict(model, newdata = grid, type = "response")

  # shift predictions
  all_grid$x_shifted = NA
  all_grid$x_shifted = all_grid[[x_var]] - all_grid$threshold

  # add extensions to lines
  # For each group of binned values (i.e., each row of `conditions`)
  extension_grid = purrr::pmap_dfr(conditions, function(...){
    row = tibble::tibble(...)
    threshold = row$threshold
    
    # create sequence for x_var
    x_vals = seq(threshold - 200, threshold + 200, length.out = resolution)
    
    # replicate other variables
    extended = row[rep(1, length(x_vals)), ]
    extended[[x_var]] = x_vals
    extended$x_shifted = x_vals - threshold
    
    return(extended)
  })
  
  extension_grid$prob = predict(model, newdata = extension_grid, type = "response")
  extension_grid$type = "dotted"
  all_grid$type = "solid"
  
  if (!is.factor(data[[axis_vars[2]]])) {
    extension_grid[[axis_vars[2]]] = factor(round(extension_grid[[axis_vars[2]]], digits=2))
  }
  plot_data = rbind(all_grid, extension_grid)

  
  p = ggplot(plot_data, aes(x = x_shifted, y = prob)) +
    geom_line(aes(linetype = type, color = !!sym(axis_vars[2])), size = 1.2, na.rm = TRUE) +
    scale_linetype_manual(values = c(solid = "solid", dotted = "dotted")) + 
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    labs(
      x = paste0("Shifted ", x_var, " (aligned at p = 0.5)"),
      y = "Predicted Probability",
      title = "Threshold-Aligned Logistic Curves"
    ) +
    theme_bw()+ guides(linetype = "none")


  # Add paneling if needed
  if (length(given_vars) == 1) {
    all_grid[[given_vars[1]]] = factor(all_grid[[given_vars[1]]])
    p = p + facet_wrap(as.formula(paste("~", given_vars[1])))
  } else if (length(panel_vars) == 2) {
    grid[[given_vars[1]]] = factor(grid[[given_vars[1]]])
    grid[[given_vars[2]]] = factor(grid[[given_vars[2]]])
    p = p + facet_grid(as.formula(paste(given_vars[2], "~", given_vars[1])))
  }
  
  return(p)
}
