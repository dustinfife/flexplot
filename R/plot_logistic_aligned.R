logistic_aligned = function(flex_obj, resolution = 100) {
  if (is.null(flex_obj$formula) || is.null(flex_obj$data)) {
    stop("This function only works with flexplot output that includes formula and data.")
  }
  
  formula = flex_obj$formula
  data    = flex_obj$data
  
  # Extract variable roles
  axis_given = flexplot_axis_given(formula)
  axis_vars  = axis_given$axis[!is.na(axis_given$axis)]
  given_vars = axis_given$given[!is.na(axis_given$given)]
  outcome    = all.vars(formula)[1]
  
  x_var      = resolve_binned_var(axis_vars[1], data)
  group_var  = if (length(axis_vars) > 1) resolve_binned_var(axis_vars[2], data) else NULL
  panel_vars = if (length(given_vars) > 0) vapply(given_vars, resolve_binned_var, character(1), data = data) else character(0)
  
  all_grouping_vars = c(group_var, panel_vars)
  all_model_vars    = c(x_var, all_grouping_vars)
  
  # Sanity check
  check_all_variables_exist_in_data(all_model_vars, data)
  
  # Fit logistic model with full interaction
  rhs = paste(all_model_vars, collapse = "*")
  model_formula = as.formula(paste(outcome, "~", rhs))
  model = glm(model_formula, data = data, family = binomial)
  
  # Create prediction grid
  grid = create_prediction_grid(all_model_vars, data)
  
  # Predict probabilities
  grid$prob = predict(model, newdata = grid, type = "response")
  
  # Create group ID (combo of group_var + panel_vars)
  group_id_vars = all_grouping_vars
  grid$group_id = if (length(group_id_vars) == 0) {
    "all"
  } else if (length(group_id_vars) == 1) {
    factor(grid[[group_id_vars[1]]])
  } else {
    apply(grid[group_id_vars], 1, paste, collapse = "_")
  }
  
  # Find threshold where p ≈ 0.5 for each group
  thresholds = find_thresholds(x_var, grid) 
  
  # Shift x-values so p = 0.5 aligns
  grid$x_shifted = shift_by_threshold(x = grid[[x_var]], group_id = grid$group_id, thresholds = thresholds)
  color_var = if (!is.null(group_var)) group_var else NULL
  
  # Plot
  library(ggplot2)
  p = ggplot(grid, aes(x = x_shifted, y = prob)) +
    geom_line(aes(color = !!sym(group_var)), size = 1.2, na.rm = TRUE)+
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    labs(
      x = paste0("Shifted ", x_var, " (aligned at p = 0.5)"),
      y = "Predicted Probability",
      title = "Threshold-Aligned Logistic Curves"
    ) +
    theme_bw()
  
  # Add paneling if needed
  if (length(panel_vars) == 1) {
    grid[[panel_vars[1]]] = factor(grid[[panel_vars[1]]])
    p = p + facet_wrap(as.formula(paste("~", panel_vars[1])))
  } else if (length(panel_vars) == 2) {
    grid[[panel_vars[1]]] = factor(grid[[panel_vars[1]]])
    grid[[panel_vars[2]]] = factor(grid[[panel_vars[2]]])
    p = p + facet_grid(as.formula(paste(panel_vars[2], "~", panel_vars[1])))
  }
  
  
  return(p)
}
