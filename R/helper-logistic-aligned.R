resolve_binned_var = function(var, data, return.name=FALSE) {

  binned_name = paste0(var, "_binned")
  
  # Return binned name if it's in the original dataset
  if (binned_name %in% names(data)) return(binned_name)
  
  # Return unbinned name if it's in the dataset
  if (return.name & var %in% names(data)) return(var)
  if (!return.name & var %in% names(data)) return(NA)
  
  stop(sprintf("Could not resolve '%s' or '%s' in the dataset.", var, binned_name))
}

convert_cut_to_flexplot_labels = function(x) {
  gsub("\\[|\\]|\\(|\\)", "", x) |>    # remove all brackets
    strsplit(",") |> 
    vapply(function(parts) paste(parts, collapse = "-"), character(1))
}

create_prediction_grid = function(all_model_vars, data, resolution=100) {
  var_levels = all_model_vars %>% 
    purrr::map(~ return_ranges_or_unique(.x, data=data, resolution=resolution)) %>%
    purrr::set_names(all_model_vars)
  grid = expand.grid(var_levels)
  return(grid)
}

return_ranges_or_unique = function(var, data, means=NULL, resolution=100) {
  
  if (length(var) == 1){
    ## if it's not numeric, return unique values
    if (!is.numeric(data[[var]])) return(unique(data[[var]]))
    
    # otherwise span the range of the variable
    rng = range(data[[var]], na.rm = TRUE)
    return(seq(rng[1], rng[2], length.out = resolution))
  }
  
  binned_name   = names(var)[1]
  unbinned_name = gsub("_binned", "", binned_name)
  
  # compute mean within category
  mean_within_bin = data %>%
    group_by(!!sym(binned_name)) %>%
    summarize(across(!!sym(unbinned_name), mean))
  
  return(mean_within_bin[[unbinned_name]])
  
  

}


compute_logistic_threshold = function(row, x_var, model) {
  
  coefs = coef(model)
  terms_obj = delete.response(terms(model))
  
model
  row[[x_var]] = 0
  row0 = model.frame(terms_obj, data = row, na.action = na.pass)
  mm0 = model.matrix(terms_obj, row0)
  
  row[[x_var]] = 1
  row1 = model.frame(terms_obj, data = row, na.action = na.pass)
  mm1 = model.matrix(terms_obj, row1)
  
  # Compute the shift in linear predictor from changing x
  A = as.numeric(mm0 %*% coefs)
  B = as.numeric((mm1 - mm0) %*% coefs)
  
  
  if (B == 0) return(NA_real_)
  return(-A / B)
}

parse_bin_midpoint = function(bin_label) {
  nums = as.numeric(unlist(regmatches(bin_label, gregexpr("[0-9.]+", bin_label))))
  mean(nums)
}

find_thresholds = function(model, grid, flex_obj) {
  group_var = "group_id"

  all_variables = flexplot_axis_given(flex_obj$formula) #%>%
  x_var         = all_variables$axis[1]
  data          = flex_obj$data
  group_var     = all_variables$axis[2] %>% resolve_binned_var(data)
  
  if (is.numeric(data[[group_var]])) {
    # Detect binning vars and raw vars
    binned_vars = grep("_binned$", names(data), value = TRUE)
    raw_vars = gsub("_binned$", "", binned_vars)
  
    # Compute mean of raw vars for each binned combination
    grid$group_id = convert_cut_to_flexplot_labels(as.character(grid$group_id))
    data$group_id = convert_cut_to_flexplot_labels(as.character(data[[group_var]]))
    bin_means = data %>%
      group_by(group_id) %>%
      summarize(across(all_of(c(raw_vars)), mean, na.rm = TRUE), .groups = "drop")
    # reformat the bin means
    bin_map = split(bin_means[, -1], bin_means$group_id)
  } else {
    
    data$group_id = data[[group_var]]
    bin_map = unique(data[[group_var]])%>%as.list %>% purrr::set_names(unique(data[[group_var]]))
  }
  
  group_ids = unique(as.character(grid[[group_var]]))
  
  thresholds = vapply(
    group_ids,
    compute_threshold,
    numeric(1),
      # the remainder are passed to compute_threshold
      bin_map = bin_map,
      x_var = x_var,
      model = model,
      bin_var_names = raw_vars,
      data = data  
  )
  
  names(thresholds) = group_ids
  return(thresholds)
}

compute_threshold = function(group_id, bin_map, x_var, model, bin_var_names, data) {
  
  coefs = coef(model)
  terms_obj = delete.response(terms(model))
  
  # extract the means
  fixed_vals = bin_map[[group_id]]
  if (is.null(fixed_vals)) return(NA_real_)
  
  # Use a template row from real data to preserve types
  template = data[1, all.vars(terms_obj), drop=FALSE]
  
  # replace the original row score with the mean of the binned variable
  for (v in names(fixed_vals)) {
    template[[v]] = fixed_vals[[v]]
  }

  # now put 0/1 as the x values
  template[[x_var]] = 0
  row0 = model.frame(terms_obj, data = template, na.action = na.pass)
  template[[x_var]] = 1
  row1 = model.frame(terms_obj, data = template, na.action = na.pass)
  
  mm0 = model.matrix(terms_obj, row0)
  mm1 = model.matrix(terms_obj, row1)
  
  A = as.numeric(mm0 %*% coefs)
  B = as.numeric((mm1 - mm0) %*% coefs)
  
  if (B == 0) return(NA_real_)
  return(-A / B)
}




shift_by_threshold = function(grid, x_var, thresholds) {
  # thresholds is a named numeric vector (e.g., c("[4.2,4.9]" = 0.5646, ...))

  # Convert thresholds to a data frame for joining
  threshold_df = tibble(
    group_id = names(thresholds),
    threshold = thresholds
  )
  
  grid = grid %>%
    mutate(group_id = as.character(group_id)) %>% # ensure join compatibility
    left_join(threshold_df, by = "group_id") %>%
    mutate(x_shifted = !!sym(x_var) - threshold)
  
  return(grid)
}

# Extract bin labels like "4.2-4.9" and convert to numeric breaks
replicate_bins_from_flexplot = function(variables, grid, data) {
  
  ## find variables that have "_binned" in the names
  matched_vars = variables[variables %in% names(data)]
  
  bin_components = lapply(matched_vars, function(v) {
    compute_bin_component(v, grid, data)
  })
  
  for (comp in bin_components) {
    grid[[comp$var[1]]] = comp$value
  }
  
  return(grid)
}

compute_bin_component = function(var, grid, data) {
  
  # variable comes in with _binned already
  binned_name = var
  var = gsub("_binned", "", var)
  
  # extract labels from flexplot data
  labels = levels(data[[binned_name]])
  
  # check if labels contain character -
  # if they don't exit prematurely
  if(length(grep("-", labels[1]))==0) return(tibble::tibble(var=var, value = grid[[var]]))
  
  # convert labels to breakpoints
  breaks = sort(unique(as.numeric(unlist(strsplit(labels, "-")))))
  
  # cut the grid values so they match flexplot's
  binned_values = cut(grid[[var]], breaks = breaks, include.lowest = TRUE)
  
  # convert (x,x) to x-x
  converted = convert_cut_to_flexplot_labels(binned_values)
  
  tibble::tibble(var = binned_name, value = converted)
}



dash_to_cut = function(label, open_left = TRUE) {
  parts = strsplit(label, "-")[[1]]
  left = parts[1]
  right = parts[2]
  left_bracket = if (open_left) "(" else "["
  paste0(left_bracket, left, ",", right, "]")
}


#' Build group ID from binned variables
#'
#' Creates a unique group ID column by combining binned versions of grouping and panel variables.
#'
#' @param grid Data frame of predicted values
#' @param group_var The grouping variable (e.g., color in plot)
#' @param panel_vars Character vector of paneling variables (facet_wrap/grid)
#' @return A character vector of group IDs
build_group_id = function(grid, group_var = NULL, panel_vars = character()) {
  group_id_vars = character()

  
  # group together all "binned" variables
  if (!is.null(group_var)) group_id_vars = c(group_id_vars, group_var)
  if (length(panel_vars) > 0) group_id_vars = c(group_id_vars, panel_vars)
  
  
  if (length(group_id_vars) == 0) return(rep("all", nrow(grid)))
  if (length(group_id_vars) == 1) return(factor(grid[[group_id_vars[1]]]))
  
  return(apply(grid[group_id_vars], 1, paste, collapse = "_"))
}
