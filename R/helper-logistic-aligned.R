resolve_binned_var = function(var, data) {
  binned_name = paste0(var, "_binned")
  
  # Prefer binned if both exist
  if (binned_name %in% names(data)) {
    return(binned_name)
  }
  
  if (var %in% names(data)) {
    return(var)
  }
  
  stop(sprintf("Could not resolve '%s' or '%s' in the dataset.", var, binned_name))
}


return_ranges_or_unique = function(var, data, resolution=100) {
  if (is.numeric(data[[var]])) {
    rng = range(data[[var]], na.rm = TRUE)
    seq(rng[1], rng[2], length.out = resolution)
  } else {
    unique(data[[var]])
  }
}
create_prediction_grid = function(all_model_vars, data, resolution=100) {
  var_levels = all_model_vars %>% purrr::map(~ return_ranges_or_unique(.x, data=data, resolution=resolution))
  names(var_levels) = all_model_vars
  grid = expand.grid(var_levels)
  return(grid)
}


find_thresholds = function(x_var, grid) {
  group_ids = unique(grid$group_id)
  thresholds = setNames(numeric(length(group_ids)), group_ids)
  for (gid in group_ids) {
    subset_data = grid[grid$group_id == gid, ]
    if (nrow(subset_data) == 0 || all(is.na(subset_data$prob))) {
      thresholds[gid] = NA
    } else {
      idx = which.min(abs(subset_data$prob - 0.5))
      thresholds[gid] = subset_data[[x_var]][idx]
    }
  }
  return(thresholds)
}

shift_by_threshold = function(x, group_id, thresholds) {
  if (length(x) != length(group_id)) {
    stop("x and group_id must be the same length.")
  }
  
  mapply(function(xval, gid) {
    if (is.na(thresholds[gid])) return(NA)
    xval - thresholds[gid]
  }, x, group_id)
}


