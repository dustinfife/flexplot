#' Visualize a fitted GLM model 
#'
#' Visualize a fitted GLM model, with special handling for logistic regression
#' @param object a glm object
#' @param plot what should be plotted? Residuals? Model plot? All of them?
#' @param formula A flexplot-style formula
#' @param plots.as.list Should the plots be returned as a list? Defaults to FALSE.
#' @param n_bins Number of bins for logistic regression overlay (when applicable)
#' @param overlay_type Type of overlay for logistic regression ("dot" or "bar")
#' @param ... Other arguments passed to flexplot
#' @return a plot containing a visual of the chosen model
#' @export
visualize.glm = function(object, plot=c("all", "residuals", "model"), formula = NULL, plots.as.list=FALSE, n_bins=10, overlay_type="dot", ...){

  plot = match.arg(plot, c("all", "residuals", "model"))
  
  d = object$model
  data = object$model
  variables = all.vars(formula(object))
  outcome = variables[1]
  predictors = variables[-1]
  
  # Check if this is a binomial GLM (logistic regression)
  is_logistic = object$family$family == "binomial"
  
  ## see if all predictors are categorical
  dv_is_factor = check.non.number(data[,outcome])
  all_ivs_factors = all(variable_types(predictors, data)$characters)
  if (dv_is_factor & all_ivs_factors & !is_logistic) {
    stop("Well, darn. You've found a limitation of flexplot. Flexplot cannot use visualize when
         all your variables are categorical. Sorry!")
  }
  
  #### use flexplot to visualize a model
  if ((plot=="all" | plot == "model" ) & is.null(formula)){
    
    #### generate formula as best we can
    #### get dataset
    
    #### now decide where things go
    if (length(predictors)>4){
      message("Note: to visualize more than four variables, I'm going to do an 'added variable plot.'")
      
      f = object$call[[2]]
      if (is_logistic) {
        # For logistic regression with many predictors, still use added variable plot
        # but may need special handling for residuals
        step3 = added.plot(f, data=d, ...) + labs(title="Analysis Plot")
      } else {
        step3 = added.plot(f, data=d, ...) + labs(title="Analysis Plot")
      }
      class(step3) <- c("flexplot", class(step3))
      return(step3)
    } else {
      
      f = make_flexplot_formula(predictors, outcome, data)
      
      if (is_logistic) {
        # Use logistic overlay instead of compare.fits for binomial models
        step3 = logistic_overlay(formula = f, data = data, n_bins = n_bins, type = overlay_type, ...)
        step3 = step3 + labs(title="Logistic Regression Analysis")
      } else {
        # Use existing compare.fits for other GLM families
        step3 = compare.fits(f, data=data, model1=object, ...) + labs(title="Analysis Plot")
      }
      
      ### if they have more than two variables, also include a added variable plot
      # if (length(predictors)>1){
      #   if (is_logistic) {
      #     # For logistic, added variable plot might need special residual handling
      #     step3b = added.plot(f, data=d, ...)+ labs(title="Added Variable Plot")
      #   } else {
      #     step3b = added.plot(f, data=d, ...)+ labs(title="Added Variable Plot")
      #   }
      #   step3 = cowplot::plot_grid(step3, step3b, rel_widths=c(.6, .4))
      # }
    }
    
  } else if (plot=="all" | plot=="model"){
    if (is_logistic) {
      step3 = logistic_overlay(formula = formula, data = data, n_bins = n_bins, type = overlay_type, ...)
      step3 = step3 + labs(title="Logistic Regression Analysis")
    } else {
      step3 = compare.fits(formula, data=data, model1=object, ...) + labs(title="Analysis Plot")
    }
    
    ### if they have more than two variables, also include a added variable plot
    if (length(predictors)>1){
      step3b = added.plot(formula, data=d, ...)+ labs(title="Added Variable Plot")
      step3 = cowplot::plot_grid(step3, step3b, rel_widths=c(.6, .4))			
    }		
  }
  
  if (plot=="residuals"){
    if (is_logistic) {
      # Create logistic-specific residual plots using binned residuals
      res.plots = logistic_residual_plots(d, object, n_bins = n_bins, ...)
    } else {
      # Use existing residual plots for other GLM families
      res.plots = residual.plots(d, object, ...)
    }
    
    p = arrange.plot(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl, step3=NULL, plot=plot, terms=res.plots$terms, numbers=res.plots$numbers)
    if (plots.as.list){
      list(histo=res.plots$histo, res.dep=res.plots$res.dep, sl=res.plots$sl)
    } else {
      return(p)
    }
  } else if (plot=="model"){
    return(step3)
  } else {
    if (is_logistic) {
      res.plots = logistic_residual_plots(d, object, n_bins = n_bins, ...)
    } else {
      res.plots = residual.plots(d, object, ...)
    }
    p = arrange.plot(res.plots$histo, res.plots$res.dep, res.plots$sl, step3, plot, res.plots$terms, res.plots$numbers)
    return(p)
  }
}

#' Create residual plots for logistic regression models
#'
#' Creates a set of residual plots specifically designed for logistic regression,
#' using binned residuals instead of raw residuals which are not meaningful for binary outcomes.
#' @param data The model data frame
#' @param object The fitted glm object (binomial family)
#' @param n_bins Number of bins for creating binned residuals
#' @param ... Additional arguments
#' @return A list containing histogram, residual dependence plot, and scale-location plot components
#' @details This function creates binned residuals by:
#' \itemize{
#'   \item Binning observations based on fitted probabilities
#'   \item Computing observed proportions within each bin
#'   \item Calculating residuals as observed - fitted proportions
#' }
logistic_residual_plots = function(data, object, n_bins = 10, ...) {
  
  # Extract basic info
  variables = all.vars(formula(object))
  outcome = variables[1]
  predictors = variables[-1]
  
  # Get the predictor variable (first predictor for now)
  main_predictor = predictors[1]
  x_vals = data[[main_predictor]]
  
  # Convert outcome to 0/1 if factor
  observed = as.numeric(data[[outcome]]) # Convert to 0/1 if factor
  if (is.factor(data[[outcome]])) {
    observed = as.numeric(data[[outcome]]) - 1  # Convert factor to 0/1
  }
  
  # Create bins based on X-axis predictor (same as logistic_overlay does)
  bin_info = calculate_bins_for_logistic_overlay(x_vals, n_bins)
  bin_breaks = bin_info$bin_breaks
  bin_centers = bin_info$bin_centers
  
  # Assign observations to bins based on X predictor
  data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
  data$x_vals = x_vals
  data$observed = observed
  
  # Calculate observed proportions within each X bin
  binned_data = data %>%
    group_by(bin) %>%
    summarise(
      x_center = mean(x_vals, na.rm = TRUE),  # X location of bin
      observed_prop = mean(observed, na.rm = TRUE),  # Observed proportion in bin
      n_obs = n(),
      .groups = 'drop'
    )
  
  # Create prediction data frame with all necessary variables
  # Get all predictor names from the model
  all_predictors = predictors  # This should include all predictors from the model
  
  # Create a data frame with representative values for all predictors
  # For each bin, use mean values for numeric predictors and mode for factors
  pred_data_full = data %>%
    group_by(bin) %>%
    summarise(
      across(all_of(all_predictors), ~if(is.numeric(.x)) mean(.x, na.rm=TRUE) else {
        tbl = table(.x)
        names(tbl)[which.max(tbl)]  # Most common level for factors
      }),
      .groups = 'drop'
    )
  
  # Set the main predictor to the bin centers
  pred_data_full[[main_predictor]] = binned_data$x_center
  
  # Get fitted probabilities at bin centers
  fitted_at_x = predict(object, newdata = pred_data_full, type = "response")
  
  # Calculate residuals: observed - fitted at each X location
  binned_data = binned_data %>%
    mutate(
      fitted_prop = fitted_at_x,
      residual = observed_prop - fitted_prop,
      se = sqrt(fitted_prop * (1 - fitted_prop) / n_obs), # Standard error for binomial
      abs_residual = abs(residual)
    )
  
  # Remove empty bins
  binned_data = binned_data[!is.na(binned_data$residual), ]
  
  # 1. Histogram of binned residuals (equivalent to residual histogram)
  histo = ggplot(binned_data, aes(x = residual)) +
    geom_histogram(bins = max(8, min(15, nrow(binned_data)/2)), 
                   alpha = 0.7, fill = "lightblue", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Distribution of Binned Residuals",
      x = "Binned Residuals (Observed - Fitted Proportion)",
      y = "Count"
    ) +
    theme_bw()
  
  # 2. Residual dependence plot (binned residuals vs fitted)
  res.dep = ggplot(binned_data, aes(x = x_center, y = residual)) +
    geom_point(size = 3, alpha = 0.7) +  # Fixed size since bins are equal
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    labs(
      title = "Binned Residuals vs Predictor",
      x = main_predictor,
      y = "Binned Residuals"
    ) +
    theme_bw()
  
  # 3. Scale-location plot (sqrt of absolute binned residuals vs fitted)
  sl = ggplot(binned_data, aes(x = x_center, y = sqrt(abs_residual))) +
    geom_point(size = 3, alpha = 0.7) +  # Fixed size since bins are equal
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    labs(
      title = "Scale-Location Plot",
      x = main_predictor, 
      y = "√|Binned Residuals|"
    ) +
    theme_bw()
  
  # Calculate terms and numbers (similar to what residual.plots might return)
  terms = length(predictors)
  numbers = list(
    n_bins = n_bins,
    n_obs = nrow(data),
    mean_residual = mean(binned_data$residual, na.rm = TRUE),
    sd_residual = sd(binned_data$residual, na.rm = TRUE)
  )
  
  # Return list in same format as residual.plots
  return(list(
    histo = histo,
    res.dep = res.dep, 
    sl = sl,
    terms = terms,
    numbers = numbers,
    binned_data = binned_data  # Extra component that might be useful
  ))
}