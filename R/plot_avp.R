##' Create an added variable plot
##'
##' Create an added variable plot
##'	
##' This function first residualizes the outcome variable based whatever variables the user decides to condition on.
##' The mean of the outcome variable is then added to the residuals (to maintain the interpretation of the variable),
##' then the function plots the residuals against the other variables the user specifies.
##' 
##' More specifically, if the user specifies `formula` but leaves the options `x` and `lm_formula` as NULL, it will 
##' default to residualizing the first variable(s) in the formula and plot the last variable entered against those residuals.
##' For example, if the user specifies y~x + z, this function will residualize y~x, then plot z against those residuals. 
##' 
##' If the user specifies a value of `x`, that tells the function which variable to residualize. So, again, if the user specifies
##' `y~x + z` then sets `x` to 2, the function will instead residualize based on z instead of x. 
##' 
##' For multivariate AVPs, the user can specify a value for `lm_formula`. The value of `lm_formula` specifies the fitted model
##' that is then residualized, while the value of `formula` specifies how the remaining variables are displayed, using `flexplot`
##' formula conventions.
##' @param formula A flexplot formula, specifying how the avp will visualize the variables. 
##' @param data The dataset used
##' @param lm_formula Optional. A formula specifying how to condition variables.  
##' @param method The smoothing method. Defaults to "loess"
##' @param x The variable you wish to place on the x axis. Defaults to NULL. 
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{flexplot}}
##' @author Dustin Fife
##' @export
##' @aliases added.plot added_plot avp
##' @import tibble
##' @return An added variable plot
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=exercise_data, x=2)
##' added.plot(weight.loss~motivation + therapy.type, 
##'      lm_formula = weight.loss~health*muscle.gain, data=exercise_data)
added.plot = added_plot = avp = function(formula, data, lm_formula=NULL, method="loess", x=NULL, ...){
  
	#### identify variable types
	variables = all.vars(formula)

	# do all the error checks
	check_all_variables_exist_in_data(variables, data)
	check_all_variables_exist_in_data(all.vars(lm_formula), data)
	check_variables_in_lm(formula, lm_formula)
	
	# prep data
	data = prep_data_for_avp(data, variables)
	formulae = make_avp_formula(formula, lm_formula, x)
	
  #### if they ask for logistic
	if (method == "logistic"){
	    fitted = fitted(glm(formulae[[1]], data=data, family=binomial))
	    data$residuals = factor.to.logistic(data=data, outcome=variables[[1]], method="logistic")[,variables[1]] - fitted
	    method = "loess"
	 } else {
	    data$residuals = residuals(lm(formulae[[1]], data=data)) + mean(data[,variables[1]])    
	 }
	

	#### plot it
	plot = flexplot(formulae[[2]], data=data, method=method, ...) + labs(y=formulae[[3]])
	class(plot) <- c("flexplot", class(plot))
	return(plot)
}


##' Create an added variable plot with binned residuals for logistic regression
##'
##' Create an added variable plot using binned percentage residuals instead of raw residuals.
##' This function first fits a logistic regression model to residualize the outcome variable 
##' based on conditioning variables, then bins the remaining predictor variable and calculates
##' binned percentage residuals (observed proportion - fitted proportion within each bin).
##' 
##' The function works similarly to added.plot but uses the binning logic from logistic_residual_plots
##' to create residuals that are more appropriate for logistic regression diagnostics.
##' 
##' @param formula A flexplot formula, specifying how the avp will visualize the variables. 
##' @param data The dataset used
##' @param lm_formula Optional. A formula specifying how to condition variables (the logistic model to fit first).  
##' @param method The smoothing method. Defaults to "loess"
##' @param x The variable you wish to place on the x axis. Defaults to NULL. 
##' @param scale Either "probability" (default) or "logit" to determine the scale of residuals.
##' @param n_bins Number of bins to use for calculating binned residuals. Defaults to 10.
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{added.plot}}, \code{\link{logistic_residual_plots}}
##' @author Dustin Fife
##' @export
##' @aliases logistic_added_plot
##' @import tibble
##' @return An added variable plot with binned residuals
##' @examples
##' # Assuming you have a binary outcome
##' # logistic_added_plot(outcome~predictor1 + predictor2, data=mydata)
##' # logistic_added_plot(outcome~predictor1 + predictor2, data=mydata, x=2)
##' # logistic_added_plot(outcome~predictor1 + predictor2, 
##' #      lm_formula = outcome~confounder1*confounder2, data=mydata)
##' # logistic_added_plot(outcome~predictor1 + predictor2, data=mydata, scale="logit")
logistic_added_plot = function(formula, data, lm_formula=NULL, method="loess", x=NULL, n_bins=10, scale="probability", ...){
  
  #### identify variable types
  variables = all.vars(formula)
  
  # do all the error checks (assuming these functions exist)
  check_all_variables_exist_in_data(variables, data)
  check_all_variables_exist_in_data(all.vars(lm_formula), data)
  check_variables_in_lm(formula, lm_formula)
  
  # prep data
  data = prep_data_for_avp(data, variables)
  formulae = make_avp_formula(formula, lm_formula, x)
  
  #### Fit the conditioning model (logistic regression)
  conditioning_model = glm(formulae[[1]], data=data, family=binomial)
  
  #### Get the remaining predictor variable (the one we'll bin on)
  # Extract variables from the plotting formula
  plot_vars = all.vars(formulae[[2]])
  x_var = plot_vars[length(plot_vars)]  # Last variable should be the x-axis variable
  outcome_var = variables[1]  # First variable from original formula is outcome
  
  # Get values for binning
  x_vals = data[[x_var]]
  
  # Convert outcome to 0/1 if factor
  observed = as.numeric(data[[outcome_var]])
  if (is.factor(data[[outcome_var]])) {
    observed = as.numeric(data[[outcome_var]]) - 1  # Convert factor to 0/1
  }
  
  #### Create bins based on X-axis predictor
  bin_info = calculate_bins_for_logistic_overlay(x_vals, n_bins)
  bin_breaks = bin_info$bin_breaks
  bin_centers = bin_info$bin_centers
  
  # Assign observations to bins
  data$bin = cut(x_vals, breaks = bin_breaks, include.lowest = TRUE)
  data$x_vals = x_vals
  data$observed = observed
  
  #### Calculate observed proportions within each X bin
  binned_data = data %>%
    group_by(bin) %>%
    summarise(
      x_center = mean(x_vals, na.rm = TRUE),
      observed_prop = mean(observed, na.rm = TRUE),
      n_obs = n(),
      .groups = 'drop'
    )
  
  #### Calculate fitted probabilities at bin centers using conditioning model
  # Create prediction data frame with all necessary variables
  # We need to get representative values for other predictors in each bin
  # Get column names excluding the ones we just added
  
  original_cols = setdiff(names(data), c("bin", "x_vals", "observed"))
  
  pred_data_full = data %>%
    group_by(bin) %>%
    summarise(
      across(all_of(original_cols), ~if(is.numeric(.x)) mean(.x, na.rm=TRUE) else {
        tbl = table(.x)
        names(tbl)[which.max(tbl)]  # Most common level for factors
      }),
      .groups = 'drop'
    )
  
  # Set the x variable to the bin centers
  pred_data_full[[x_var]] = binned_data$x_center
  
  # Get fitted probabilities from conditioning model
  fitted_at_x = predict(conditioning_model, newdata = pred_data_full, type = "response")
  
  #### Calculate binned residuals
  if (scale == "logit") {
    # Convert proportions to log odds, handling edge cases
    observed_logit = ifelse(binned_data$observed_prop == 0, 
                            log(0.5/binned_data$n_obs), # Use continuity correction for 0
                            ifelse(binned_data$observed_prop == 1,
                                   log((binned_data$n_obs - 0.5)/0.5), # Use continuity correction for 1
                                   log(binned_data$observed_prop / (1 - binned_data$observed_prop))))
    
    fitted_logit = log(fitted_at_x / (1 - fitted_at_x))
    
    binned_data = binned_data %>%
      mutate(
        fitted_prop = fitted_at_x,
        observed_logit = observed_logit,
        fitted_logit = fitted_logit,
        binned_residual = observed_logit - fitted_logit,
        se = sqrt(1 / (n_obs * fitted_prop * (1 - fitted_prop))) # SE for log odds
      )
  } else {
    # Probability scale (original)
    binned_data = binned_data %>%
      mutate(
        fitted_prop = fitted_at_x,
        binned_residual = observed_prop - fitted_prop,
        se = sqrt(fitted_prop * (1 - fitted_prop) / n_obs)
      )
  }
  
  # Remove empty bins
  binned_data = binned_data[!is.na(binned_data$binned_residual), ]
  
  #### Create the plot data
  # Add mean of original outcome to maintain interpretation (like in original added.plot)
  if (scale == "logit") {
    # For log odds scale, add the overall log odds
    overall_prop = mean(observed, na.rm = TRUE)
    original_mean = log(overall_prop / (1 - overall_prop))
    y_label = paste("Binned Log Odds Residuals +", round(original_mean, 3))
  } else {
    # For probability scale, use the proportion of 1s
    original_mean = mean(observed, na.rm = TRUE)
    y_label = paste("Binned Residuals +", round(original_mean, 3))
  }
  
  binned_data$residuals = binned_data$binned_residual + original_mean
  
  # Create a modified dataset for plotting
  plot_data = binned_data
  names(plot_data)[names(plot_data) == "x_center"] = x_var
  
  #### Create the flexplot formula for plotting
  plot_formula = as.formula(paste("residuals ~", x_var))
  
  #### Plot it using flexplot (assuming flexplot can handle this)
  plot = flexplot(plot_formula, data=plot_data, method=method, ...) + 
    labs(
      y = y_label,
      title = paste("Added Variable Plot with Binned Residuals (", 
                    ifelse(scale == "logit", "Log Odds", "Probability"), 
                    " Scale)", sep="")
    )
  
  class(plot) <- c("flexplot", class(plot))
  
  # Add some useful attributes
  attr(plot, "binned_data") = binned_data
  attr(plot, "conditioning_model") = conditioning_model
  attr(plot, "n_bins") = n_bins
  attr(plot, "scale") = scale
  
  return(plot)
}