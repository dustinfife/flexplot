#' Magnet Plot: Stacked Dot Plot for Binary Outcome with Logistic Fit
#'
#' Creates a "magnet plot" — a proportional stacked dot plot for a binary outcome 
#' variable (0/1), scaled relative to the group with the largest sample size. The function
#' also overlays the predicted probabilities from a logistic regression model and includes
#' optional horizontal jittering and transparency for dot visibility. 
#'
#' @param formula A formula in the form `Y ~ X`, where `Y` is a binary variable (0/1) and `X` is a categorical predictor.
#' @param data A data frame containing the variables specified in the formula.
#' @param jitter Logical; if `TRUE`, dots will be horizontally jittered to reduce overplotting. Default is `TRUE`.
#' @param alpha Numeric; transparency level for dots (between 0 and 1). Default is `0.6`.
#' @param dot_size Numeric; size of the dots. Default is `2`.
#'
#' @return A `ggplot` object showing:
#'   - Stacked dots representing individual observations, scaled relative to the group with the maximum sample size.
#'   - A dashed horizontal line at y = 0.5.
#'   - Blue points indicating logistic regression predicted probabilities for each group.
#'   - Sample size labels above each group.
#'
#' @details The Y-axis is bounded between 0 and 1, with dots for `Y = 0` stacking upward from 0 and dots for `Y = 1` stacking downward from 1.
#' The height of each stack is proportional to both the proportion of 0/1 outcomes within each group and the total sample size of that group,
#' relative to the group with the largest sample size.
#'
#' The function assumes that `Y` is binary-coded as 0 and 1. If it is not, you should recode it prior to using this function.
#'
#' @examples
#' set.seed(123)
#' df = data.frame(
#'   X = rep(c("A", "B", "C"), times = c(50, 300, 100)),
#'   Y = c(
#'     rbinom(50, 1, 0.8),
#'     rbinom(300, 1, 0.2),
#'     rbinom(100, 1, 0.5)
#'   )
#' )
#' magnet_plot(Y ~ X, data = df)
#'
#' @export
magnet_plot = function(formula, data,
                                 jitter = TRUE,
                                 alpha = 0.6,
                                 dot_size = 2) {
  
  variables = all.vars(formula, unique=FALSE)
  yvar = variables[1]
  xvar = variables[2]
  
  # Get variable names as symbols
  xvar_sym = rlang::ensym(xvar)
  yvar_sym = rlang::ensym(yvar)
  
  # Check that Y is binary 0/1
  y_values = unique(data[[yvar]])
  if (!all(y_values %in% c(0, 1))) {
    stop("The response variable must be binary and coded as 0/1.")
  }
  
  # Check that x is categorical
  if (is.numeric(data[[xvar]])) data[[xvar]] = factor(data[[xvar]])
  
  
  # Compute proportion within each X group and scaling factor
  df_summary = data %>%
    group_by(!!xvar_sym) %>%
    summarise(n_total = n(),
              n_0 = sum(!!yvar_sym == 0),
              n_1 = sum(!!yvar_sym == 1),
              prop_0 = n_0 / n_total,
              prop_1 = n_1 / n_total,
              .groups = "drop")
  
  # Compute max N across all groups
  max_n_total = max(df_summary$n_total)
  
  df_summary = df_summary %>%
    mutate(scaling_factor = n_total / max_n_total)
  
  # Join proportions + scaling factor back to original data
  df_scaled = data %>%
    left_join(df_summary, by = rlang::as_name(xvar_sym)) %>%
    group_by(!!xvar_sym, !!yvar_sym) %>%
    mutate(stack_pos = row_number()) %>%
    ungroup()
  
  # Compute scaled Y position
  df_scaled = df_scaled %>%
    mutate(
      y_scaled = case_when(
        !!yvar_sym == 0 ~ 0 + (stack_pos - 0.5) / n_0 * (prop_0 * scaling_factor * 0.5),
        !!yvar_sym == 1 ~ 1 - (stack_pos - 0.5) / n_1 * (prop_1 * scaling_factor * 0.5)
      )
    )
  
  # Fit logistic regression
  formula = as.formula(paste(rlang::as_name(yvar_sym), "~", rlang::as_name(xvar_sym)))
  model = glm(formula, data = data, family = binomial)
  
  # Predicted probabilities
  x_levels = levels(factor(data[[rlang::as_name(xvar_sym)]]))
  


  
  df_pred = data.frame(
    X = x_levels,
    pred_prob = predict(model, newdata = setNames(data.frame(x_levels), rlang::as_name(xvar_sym)), type = "response")
  ) %>% arrange(desc(pred_prob))
  
  # Base ggplot object
  p = ggplot() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    labs(title = "Magnet Plot",
         y = rlang::as_name(yvar_sym),
         x = rlang::as_name(xvar_sym)) +
    theme_bw()

  # order x-axis by predicted probabilities
  if (class(df_scaled[[xvar]])[1] != "ordered") {
    df_scaled[[xvar]] = factor(df_scaled[[xvar]], levels = df_pred$X, ordered=T)  
  }
  
  
  # Dot style options
  point_shape = 1
  point_position = if (jitter) position_jitter(width = 0.05) else position_identity()
  
  # Add stacked dots
  p = p + geom_point(data = df_scaled,
                     aes(x = !!xvar_sym, y = y_scaled),
                     color = "black",
                     alpha = alpha,
                     shape = point_shape,
                     size = dot_size,
                     position = point_position)
  
  # Add predicted probabilities
  p = p + geom_point(data = df_pred, 
                     aes(x = X, y = pred_prob), 
                     color = "blue", size = 3)
  
  # Add sample size labels
  p = p + geom_text(data = df_summary,
                    aes(x = !!xvar_sym, y = 0.52, label = paste0("N=", n_total)),
                    vjust = 0, size = 3.5)
  
  # Return plot
  return(p)
}