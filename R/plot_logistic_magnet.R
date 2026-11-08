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
  
  variables = all.vars(formula)
  yvar = variables[1]
  xvar = variables[2]
  
  # Extract optional panel variable
  rhs = formula[[3]]
  panelvar = NULL
  
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    panel_vars = all.vars(rhs[[3]])
    
    if (length(panel_vars) != 1) {
      stop("Magnet plots support one paneling variable.")
    }
    
    panelvar = panel_vars[1]
  }
  
  # Validate and prepare data
  data = check_magnet_errors(data, yvar, xvar)
  
  required_vars = c(yvar, xvar, panelvar)
  missing_vars = setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ",
         paste(missing_vars, collapse = ", "))
  }
  
  data = data[stats::complete.cases(data[, required_vars, drop = FALSE]), ,
              drop = FALSE]
  
  if (nrow(data) == 0) {
    stop("No complete observations available.")
  }
  
  if (!all(data[[yvar]] %in% c(0, 1))) {
    stop("The response variable must be binary and coded as 0/1.")
  }
  
  # Treat numeric predictors and panel variables as categorical
  if (is.numeric(data[[xvar]])) {
    data[[xvar]] = factor(data[[xvar]])
  }
  
  if (!is.null(panelvar) && is.numeric(data[[panelvar]])) {
    data[[panelvar]] = factor(data[[panelvar]])
  }
  
  data[[xvar]] = droplevels(factor(data[[xvar]]))
  
  if (!is.null(panelvar)) {
    data[[panelvar]] = droplevels(factor(data[[panelvar]]))
  }
  
  # Use a single internal panel variable for both cases
  panel_col = ".magnet_panel"
  
  while (panel_col %in% names(data)) {
    panel_col = paste0(panel_col, "_")
  }
  
  if (is.null(panelvar)) {
    data[[panel_col]] = factor("All")
  } else {
    data[[panel_col]] = data[[panelvar]]
  }
  
  # Counts and proportions within each panel and X group
  df_summary = data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(panel_col, xvar)))
    ) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_0 = sum(.data[[yvar]] == 0),
      n_1 = sum(.data[[yvar]] == 1),
      prop_0 = n_0 / n_total,
      prop_1 = n_1 / n_total,
      .groups = "drop"
    ) |>
    dplyr::group_by(.data[[panel_col]]) |>
    dplyr::mutate(
      scaling_factor = n_total / max(n_total)
    ) |>
    dplyr::ungroup()
  
  # Join summaries back to observations
  df_scaled = dplyr::left_join(
    data,
    df_summary,
    by = c(panel_col, xvar)
  )
  
  df_scaled = df_scaled |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(panel_col, xvar, yvar)))
    ) |>
    dplyr::mutate(stack_pos = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      y_scaled = dplyr::case_when(
        .data[[yvar]] == 0 ~
          (stack_pos - 0.5) / n_0 *
          (prop_0 * scaling_factor * 0.5),
        
        .data[[yvar]] == 1 ~
          1 - (stack_pos - 0.5) / n_1 *
          (prop_1 * scaling_factor * 0.5)
      )
    )
  
  # Fit logistic models separately within panels
  panel_data = split(data, data[[panel_col]], drop = TRUE)
  
  model_formula = stats::reformulate(xvar, response = yvar)
  
  predictions = lapply(panel_data, function(d) {
    
    x_levels = unique(as.character(d[[xvar]]))
    
    newdata = data.frame(
      x = factor(x_levels, levels = levels(data[[xvar]]))
    )
    names(newdata) = xvar
    
    # If a panel has only one outcome class, use observed proportions
    if (length(unique(d[[yvar]])) < 2 ||
        length(unique(d[[xvar]])) < 2) {
      probabilities = vapply(x_levels, function(x) {
        mean(d[[yvar]][as.character(d[[xvar]]) == x])
      }, numeric(1))
    } else {
      # Drop unused X levels before fitting
      d[[xvar]] = droplevels(d[[xvar]])
      newdata[[xvar]] = factor(
        x_levels,
        levels = levels(d[[xvar]])
      )
      
      model = suppressWarnings(
        stats::glm(
          model_formula,
          data = d,
          family = stats::binomial()
        )
      )
      
      probabilities = as.numeric(
        stats::predict(
          model,
          newdata = newdata,
          type = "response"
        )
      )
    }
    
    result = data.frame(
      X = x_levels,
      pred_prob = probabilities
    )
    
    result[[panel_col]] = d[[panel_col]][1]
    result
  })
  
  df_pred = dplyr::bind_rows(predictions)
  
  # Consistent X ordering across all panels
  if (!is.ordered(data[[xvar]])) {
    global_order = df_summary |>
      dplyr::group_by(.data[[xvar]]) |>
      dplyr::summarise(
        proportion = sum(n_1) / sum(n_total),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(proportion)) |>
      dplyr::pull(.data[[xvar]]) |>
      as.character()
  } else {
    global_order = levels(data[[xvar]])
  }
  
  df_scaled[[xvar]] = factor(
    df_scaled[[xvar]],
    levels = global_order,
    ordered = TRUE
  )
  
  df_summary[[xvar]] = factor(
    df_summary[[xvar]],
    levels = global_order,
    ordered = TRUE
  )
  
  df_pred$X = factor(
    df_pred$X,
    levels = global_order,
    ordered = TRUE
  )
  
  # Build plot
  p = ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    ggplot2::labs(
      title = "Magnet Plot",
      y = yvar,
      x = xvar
    ) +
    ggplot2::theme_bw()
  
  point_position = if (jitter) {
    ggplot2::position_jitter(width = 0.05)
  } else {
    ggplot2::position_identity()
  }
  
  p = p +
    ggplot2::geom_point(
      data = df_scaled,
      ggplot2::aes(
        x = .data[[xvar]],
        y = y_scaled
      ),
      color = "black",
      alpha = alpha,
      shape = 1,
      size = dot_size,
      position = point_position
    ) +
    ggplot2::geom_point(
      data = df_pred,
      ggplot2::aes(
        x = X,
        y = pred_prob
      ),
      color = "blue",
      size = 3
    ) +
    ggplot2::geom_text(
      data = df_summary,
      ggplot2::aes(
        x = .data[[xvar]],
        y = 0.52,
        label = paste0("N=", n_total)
      ),
      vjust = 0,
      size = 3.5
    )
  
  # Add panels only when requested
  if (!is.null(panelvar)) {
    p = p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", panel_col))
      )
    
    # Replace internal panel label with original variable name
    names(p$data)[names(p$data) == panel_col] = panelvar
  }
  
  return(p)
}

check_magnet_errors = function(data, yvar, xvar) {
  data = data[!is.na(data[[yvar]]), , drop=FALSE]
  if (length(unique(data[[xvar]])) > 5 & is.numeric(data[[xvar]])) {
    stop ("Your predictor variable must be categorical. Use the logistic_overlay function instead.")
  }
  if (length(unique(data[[yvar]])) != 2) stop ("Your outcome variable must have exactly 2 levels")
  if (!is.numeric(data[[yvar]])) {
    new_y = factor(data[[yvar]], 
                   levels = unique(data[[yvar]]), 
                   labels = c(0,1)) %>%
      as.character %>%
      as.numeric
    message(paste0("Note: your outcome variable levels (", paste0(unique(data[[yvar]]), collapse=" and "), ") are not 1/0.",
                   "\n", 
                   "I have converted them to 1/0, where 1 = ", unique(data[[yvar]])[2], " and 0 = ", unique(data[[yvar]])[1]))
    data[[yvar]] = new_y
  }
  return(data)
}
