#' Apply a Box–Cox Transformation to a Model's Outcome Variable
#'
#' This helper function automates the full Box–Cox workflow:
#' it extracts the outcome variable from a fitted model,
#' finds the maximum-likelihood Box–Cox lambda,
#' applies the appropriate transformation to the outcome,
#' and refits the model using the transformed response.
#'
#' If the outcome contains non-positive values, the function automatically
#' shifts the variable upward (adding the absolute minimum plus a small constant)
#' so that the Box–Cox transformation is valid.
#'
#' @param mod A fitted model object, typically from \code{\link{lm}} or
#'   another model compatible with \code{MASS::boxcox}.
#' @param return.lambda Logical. If \code{TRUE}, the function returns the
#'   estimated Box–Cox lambda rather than the updated model. Default is \code{FALSE}.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Extracts the dependent variable from the model.
#'   \item Checks whether the variable contains zero or negative values; if so,
#'     it shifts the entire variable upward to make all values positive.
#'   \item Uses \code{MASS::boxcox} to estimate the optimal lambda.
#'   \item Applies either the log transform (if lambda = 0) or the general
#'     Box–Cox power transform.
#'   \item Refits the model with the transformed outcome.
#' }
#'
#' @return
#' If \code{return.lambda = TRUE}, returns a numeric scalar containing the
#' estimated Box–Cox lambda.
#'
#' If \code{return.lambda = FALSE}, returns a refitted model object with the
#' transformed outcome variable.
#'
#' @examples
#' \dontrun{
#'   m = lm(y ~ x1 + x2, data = dat)
#'   m_bc = boxcox_my_mod(m)
#'
#'   # Just get the lambda
#'   lambda = boxcox_my_mod(m, return.lambda = TRUE)
#' }
#'
#' @importFrom MASS boxcox
#' @export
boxcox_my_mod = function(mod, return.lambda = FALSE) {
  
  # extract formula + data
  f = formula(mod)
  d = model.frame(mod)
  dv = all.vars(f)[1]
  
  # ensure positivity
  if (min(d[[dv]]) <= 0) {
    shift = abs(min(d[[dv]])) + .01
    d[[dv]] = d[[dv]] + shift
  }
  
  y = d[[dv]]
  
  # Box-Cox log-likelihood function
  bc_ll = function(lambda) {
    if (lambda == 0) {
      yt = log(y)
    } else {
      yt = (y^lambda - 1) / lambda
    }
    
    # fit model on transformed y
    d2 = d
    d2[[dv]] = yt
    m2 = lm(f, data = d2)
    
    # log-likelihood up to additive constant
    -0.5 * length(y) * log(mean(residuals(m2)^2)) + (lambda - 1) * sum(log(y))
  }
  
  # search grid like MASS::boxcox
  lambda_grid = seq(-2, 2, length = 200)
  ll_vals = sapply(lambda_grid, bc_ll)
  lambda = lambda_grid[which.max(ll_vals)]
  
  if (return.lambda) return(lambda)
  
  # apply best lambda
  if (lambda == 0) {
    d[[dv]] = log(y)
  } else {
    d[[dv]] = (y^lambda - 1) / lambda
  }
  
  final_mod = lm(f, data = d)
  final_mod
}







return_factors_names = function(model) {
  
  terms = remove_interaction_terms(model)
  d = extract_data_from_fitted_object(model)
  d %>% select(-where(is.numeric)) %>%  names
}

identify_method = function(data, outcome, axis, method=NULL) {
  
  if (axis[1] == "1") return("loess")
  if (!is.null(method)) return(method)
  
  # Binary outcome
  if (length(unique(stats::na.omit(data[[outcome]]))) == 2) {
    return("logistic")
  }
  
  return("loess")
}

#' Create a 1080p plot for YouTube
#'
#' @param p the ggplot object
#' @param name the name of the file (excluding the ".jpg")
#' @param path the path to the folder
#'
#' @export
youtube_plot = function(p, name="plot", path="", font = "Moon Flower Bold"){
  require(extrafont)
  loadfonts(quiet=TRUE)
  p = p + theme(text=element_text(size=18, family=font))
  file_path = ifelse(path=="", paste0(name, ".jpg"), paste0(path, "/", name, ".jpg"))
  ggsave(file_path, plot=p, height = 1080/150, width=1920/150, dpi=150, units = "in")
}