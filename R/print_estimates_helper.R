#' Print estimates Summary
#'
#' Print estimates Summary
#' @aliases print.estimates
#' @param x an estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.estimates = function(x,...){
  

  #### print summary
  # check for r squared
  if ("r.squared" %in% names(x)) {
    cat(paste("Model R squared:\n", round(x$r.squared[1], digits=3), " (", round(x$r.squared[2], digits=2),", ", round(x$r.squared[3], digits=2),")\n\nSemi-Partial R squared:\n",sep=""))
    print(round(x$semi.p, digits=3))
  }
  
  #### print correlation
  if ("correlation" %in% names(x)) {
    if (!is.na(x$correlation[1])){
      cat(paste("Correlation:\n", round(x$correlation[1], digits=3), "\n"))
    }
  }
  
  #### replace NA with - 
  f = function(x){ x[is.na(x)] = "-"; x}
  if (length(x$factors)>0){
    cat(paste("\nEstimates for Factors:\n"))
    x$factor.summary[,3:ncol(x$factor.summary)] = round(x$factor.summary[,3:ncol(x$factor.summary)], digits=2)
    x$factor.summary[,3:ncol(x$factor.summary)] = apply(x$factor.summary[,3:ncol(x$factor.summary)], 2, f)
    #print(round(x$numbers.summary, digits=2))		
    print(x$factor.summary)
    cat(paste0("\n\nMean Differences:\n"))
    x$difference.matrix[,3:ncol(x$difference.matrix)] = round(x$difference.matrix[,3:ncol(x$difference.matrix)], digits=2)
    x$difference.matrix$variables[is.na(x$difference.matrix$variables)] = ""
    print(x$difference.matrix)		
  }
  if (length(x$numbers)>0){
    cat(paste("\n\nEstimates for Numeric Variables = \n"))
    x$numbers.summary[,2:ncol(x$numbers.summary)] = round(x$numbers.summary[,2:ncol(x$numbers.summary)], digits=2)
    #print(round(x$numbers.summary, digits=2))		
    print(x$numbers.summary)		
  }
  #cat(paste("\nsigma = ", round(x$sigma, digits=4), "\n\n"))
}

#' Print lmer_estimates Summary
#'
#' Print a lmer_estimates object
#' @aliases print.lmer_estimates
#' @param x an lmer_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.lmer_estimates = function(x,...){
  cat(paste("Fixed Effects: \n", sep=""))
  print(x$fixed)
  cat(paste("\n\nRandom Effects: \n", sep=""))
  print(x$rand)
  cat(paste("\n\nICC and Design Effect: \n", sep=""))
  print(x$icc)
  cat(paste("\n\nR Squared: \n\n", sep=""))
  print(x$r.squared)    
}	

#' Print glmer_estimates Summary
#'
#' Print a glmer_estimates object
#' @aliases print.glmer_estimates
#' @param x an glmer_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.glmer_estimates = function(x,...){
  print.default(x)
}	

#' Print rf_estimates Summary
#'
#' Print a rf_estimates object
#' @aliases print.rf_estimates
#' @param x an rf_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.rf_estimates = function(x,...){
  if (attr(x, "numeric")) {
    cat(paste("\n\nQuantiles of absolute value of OOB performance (i.e., abs(predicted - actual)):\n\n", sep=""))
    print(x$oob)
    cat(paste("\n\nModel R Squared:\n\n", sep=""))
    print(x$rsq)
    cat(paste("\n\nVariable importance (root MSE of predicted versus permuted):\n\n", sep=""))
    print(x$importance)
  } else {
    cat(paste("\n\nOOB accuracy in prediction:\n\n", sep=""))
    cat(x$oob)
    cat(paste("\n\nVariable importance (mean decrease in accuracy when permuted):\n\n", sep=""))
    print(x$importance)
  }
  
}	

#' Print glm_estimates Summary
#'
#' Print a glm_estimates object
#' @aliases print.glm_estimates
#' @param x an glm_estimates object
#' @param ... ignored
#' @return A printed list of estimates
#' @export
print.glm_estimates = function(x, ...) {
  
  raw =          with(x, dplyr::tibble(Variables=attr(x, "row.names"), 
                                       raw.coefficients, OR, inverse.OR, instantaneous_slope, 
                                       intercept_threshold))
  standardized = with(x, dplyr::tibble(variables=attr(x, "row.names"), 
                                       standardized.OR, inverse.standardized.OR, 
                                       standardized_slope, standardized_threshold, 
                                       `Prediction Difference (+/- 1 SD)`)) %>%
                                            purrr::set_names(c("Variables", "OR", "inverse.OR", "instantaneous.slope",
                                                        "threshold", "Prediction Difference (+/- 1 SD)"))
  cat("Raw Estimates:\n")
  print(raw)
  
  cat("\n\nStandardized Estimates:\n")
  print(standardized)
  
  cat("\n\n To see the documentation for each of these estimates, type ?estimates.glm")
}






