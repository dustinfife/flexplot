##' Compare the improvement in R squared from a baseline model
##'
##' Compare the improvement in R squared from a baseline model
##'	
##' Compare the improvement in fit from a baseline model, using mixed models
##' @param full the full mixed model
##' @param reduced the reduced mixed model (typically a random effect ANOVA model)
##' @return A vector containing the proportion change in fit for each random effects parameter
##' @author Dustin Fife
##' @importFrom lme4 lmer
##' @importFrom lme4 VarCorr
##' @export
##' @examples
#' data(math)
#' require(lme4)
#' baseline.model = lmer(MathAch~1 + (1|School), data=math)
#' full.model = lmer(MathAch~SES + (SES|School), data=math)
#' rsq_change(full.model, baseline.model)
rsq_change = function(full, reduced){
  
  #### create objects for variance terms
  full.var = as.data.frame(VarCorr(full)); names(full.var)[4:5] = c("variance", "sd")
  reduced.var = as.data.frame(VarCorr(reduced))
  
  if (nrow(full.var)<nrow(reduced.var)){
    stop("Your 'full' model must be larger than your 'reduced' model.")
  }
  
  #### look only at those that have common terms
  full.var = full.var[with(full.var, order(grp, var1, var2)), ]; full.var$vcov2 = full.var$vcov
  reduced.var = reduced.var[with(reduced.var, order(grp, var1, var2)), ]	
  
  full.var = merge(full.var, reduced.var, all.y=T, all.x=F)
  
  #### get rid of those with something in var2 (that's a covariance, which we don't care about)
  if (length(which(!is.na(full.var$var2)))>0){
    full.var = full.var[-which(!is.na(full.var$var2)),]
  }
  
  #### compute estimates
  change.in.fit = (full.var$vcov-full.var$variance)/full.var$vcov
  names(change.in.fit) = full.var$var1; names(change.in.fit)[is.na(names(change.in.fit))] = "Residual"
  
  change.in.fit
  
}