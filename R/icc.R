##' Compute the ICC from a lmer (package lme4) model
##'
##' This function will extract the variances from a mixed model and output the value of the ICC
##'	
##' Recall that the equation for the ICC is var(school)/[var(school)+ var(person)]. 
##' This forumla extracts the necessary variances from the mixed model and computes it. 
##' @param model a lmer model
##' @return the ICC
##' @author Dustin Fife
##' @export
##' @examples
##' data(alcuse)
##' require(lme4)
##' model = lmer(ALCUSE~1|ID, data=alcuse)
##' icc(model)
icc = function(model){
  #### compute ICC
  var.components = as.data.frame(VarCorr(model))$vcov
  ICC = var.components[1]/sum(var.components)
  
  #### find out average cluster size
  id.name = names(coef(model))
  clusters = nrow(matrix(unlist((coef(model)[id.name]))))
  n = length(residuals(model))
  average.cluster.size = n/clusters
  
  #### compute design effects
  design.effect = 1+ICC*(average.cluster.size-1)
  
  #### return stuff
  list(icc=ICC, design.effect=design.effect)
  
}