#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}

t_or_f = function(linmod_results, tabdat){
  
  # reg_mod_coef = summary(linmod_results$model)$coefficients
  # anova_mod_coef = anova(linmod_results$model)
  # 
  # 
  # ### if only one variable is provided, compare to a zero model (ttest)
  # if (length(tabdat$terms)==1) {
  #   tabdat$teststat = "t"
  #   tabdat$statval = reg_mod_coef[1,3]
  #   tabdat$df = as.character(round(summary(linmod_results$model)$df[1]))
  #   tabdat$p = reg_mod_coef[1,4]
  #   return(tabdat)
  # } else {
  
    # #prepopulate
    # f = (summary(linmod_results$model))$fstatistic
    # f[2] = round(f[2]); f[3] = round(f[3])
    # tabdat$teststat = rep("F", times=length(tabdat$terms))
    # tabdat$statval = rep(f[1], times=length(tabdat$terms))
    # tabdat$df = rep(paste0(f[2], ", ", f[3]), times=length(tabdat$terms))
    # tabdat$p = rep(pf(f[1],f[2],f[3],lower.tail=F), times=length(tabdat$terms))
    
    # # #find numbers/categories
    # numbs = tabdat$terms %in% linmod_results$numbers
    # facts = which(tabdat$terms %in% linmod_results$factors)
    # ## repopulate with real values
    # tabdat$teststat[numbs] = "t"
    # tabdat$statval[numbs] = reg_mod_coef[numbs,3]
    # tabdat$statval[facts] = anova_mod_coef[facts-1,4]
    # tabdat$df[numbs] = as.character(round(anova_mod_coef[which(numbs)-1,"Df"]))
    # tabdat$df[facts] = paste0(anova_mod_coef[facts-1,"Df"], ", ", anova_mod_coef["Residuals", "Df"])
    # tabdat$p[numbs] = reg_mod_coef[numbs,4]
    # tabdat$p[facts] = anova_mod_coef[facts-1,5]
    
    return(tabdat)
  # }
}


### function to organize residual plots
arrange_jasp_plots = function(plot_list, theme){
  
  plot_list = lapply(plot_list, theme_it, theme)
  if (is.null(plot_list[["res.dep"]])){
    plot_list[["res.dep"]] = NULL
    plot = cowplot::plot_grid(plotlist = plot_list)
  } else {
    top.row =suppressMessages(cowplot::plot_grid(plot_list$histo, plot_list$res.dep,ncol=2))
    bottom.row =suppressMessages(cowplot::plot_grid(NULL, plot_list$sl, NULL, ncol=3, rel_widths=c(.25, .5, .25)))
    plot = suppressMessages(cowplot::plot_grid(top.row, bottom.row, nrow=2))
  }
  return(plot)
}

theme_it = function(plot, theme) {
  originalTheme <- theme
  theme <- tolower(theme)
  if (!theme %in% c("jasp", "black and white", "minimal", "classic", "dark"))
    stop("Invalid theme provided: ", originalTheme)
  
  if (theme == "jasp")
    plot = themeJasp(plot)
  else
    plot = addGgplotThemeLayer(plot, theme)
  
  return(plot)
}

addGgplotThemeLayer <- function(plot, theme) {
  plotTheme <- switch(theme,
                      "black and white" = theme_bw(),
                      "minimal"         = theme_minimal(),
                      "classic"         = theme_classic(),
                      "dark"            = theme_dark())
  plotTheme <- plotTheme + theme(text=element_text(size=18))
  
  return(plot + plotTheme)
}

modify_dv = function(dataset, outcome, family){
  if (family!="Logistic") {
    dataset[,outcome] = as.numeric(dataset[,outcome])
  } else {
    dataset = factor.to.logistic(data = dataset, outcome = outcome)
  }
  return(dataset)
}
  
## find main effects
find.me = function(x) {
  if (length(x)>1){
    return(x)
  } 
}

#term.labels = c("a", "b", "c", "a:b", "b:c")
#term.labels = letters[1:5]
main_effects_2_remove = function(term.labels){
  splits = strsplit(term.labels, ":")
  interactions = unlist(lapply(splits, find.me))
  return(unique(interactions))
}
