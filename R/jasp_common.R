#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}

make_mctable = function(linmod_results) {
  save(linmod_results, file="/Users/fife/Documents/jaspresults.Rdata")
  all_terms = all.vars(formula(linmod_results$model))[-1]
  
  reg_mod_coef = summary(linmod_results$model)$coefficients
  anova_mod_coef = anova(linmod_results$model)
  f = (summary(linmod_results$model))$fstatistic
  f[2] = round(f[2]); f[3] = round(f[3])
  
  ### take care of situations where terms<2
  if (length(all_terms)==1) {
    tabdat = list(
      rsq = summary(linmod_results$model)$r.squared,
      bayes = NA,
      bayesinv = NA
    )
  
    
    
    ### check for correlation
    if (length(linmod_results$numbers)>0) {
      tabdat$terms = linmod_results$numbers
      tabdat$teststat = "r"
      tabdat$statval = coef(linmod_results$model)[2]*
          (sd(linmod_results$model$model[,2])/sd(linmod_results$model$model[,1])) 
      tabdat$df = summary(linmod_results$model)$df[2] 
      tabdat$p = reg_mod_coef[2,"Pr(>|t|)"]
      return(tabdat)
      
    } 
    
    ### check for ttest
    if (length(unique(linmod_results$model$model[,2])) == 2){
      tabdat$terms = linmod_results$factors
      tabdat$teststat = "t"
      tabdat$statval = reg_mod_coef[2,"t value"]
      tabdat$df = summary(linmod_results$model)$df[2]
      tabdat$p = reg_mod_coef[2,"Pr(>|t|)"]
      return(tabdat)
    } 
    
    ### anova
    if (length(unique(linmod_results$model$model[,2])) > 2){
      tabdat$terms = linmod_results$factors
      tabdat$teststat = "F"
      tabdat$statval = anova_mod_coef[1,"F value"] 
      tabdat$df = paste0(f[2], ", ", f[3]) 
      tabdat$p = pf(f[1],f[2],f[3],lower.tail=F)
      return(tabdat)
    }
  }    
    
    
    mc = linmod_results$model.comparison
    ### reformat : to be a times
    term.labels = as.character(mc$all.terms)
    main.effects = main_effects_2_remove(term.labels)
    term.labels = gsub(":", "×", term.labels)
    
      
    ### output results
    tabdat = list(
      terms = term.labels,
      rsq = mc$rsq,
      bayes = mc$bayes.factor,
      bayesinv = 1/mc$bayes.factor,
      teststat = "F",
      statval = f[1], 
      df = paste0(f[2], ", ", f[3]), 
      p = pf(f[1],f[2],f[3],lower.tail=F)
    )
    tabdat$teststat[2] = "t"

    #### remove main effects where there's an interaction present
    condition.me = term.labels %in% main.effects
    tabdat$rsq[condition.me] = NA
    tabdat$bayes[condition.me] = NA
    tabdat$bayesinv[condition.me] = NA
      
    for (i in 2:length(term.labels)){
      
      ### check if numeric
      if (term.labels[i] %in% linmod_results$numbers){
        tabdat$teststat[i] = "t"
        tabdat$statval[i] = reg_mod_coef[term.labels[i], "t value"]
        tabdat$df[i] = anova_mod_coef[term.labels[i], "Df"]
        tabdat$p[i] = reg_mod_coef[term.labels[i], "Pr(>|t|)"]
      } else {
        tabdat$teststat[i] = "F"
        tabdat$statval[i] = anova_mod_coef[term.labels[i], "F value"]
        tabdat$df[i] = paste0(
          anova_mod_coef[term.labels[i], "Df"], 
          ", ",
          anova_mod_coef["Residuals", "Df"]) 
        tabdat$p[i] = pf(
          anova_mod_coef[term.labels[i], "F value"],
          anova_mod_coef[term.labels[i], "Df"],
          anova_mod_coef["Residuals", "Df"],
          lower.tail=F)
      }
        
  }
   
  return(tabdat)
  
}

t_or_f = function(reg_mod_coef, anova_mod_coef, term, t.or.f){
  if (t.or.f=="t"){
    tabdat = list(
      teststat = "t",
      statval = reg_mod_coef[term, "t value"],
      df = anova_mod_coef[term, "Df"],
      p = reg_mod_coef[term, "Pr(>|t|)"]
    )
    return(tabdat)
  }
  
  if (t.or.f=="F"){
    tabdat = list(
      teststat = "F",
      statval = anova_mod_coef[term, "F value"],
      df = paste0(
        anova_mod_coef[term, "Df"], 
        ", ",
        anova_mod_coef["Residuals", "Df"]), 
      tabdat$p[i] = pf(
        anova_mod_coef[term, "F value"],
        anova_mod_coef[term, "Df"],
        anova_mod_coef["Residuals", "Df"],
        lower.tail=F)
    )
    return(tabdat)
  }
  
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
