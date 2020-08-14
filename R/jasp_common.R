#tested
return_baseline_model = function(formula) {
  all_terms = all.vars(formula)[-1]
  if (length(all_terms) == 0 ) return ("Baseline: Zero Model")
  if (length(all_terms) == 1) return ("Baseline: Mean Model")
  if (length(all_terms) > 1) return ("Baseline: Full Model")
}

#tested
return_baseline_rsq = function(model){
  #browser()
  all_terms = all.vars(formula(model))[-1]
  if (length(all_terms) == 1 ) return (NA)
  return(summary(model)$r.squared)
}

#tested
return_baseline_df = function(model) {
  #browser()
  all_terms = all.vars(formula(model))[-1]
  residual_df = anova(model)["Residuals", "Df"]
  model_df = sum(anova(model)[,"Df"]) - residual_df
  
  if (length(all_terms) > 1) return (list("df_num" = model_df+1, "df_denom" = residual_df))
  if (length(all_terms) == 1) return (list("df_num" = 1, "df_denom" = residual_df + model_df))
  if (length(all_terms) == 0) return (list("df_num" = 0, "df_denom" = residual_df + model_df + 1))
  
}


#tested
complete_teststat_when_one_var = function(model, term, first.term = TRUE){
 
  if (length(grep(":", term))>0) {
    teststat = "t"
    ## coefs from model display the referent level too...getting the row number from ANOVA instead
    rownum = which(row.names(anova(model))==term)
    statval = summary(model)$coefficients[rownum+1,"t value"]
    return(list("teststat"=teststat, "statval" = statval))
  }

  ### correlation
  if (is.numeric(model$model[,term]) & first.term) {
    teststat = "r"
    statval = coef(model)[2]*
      (sd(model$model[,2])/sd(model$model[,1])) 
    return(list("teststat"=teststat, "statval" = statval))
  }
  
  if (length(unique(model$model[,term])) == 2 | is.numeric(model$model[,term])) {
    teststat = "t"
    ## coefs from model display the referent level too...getting the row number from ANOVA instead
    rownum = which(row.names(anova(model))==term)
    statval = summary(model)$coefficients[rownum+1,"t value"]
    return(list("teststat"=teststat, "statval" = statval))
  }
  
  teststat = "F"
  statval = anova(model)[term, "F value"]
  return(list("teststat"=teststat, "statval" = statval))
}

#tested
return_term_df = function(teststatistic, model, term) {
  #browser()
  df_numerator = anova(model)[term, "Df"]
  df_denom = anova(model)["Residuals", "Df"]
  if (teststatistic == "F") {
    return(
      list(
        "df_denom" = paste0(df_denom),
        "p" = pf(anova(model)[term, "F value"],df_numerator, df_denom,lower.tail=F),
        "df_num" = paste0(df_numerator)
      )
    )
  } else {
    return(
      list(
        "df_denom" = df_denom,
        "p" = anova(model)[term, "Pr(>F)"],
        "df_num" = df_numerator
      )
    )
    #return(df_denom)
  }
}


### this function returns the output table for model comparisons
return_tabdata = function(linmod_results) {
  #### set first instance of all
  tabdat = list()
  tabdat$terms = return_baseline_model(formula(linmod_results$model))
  tabdat$rsq = return_baseline_rsq(linmod_results$model)
  tabdat$bayes = NA
  tabdat$bayesinv = NA
  tabdat$teststat = ""
  tabdat$statval = NA
  tabdat$df_num = return_baseline_df(linmod_results$model)[[1]]
  tabdat$df_denom = return_baseline_df(linmod_results$model)[[2]]
  tabdat$p = NA
  
  #### create variables of models
  formula = formula(linmod_results$model)
  all_terms = attr(terms(formula), "term.labels")
  reg_mod_coef = summary(linmod_results$model)$coefficients
  estimates = flexplot::estimates(linmod_results$model)
  anova_mod_coef = anova(linmod_results$model)
  mc = estimates$model.comparison
  
  #### return tabdat if it's an intercept only model
  if (length(all_terms) == 0) {
    #### create new null model
    y = names(linmod_results$model$model)[1]
    f = make.formula(y, "0")
    null_mod = lm(f, data=linmod_results$model$model)
    bf = flexplot::bf.bic(linmod_results$model, null_mod)
    
    ### fill in terms unique to this
    tabdat$terms[2] = y
    tabdat$rsq[2] = estimates$r.squared[1]
    tabdat$bayes[2] = bf
    tabdat$bayesinv[2] = 1/bf
    
    ## enter test statistic (all this can be in the loop)
    tabdat$teststat[2] = "t"
    tabdat$statval[2] = reg_mod_coef[,"t value"]
    tabdat$p[2] = reg_mod_coef[,"Pr(>|t|)"]
    
    tabdat$df_num[2] = 1
    tabdat$df_denom[2] = anova_mod_coef["Residuals", "Df"]
    return(tabdat)    
  }
  
  #### return if there's only one variable
  if (length(all_terms) == 1) {
    #browser()
    #### create new null model
    y = names(linmod_results$model$model)[1]
    f = make.formula(y, "1")
    null_mod = lm(f, data=linmod_results$model$model)
    bf = flexplot::bf.bic(linmod_results$model, null_mod)
    
    ### fill in terms unique to this
    tabdat$terms[2] = all_terms
    tabdat$rsq[2] = estimates$r.squared[1]
    tabdat$bayes[2] = bf
    tabdat$bayesinv[2] = 1/bf
    
    ## enter test statistic (all this can be in the loop)
    teststatistics = complete_teststat_when_one_var(linmod_results$model, all_terms)
    tabdat$teststat[2] = teststatistics[[1]]
    tabdat$statval[2] = teststatistics[[2]]
    
    df_p = return_term_df(teststatistics[[1]], linmod_results$model, all_terms)
    tabdat$df_num[2] = as.character(df_p[[3]])
    tabdat$df_denom[2] = as.character(df_p[[1]])
    tabdat$p[2] = df_p[[2]]
    return(tabdat)
  }
  
  for (i in 1:(length(all_terms))){
    
    bf = mc[mc$all.terms == all_terms[i], "bayes.factor"]
    tabdat$terms[i+1] = all_terms[i]
    tabdat$rsq[i+1] = estimates$semi.p[all_terms[i]]
    tabdat$bayes[i+1] = bf
    tabdat$bayesinv[i+1] = 1/bf
    
    ## enter test statistic (all this can be in the loop)
    #browser()
    teststatistics = complete_teststat_when_one_var(linmod_results$model, all_terms[i], first.term = FALSE)
    tabdat$teststat[i+1] = teststatistics[[1]]
    tabdat$statval[i+1] = teststatistics[[2]]
    df_p = return_term_df(teststatistics[[1]], linmod_results$model, all_terms[i])
    tabdat$df_denom[i+1] = as.character(df_p[[1]])
    tabdat$df_num[i+1] = as.character(df_p[[3]])
    tabdat$p[i+1] = df_p[[2]]
  }
  
  return(tabdat)
}


convert_to_grayscale = function(plot){
  plot = plot + scale_colour_grey(start = 0, end = .9) + scale_fill_grey()
  plot$layers[[2]]$aes_params$colour <-  "black"
  if (length(p$layers)>2) plot$layers[[3]]$aes_params$colour <-  "black"
  return(plot)
}

#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}


make_mctable = function(linmod_results) {
  #save(linmod_results, file="/Users/fife/Documents/jaspresults.Rdata")
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

# t_or_f = function(reg_mod_coef, anova_mod_coef, term, t.or.f){
#   if (t.or.f=="t"){
#     tabdat = list(
#       teststat = "t",
#       statval = reg_mod_coef[term, "t value"],
#       df = anova_mod_coef[term, "Df"],
#       p = reg_mod_coef[term, "Pr(>|t|)"]
#     )
#     return(tabdat)
#   }
#   
#   if (t.or.f=="F"){
#     tabdat = list(
#       teststat = "F",
#       statval = anova_mod_coef[term, "F value"],
#       df = paste0(
#         anova_mod_coef[term, "Df"], 
#         ", ",
#         anova_mod_coef["Residuals", "Df"]), 
#       tabdat$p[i] = pf(
#         anova_mod_coef[term, "F value"],
#         anova_mod_coef[term, "Df"],
#         anova_mod_coef["Residuals", "Df"],
#         lower.tail=F)
#     )
#     return(tabdat)
#   }
#   
# }


### function to organize residual plots
arrange_jasp_plots = function(plot_list, theme, bw=FALSE){
  #browser()
  plot_list = lapply(plot_list, theme_it, theme)
  if (bw) plot_list =lapply(plot_list, convert_to_grayscale)
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
