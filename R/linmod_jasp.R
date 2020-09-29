#' General Linear Model in JASP
#' 
#' This function was developed for use in JASP. It takes a dataset as input with user 
#' options and returns results (tables, plots, etc)
#'
#' @param jaspResults A JASP object
#' @param dataset dataset supplied by JASP
#' @param options a list of options to pass to JASP
#'
#' @return a table, plot, etc. 
#' @export
linmod_jasp<- function(jaspResults, dataset, options) {

  ### check if they have an IV and a DV
  ready <- (options$dependent != "")
  
  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_linmod_data(dataset, options)
    
    ### check for categorical/numeric variables
    if (length(options$variables)>0){
      check.non.number = function(x){
        return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
        return.bool
      }
      character = sapply(dataset[,encodeColNames(options$variables), drop=F], check.non.number)
      numeric = !character
    } else {
      character = sapply(dataset[,encodeColNames(options$dependent), drop=F], check.non.number)
      numeric = !character
    }
  }
  
  ### compute results
  if (is.null(jaspResults[["linmod_results"]]))
    .linmod_compute(jaspResults, dataset, options, ready)
  
  ### show plots (if user specifies them)
  if (options$model) {
    
    if (is.null(jaspResults[["linmod_model_plot"]])){
      .linmod_model_plot(jaspResults, options, ready)
    }
  }
  
  if (options$avp) {
    if (is.null(jaspResults[["linmod_avp_plot"]])){
      .linmod_avp_plot(jaspResults, options, ready)
    }
  }
  
  if (options$residuals) {
    if (is.null(jaspResults[["linmod_residual_plot"]])){
      .linmod_residual_plot(jaspResults, options, ready)
    }
  }
  #### show plots (if user specifies them)
  if (options$univariate) {
    if (is.null(jaspResults[["linmod_univariate_plot"]])){
      .linmod_univariate_plot(jaspResults, options, ready, dataset)
    }
  }
  
  ### show output, depending on results
  if (ready && sum(numeric)>0){
    
    #if (length(options$variables)>1){
      if (options$modinf){
        if (is.null(jaspResults[["linmod_table_modcomp"]])){
          .create_linmod_table_modcomp(jaspResults, options, ready)
        }
      }
    #}
    
    if (options$sl && length(options$variables)>0){
      if (is.null(jaspResults[["linmod_table_slopes"]])){
        .create_linmod_table_slopes(jaspResults, options, ready)
      }
    }
  }
  
  if (ready && sum(character)>0){
    
    #if (length(options$variables)>1){
      if (options$modinf) {
        if (is.null(jaspResults[["linmod_table_modcomp"]])){
          .create_linmod_table_modcomp(jaspResults, options, ready)
        }
      }
    #}
    
    if (options$diff) {
      if (is.null(jaspResults[["linmod_table_differences"]])){
        .create_linmod_table_differences(jaspResults, options, ready)
      }
    }
  }
  
  if (ready && (sum(character)>0 || length(options$variables) == 0)){
    
    ### check if there's a jasp table already. if not, create it
    if (options$means){
      if (is.null(jaspResults[["linmod_table_means"]])){
        .create_linmod_table_means(jaspResults, options, ready)
      }
    }
  }

    
}
  


.linmod_model_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  modelplot <- createJaspPlot(title = "Plot of the Statistical Model",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  modelplot$dependOn(c("variables", "residuals", "model", "dependent", "interactions", "bw"))
  
  ### fill the plot object
  jaspResults[["modelplot"]] <- modelplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "model"
  
  .create_flexplot_linmod(jaspResults, modelplot, options, model.type)
  
  return()
}

.linmod_univariate_plot <- function(jaspResults, options, ready, dataset) {
  
  ### create plot options
  uniplot <- createJaspPlot(title = "Univariate Plots",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  uniplot$dependOn(c("dependent", "variables", "theme", "univariate", "bw"))
  
  ### fill the plot object
  jaspResults[["uniplot"]] <- uniplot
  
  if (!ready)
    return()
  
  ### loop through and plot everything
  all.variables = c(encodeColNames(options$dependent), encodeColNames(options$variables))
  
  a = theme_it(flexplot(make.formula(encodeColNames(options$dependent), "1"), dataset), options$theme)
  a = a + labs(y="Count", x=options$dependent)
  plot.list = list(rep(a, times=length(all.variables)))
  plot.list[[1]] = a
  for (i in 2:length(all.variables)){
    p = theme_it(flexplot(make.formula(encodeColNames(options$variables[i-1]), "1"), dataset), options$theme)
    p = p + labs(y="Count", x=options$variables[i-1])
    plot.list[[i]] = p
  }
  
  if (length(options$variables)<3){
    nc = length(options$variables) + 1
  } else if ((length(options$variables)+1)/2 == round((length(options$variables)+1)/2)){
    nc = 2
  } else {
    nc = 3
  }
  uniplot$plotObject <- cowplot::plot_grid(plotlist= plot.list, ncol=nc)
  
  return()
}

.linmod_avp_plot <- function(jaspResults, options, ready) {

  ### create plot options
  addedplot <- createJaspPlot(title = "Added Variable Plot",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  addedplot$dependOn(c("variables", "residuals", "model", "dependent", "avp", "interactions", "bw"))
  
  ### fill the plot object
  jaspResults[["avp"]] <- addedplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "added"
  .create_flexplot_linmod(jaspResults, addedplot, options, model.type)
  
  return()
}

.linmod_residual_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  residualplot <- createJaspPlot(title = "Diagnostic Plots",  width = 800, height = 500)
  
  ### what options should change the flexplot?
  residualplot$dependOn(c("variables", "residuals", "model", "dependent", "interactions", "bw"))
  
  ### fill the plot object
  jaspResults[["residualplot"]] <- residualplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "residuals"
  .create_flexplot_linmod(jaspResults, residualplot, options, model.type)
  
  return()
}

.create_flexplot_linmod <- function(jaspResults, flexplot, options, model.type) {
  linmod_results <- jaspResults[["linmod_results"]]$object

  
    ### if user removes terms from "Model terms," it will try to build a model from different sets of variables
  terms = all.vars(formula(linmod_results$model))[-1]
  
  if (length(terms)!=0)  generated.formula = flexplot:::make_flexplot_formula(terms, encodeColNames(options$dependent), linmod_results$model$model)

  if	(options$ghost & length(options$variables)<4){
    ghost=rgb(1,0,0,.4)
    if (options$bw) ghost = "gray"
  } else {
    ghost = NULL
  }

  whiskers = list("quartiles" = "quartiles",
                  "standard errors" = "sterr",
                  "standard deviations", "stdev")
  center = list("quartiles" = "median", 
                "standard errors" = "mean",
                "standard deviations" = "mean")
  if (model.type == "model" && length(terms) == 0) {
    # trick flexplot into plotting this
    new_data = linmod_results$model$model
    f = make.formula(encodeColNames(options$dependent), "1")
    plot = ggplot(data = new_data,aes_string(y = encodeColNames(options$dependent), x=1)) +
      labs(x = "") + 
      geom_hline(yintercept=0, col='lightgray') +
      geom_jitterd(alpha=options$alpha, width=options$jitx, height=options$jity)+
      stat_summary(fun="mean", geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')+
      stat_summary(geom='errorbar', 
                   fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, 
                   fun.max = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, 
                   width=.2, size = 1.25, 
                   position=position_dodge(width=.2), color = '#bf0303') +
      coord_cartesian(xlim=c(.75, 1.25))+
      theme_bw() 
      
  
  } else if (model.type=="model"){
    plot = compare.fits(generated.formula, data = linmod_results$model$model, model1 = linmod_results$model,
                 alpha=options$alpha, ghost.line=ghost, jitter=c(options$jitx, options$jity))
    plot = fancifyMyLabels(plot, options, generated.formula)    
  } else if (model.type == "residuals"){
    
    plot = visualize(linmod_results$model, linmod_results, plot=model.type, plots.as.list=TRUE,
                     alpha=options$alpha, jitter=c(options$jitx, options$jity))
    plot = arrange_jasp_plots(plot, options$theme, options$bw)
    plot = fancifyMyLabels(plot, options)
  } else if (model.type == "added" && length(options$variables) > 1){
    
    methods = list("Regression"="lm", 
                   "Quadratic"="quadratic", 
                   "Cubic"="cubic")
    formla = make.formula(encodeColNames(options$dependent),encodeColNames(options$variables))
    
    plot = added.plot(formla, linmod_results$model$model, method=methods[options$linetype], alpha=options$alpha,
                      jitter=c(options$jitx, options$jity))
    plot = fancifyMyLabels(plot, options)
  }
  
  plot <- theme_it(plot, options$theme)
  
  if (length(options$variables)<4){
    plot = plot + theme(legend.position = "none")      
  }
  
  if (length(options$variables)==0){
    # remove x axis
    plot = plot + theme(text=element_text(size=18),axis.text.x=element_blank(), axis.ticks.x=element_blank())
  }
  
  if (options$bw) {
    plot = convert_to_grayscale(plot)
  }
  
  #+ theme(legend.position = "none")
  #flexplot$addFootnote("message")
  flexplot$plotObject <- plot
  
  
  return()
}

.linmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    linmod_results <- createJaspState()
    jaspResults[["linmod_results"]] <- linmod_results
    linmod_results$dependOn(c("dependent", "variables", "interactions", "linetype"))
    
    ## interactions are stored in a deeply nested list. de-listify them
    
    predictors = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(encodeColNames(unlist(x$components)), collapse="*"))
      ), 
      collapse=" + ")
    
    # add variables with polynomial terms -------------------------------------
    vars = unlist(lapply(encodeColNames(options$interactions), FUN=function(x) encodeColNames(unlist(x$components))))
    polys = unlist(lapply(encodeColNames(options$interactions), FUN=function(x) encodeColNames(unlist(x$polynoms))))
    vars.with.poly = vars[polys]
    # specify degree
    if (options$linetype=="Quadratic" & length(vars.with.poly)>0){
      vars = paste0(add_polynomials(vars.with.poly, dataset, 2), collapse=" + ")
      predictors = paste0(predictors, " + ", vars)
    } else if (options$linetype == "Cubic" &  length(vars.with.poly)>0){
      vars = paste0(add_polynomials(vars.with.poly, dataset, 3), collapse=" + ")
      predictors = paste0(predictors, " + ", vars)
    }
    
    # create formula
    if (predictors != ""){
      f = paste0(encodeColNames(options$dependent), " ~ ", predictors, collapse = "")
    } else if (is.null(unlist(options$variables))) {
      f = paste0(encodeColNames(options$dependent), " ~ ", 1)
    } else {
      f = paste0(encodeColNames(options$dependent), " ~ ", encodeColNames(options$variables))
    }
    
    f = as.formula(f)
    ### store all the information
    model = lm(f, dataset)

    est = estimates(model, mc=TRUE)
    
    est$model = model
    
    linmod_results$object = est
    
    return()
  }
}

.create_linmod_table_means <- function(jaspResults, options, ready) {
  linmod_table_means <- createJaspTable(title = "Means of Categorical Variables")
  
  ### which options are required
  linmod_table_means$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
  ### add citation
  linmod_table_means$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_means$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  linmod_table_means$addColumnInfo(name = "levels",    title = "Level",       type = "string", combine = TRUE)	
  linmod_table_means$addColumnInfo(name = "est",      title = "Estimate",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_means$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_means$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }

  
  ### add message about what type of interval was used
  message <- paste0("")
  linmod_table_means$addFootnote(message)  
  linmod_table_means$showSpecifiedColumnsOnly <- TRUE
  
  ### store the table structure
  jaspResults[["linmod_table_means"]] <- linmod_table_means
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_means(linmod_table_means, linmod_results)
  
  return()
  
}

.create_linmod_table_differences <- function(jaspResults, options, ready) {
  linmod_table_differences <- createJaspTable(title = "Mean Differences Between Groups")
  
  ### which options are required
  linmod_table_differences$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
  ### add citation
  linmod_table_differences$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_differences$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  linmod_table_differences$addColumnInfo(name = "comparison",    title = "Comparison",       type = "string", combine = TRUE)	
  linmod_table_differences$addColumnInfo(name = "diff",      title = "Difference",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_differences$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_differences$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  linmod_table_differences$addColumnInfo(name = "cohensd",      title = "Cohen's d",    type = "number", format = "dp:2", combine = TRUE)	
  
  ### add message about what type of interval was used
  message <- paste0("")
  linmod_table_differences$addFootnote(message)  
  linmod_table_differences$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["linmod_table_differences"]] <- linmod_table_differences
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_differences(linmod_table_differences, linmod_results)
  
  return()
}

.create_linmod_table_slopes <- function(jaspResults, options, ready) {
  linmod_table_slopes <- createJaspTable(title = "Regression Slopes and Intercept")
  
  ### which options are required
  linmod_table_slopes$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf", "linetype"))
  
  ### add citation
  linmod_table_slopes$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_slopes$addColumnInfo(name = "var",      title = "Variables",   type = "string", combine = TRUE)
  linmod_table_slopes$addColumnInfo(name = "val",    title = "Value",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_slopes$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_slopes$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  linmod_table_slopes$addColumnInfo(name = "std",    title = "Standardized Slope (β)",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_slopes$addColumnInfo(name = "slwr",      title = "Lower β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_slopes$addColumnInfo(name = "supr",      title = "Upper β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  ### add message about what type of interval was used
  message <- paste0("Confidence intervals computed 95% Confidence Intervals.")
  message = paste0(message, "\n All estimates are conditional estimates.")
  linmod_table_slopes$addFootnote(message)  
  linmod_table_slopes$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["linmod_table_slopes"]] <- linmod_table_slopes
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_slopes(linmod_table_slopes, linmod_results)
  
  return()
}

.create_linmod_table_modcomp <- function(jaspResults, options, ready) {
  linmod_table_modcomp <- createJaspTable(title = "Model Comparisons (Estimating the Effect of Removing Terms)")
  
  ### which options are required
  linmod_table_modcomp$dependOn(c("dependent", "variables", "ci", "interactions", 
                                  "means", "diff", "sl", "modinf", "linetype", "pval"))
  
  ### add citation
  linmod_table_modcomp$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_modcomp$addColumnInfo(name = "terms",      title = "Term",   type = "string", combine = TRUE)
  linmod_table_modcomp$addColumnInfo(name = "rsq",    title = "Semi-partial R Squared",       type = "number", format = "dp:2", combine = TRUE)	
  linmod_table_modcomp$addColumnInfo(name = "bayes",      title = "Semi-partial Bayes Factor", type = "number", combine = TRUE)
  linmod_table_modcomp$addColumnInfo(name = "bayesinv",      title = "Inverted Bayes Factor", type = "number", combine = TRUE)
  
  ### add p-values
  if (options$pval){
    linmod_table_modcomp$addColumnInfo(name = "teststat",      title = "Test Statistic",       type = "string", overtitle = "Statistical Significance")
    linmod_table_modcomp$addColumnInfo(name = "statval",      title = "Value",       type = "number", format = "dp:2", combine = TRUE, overtitle = "Statistical Significance")
    linmod_table_modcomp$addColumnInfo(name = "df_num",      title = "df (spent)", type = "string", overtitle = "Statistical Significance")
    linmod_table_modcomp$addColumnInfo(name = "df_denom",      title = "df (remaining)", combine = TRUE, type = "string", overtitle = "Statistical Significance")
    linmod_table_modcomp$addColumnInfo(name = "p",      title = "p-value",      type = "number", format = "dp:3", combine = TRUE, overtitle = "Statistical Significance")
  }
  
  
  
  message = paste0("Semi-partials indicate the effect of removing that particular term from the model. ",
                   "Bayes factors are computed using the BIC.")
  if (length(options$interactions)>0){
    message = paste0(message, "\n 
                     Main effect estimates of r squared and BF have been suppressed because there is an interaction in the model.")
  }
  linmod_table_modcomp$addFootnote(message)  
  linmod_table_modcomp$showSpecifiedColumnsOnly <- TRUE
  
  ### store the table structure
  jaspResults[["linmod_table_modcomp"]] <- linmod_table_modcomp
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_modcomp(linmod_table_modcomp, linmod_results)
  
  return()
}


.fill_linmod_table_means = function(linmod_table_means, linmod_results){
  

  #### fill in table if they just supply one variable
  
  #if (options)
  factors = linmod_results$factor.summary
  
  if (is.na(factors)){
    mean_ci = predict(linmod_results$model, interval="confidence")[1,]
    tabdat = list(
      var = decodeColNames(names(linmod_results$model$model)),
      levels = "",
      est = mean_ci[1],
      lwr = mean_ci[2],
      upr = mean_ci[3]
      
    )
  } else {
    ### output results
    tabdat = list(
      var = decodeColNames(as.character(factors$variables)),
      levels = factors$levels,
      est = factors$estimate,
      lwr = factors$lower,
      upr = factors$upper
    )
  }
  linmod_table_means$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_differences = function(linmod_table_differences, linmod_results){
  
  
  diff = linmod_results$difference.matrix
  
  ### output results
  tabdat = list(
    var = decodeColNames(as.character(diff$variables)),
    comparison = diff$comparison,
    diff = diff$difference,
    lwr = diff$lower,
    upr = diff$upper,
    cohensd = diff$cohens.d
  )
  
  linmod_table_differences$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_slopes = function(linmod_table_slopes, linmod_results){
  
  slopes = linmod_results$numbers.summary
  
  ### output results
  tabdat = list(
    var = decodeColNames(as.character(slopes$variables)),
    val = slopes$estimate,
    lwr = slopes$lower,
    upr = slopes$upper,
    std = slopes$std.estimate,
    slwr = slopes$std.lower,
    supr = slopes$std.upper
  )
  
  linmod_table_slopes$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_modcomp = function(linmod_table_modcomp, linmod_results){
  
  
  
  tabdat = return_tabdata(linmod_results)
 
  ## convert names
  tabdat$terms[-1] = decodeColNames(tabdat$terms[-1])
  ### remove main effects for interactions
  if (length(grep(":", tabdat$terms))>1) {
    main_effects = unique(unlist(lapply(tabdat$terms[-1], function(x) strsplit(x, ":"))))
    good_bye_terms = which(tabdat$terms %in% main_effects)
    for (i in 2:length(tabdat)){
      if (is.numeric(tabdat[[i]])){
        tabdat[[i]][good_bye_terms] = NA
      } else {
        tabdat[[i]][good_bye_terms] = ""
      }
    }
  }
  linmod_table_modcomp$setData(tabdat)
  
  
  return()
}




.check_linmod_error = function(dataset, options){
  
  # check length of variables
  if ((options$dependent == "" & length(options$paneledVars)>0) | (options$dependent == "" & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
  if (options$dependent != "" & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")
  
}

.read_linmod_data = function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    dataset = .readDataSetToEnd(columns=(c(options$dependent, options$variables))) 
  ## variable names in the dataset are encoded. de-encodify them
  #names(dataset) = JASP:::.unv(names(dataset))
  return(dataset)
}

