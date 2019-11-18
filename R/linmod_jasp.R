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
  ready <- (options$dependent != "" & length(options$variables)>0)
  
  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_linmod_data(dataset, options)
    #.check_linmod_error()  #### HOW DO YOU HAVE IT THROW AN ERROR IF THE VARIABLE IS NOT NUMBERIC?
  
    ### check for categorical/numeric variables
    check.non.number = function(x){
      return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
      return.bool
    }
    character = sapply(dataset[,options$variables, drop=F], check.non.number)
    numeric = !character
    
    
    #save(dataset, character, options, file="/Users/fife/Documents/jaspbroke.rdat")
    
    ### compute results
    if (is.null(jaspResults[["linmod_results"]]))
      .linmod_compute(jaspResults, dataset, options, ready)


    ### show plots (if user specifies them)
    if (options$model) {

      if (is.null(jaspResults[["linmod_model_plot"]])){
        .linmod_model_plot(jaspResults, options, ready)
      }
    }

    if (options$residuals) {
      if (is.null(jaspResults[["linmod_residual_plot"]])){
        .linmod_residual_plot(jaspResults, options, ready)
      }
    }


    ### show output, depending on results
    if (sum(numeric)>0){

      if (length(options$variables)>1){
        if (options$modinf){
          if (is.null(jaspResults[["linmod_table_modcomp"]])){
            .create_linmod_table_modcomp(jaspResults, options, ready)
          }
        }
      }

      if (options$sl){
        if (is.null(jaspResults[["linmod_table_slopes"]])){
          .create_linmod_table_slopes(jaspResults, options, ready)
        }
      }
    }

    if (sum(character)>0){

      if (length(options$variables)>1){
        if (options$modinf) {
          if (is.null(jaspResults[["linmod_table_modcomp"]])){
            .create_linmod_table_modcomp(jaspResults, options, ready)
          }
        }
      }

      ### check if there's a jasp table already. if not, create it
      if (options$means){
        if (is.null(jaspResults[["linmod_table_means"]])){
        .create_linmod_table_means(jaspResults, options, ready)
        }
      }

      if (options$diff) {
        if (is.null(jaspResults[["linmod_table_differences"]])){
          .create_linmod_table_differences(jaspResults, options, ready)
        }
      }

    }
    
    # 

  }  
}

.linmod_model_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  modelplot <- createJaspPlot(title = "Plot of the Statistical Model",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  modelplot$dependOn(c("variables", "residuals", "model", "dependent"))
  
  ### fill the plot object
  jaspResults[["modelplot"]] <- modelplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "model"
  
  .create_flexplot_linmod(jaspResults, modelplot, options, model.type)
  
  return()
}

.linmod_residual_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  residualplot <- createJaspPlot(title = "Diagnostic Plots",  width = 800, height = 500)
  
  ### what options should change the flexplot?
  residualplot$dependOn(c("variables", "residuals", "model", "dependent"))
  
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
  
  if (model.type=="model"){
    type = "model"
  } else if (model.type=="residuals"){
    type = "model"
  }

  generated.formula = make_flexplot_formula(options$variables, options$dependent, linmod_results$model$model)
  save(generated.formula, linmod_results, options, file="/Users/fife/Documents/jaspbroke.rdat")
  
  if	(options$ghost){
    ghost="black"
  } else {
    ghost = NULL
  }
  
  whiskers = list("quartiles" = "quartiles",
                  "standard errors" = "sterr",
                  "standard deviations", "stdev")
  if (model.type=="model"){
    
    plot = compare.fits(generated.formula, data = linmod_results$model$model, model1 = linmod_results$model,
                      alpha=options$alpha, ghost.line=ghost)
  } else {
    plot = visualize(linmod_results$model, linmod_results, plot=model.type, alpha=options$alpha)
  }
  
  if (options$theme == "JASP"){
    plot = themeJasp(plot)
  } else {
    theme = list("Black and white"="theme_bw()+ theme(text=element_text(size=18))",
                 "Minimal" = "theme_minimal()+ theme(text=element_text(size=18))",
                 "Classic" = "theme_classic()+ theme(text=element_text(size=18))",
                 "Dark" = "theme_dark() + theme(text=element_text(size=18))")
    plot = plot + eval(parse(text=theme[[options$theme]])) + theme(legend.position = "none")
  }
  flexplot$plotObject <- plot
  
  return()
}

.linmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    linmod_results <- createJaspState()
    jaspResults[["linmod_results"]] <- linmod_results
    linmod_results$dependOn(c("dependent", "variables", "interactions"))
    
    ## interactions are stored in a deeply nested list. de-listify them
    predictors = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
      ), 
      collapse=" + ")
    f = paste0(options$dependent, " ~ ", predictors, collapse = "")
    f = as.formula(f)
    
    
    
    #save(options, dataset, model, f, file="/Users/fife/Dropbox/jaspresults.Rdat")
     
    
    ### store all the information
    model = lm(f, dataset)
    est = estimates(model)
    #save(options, dataset, ready, model, file="/Users/fife/Documents/jaspresults.Rdat")
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
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
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
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
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
  linmod_table_slopes$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
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
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
  
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
  linmod_table_modcomp$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
  ### add citation
  linmod_table_modcomp$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_modcomp$addColumnInfo(name = "terms",      title = "Term",   type = "string", combine = TRUE)
  linmod_table_modcomp$addColumnInfo(name = "rsq",    title = "Semi-partial R Squared",       type = "number", format = "dp:2", combine = TRUE)	
  linmod_table_modcomp$addColumnInfo(name = "bayes",      title = "Semi-partial Bayes Factor", type = "number", combine = TRUE)
  
  
  message = paste0("message \n Note: Semi-partials indicate the effect of removing that particular term from the model. ",
    "Bayes factors are computed using the BIC. Higher Bayes factors indicate important terms. Lower Bayes factors suggest they can be removed from the model")
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
  

  factors = linmod_results$factor.summary
  
  ### output results
  tabdat = list(
    var = as.character(factors$variables),
    levels = factors$levels,
    est = factors$estimate,
    lwr = factors$lower,
    upr = factors$upper
  )
  linmod_table_means$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_differences = function(linmod_table_differences, linmod_results){
  
  
  diff = linmod_results$difference.matrix
  
  ### output results
  tabdat = list(
    var = as.character(diff$variables),
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
    var = as.character(slopes$variables),
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
  
  mc = linmod_results$model.comparison
  
  
  ### reformat : to be a times
  term.labels = as.character(mc$all.terms)
  term.labels = gsub(":", "×", term.labels)
  ### output results
  tabdat = list(
    terms = term.labels,
    rsq = mc$rsq,
    bayes = mc$bayes.factor
  )
  #save(mc, file="/Users/fife/Documents/jaspresults.rdat")
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
    names(dataset) = JASP:::.unv(names(dataset))
    return(dataset)
}
