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
glinmod_jasp<- function(jaspResults, dataset, options) {

  ### check if they have an IV and a DV
  ready <- (options$dependent != "" & length(options$variables)>0)
  
  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_glinmod_data(dataset, options)
    #.check_glinmod_error()  #### HOW DO YOU HAVE IT THROW AN ERROR IF THE VARIABLE IS NOT NUMBERIC?
  
    ### check for categorical/numeric variables
    check.non.number = function(x){
      return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
      return.bool
    }
    character = sapply(dataset[,options$variables, drop=F], check.non.number)
    numeric = !character
    
    #### compute results
    if (is.null(jaspResults[["glinmod_results"]]))
      .glinmod_compute(jaspResults, dataset, options, ready)
    
    
    #### show plots (if user specifies them)
    if (options$model) {
      if (is.null(jaspResults[["glinmod_model_plot"]])){
        .glinmod_model_plot(jaspResults, options, ready)
      }
    }

    if (options$residuals) {
      if (is.null(jaspResults[["glinmod_residual_plot"]])){
        .glinmod_residual_plot(jaspResults, options, ready)
      }
    }
    
    
    ### show output, depending on results
    if (sum(numeric)>0){
    
      if (length(options$variables)>1){
        if (is.null(jaspResults[["glinmod_table_modcomp"]])){
          .create_glinmod_table_modcomp(jaspResults, options, ready)
        }
      }
      
      if (is.null(jaspResults[["glinmod_table_slopes"]])){
        .create_glinmod_table_slopes(jaspResults, options, ready)
      }

    }
    
    if (sum(character)>0){
      
      if (length(options$variables)>1){
        if (is.null(jaspResults[["glinmod_table_modcomp"]])){
          .create_glinmod_table_modcomp(jaspResults, options, ready)
        }
      }
      
      ### check if there's a jasp table already. if not, create it
      if (is.null(jaspResults[["glinmod_table_means"]])){
        .create_glinmod_table_means(jaspResults, options, ready)
      }  
      
      if (is.null(jaspResults[["glinmod_table_differences"]])){
        .create_glinmod_table_differences(jaspResults, options, ready)
      }  
      
    }
    
    # 

  }  
}

.glinmod_model_plot <- function(jaspResults, options, ready) {
  
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
  .create_flexplot(jaspResults, modelplot, options, model.type)
  
  return()
}

.glinmod_residual_plot <- function(jaspResults, options, ready) {
  
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
  .create_flexplot(jaspResults, residualplot, options, model.type)
  
  return()
}

.create_flexplot <- function(jaspResults, flexplot, options, model.type) {
  
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  if (model.type=="model"){
    flexplot$plotObject <- visualize(glinmod_results$model, plot="model")
  } else if (model.type=="residuals") {
    flexplot$plotObject <- visualize(glinmod_results$model, plot="residuals")
  }

  return()
}


.glinmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    glinmod_results <- createJaspState()
    jaspResults[["glinmod_results"]] <- glinmod_results
    glinmod_results$dependOn(c("dependent", "variables", "interactions"))
    
    ## interactions are stored in a deeply nested list. de-listify them
    predictors = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
      ), 
      collapse=" + ")
    f = paste0(options$dependent, " ~ ", predictors, collapse = "")
    f = as.formula(f)
    
    ## save results (for debugging purposes)
    
    #save(dataset, options, file="/Users/fife/Dropbox/jaspresults.Rdat")
    
    ### model it
    model = lm(f, dataset)
    #save(options, dataset, model, f, file="/Users/fife/Dropbox/jaspresults.Rdat")
    ### store all the information
    est = estimates(model)
    est$model = model
    
    glinmod_results$object = est
    
    return()
  }
}

.create_glinmod_table_means <- function(jaspResults, options, ready) {
  glinmod_table_means <- createJaspTable(title = "Means of Categorical Variables")
  
  ### which options are required
  glinmod_table_means$dependOn(c("dependent", "variables", "ci", "interactions"))
  
  ### add citation
  glinmod_table_means$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table_means$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  glinmod_table_means$addColumnInfo(name = "levels",    title = "Level",       type = "string", combine = TRUE)	
  glinmod_table_means$addColumnInfo(name = "est",      title = "Estimate",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    glinmod_table_means$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    glinmod_table_means$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  ### add message about what type of interval was used
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
  glinmod_table_means$addFootnote(message)  
  glinmod_table_means$showSpecifiedColumnsOnly <- TRUE
  
  ### store the table structure
  jaspResults[["glinmod_table_means"]] <- glinmod_table_means
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  ### fill the table with those results
  .fill_glinmod_table_means(glinmod_table_means, glinmod_results)
  
  return()
  
}

.create_glinmod_table_differences <- function(jaspResults, options, ready) {
  glinmod_table_differences <- createJaspTable(title = "Mean Differences Between Groups")
  
  ### which options are required
  glinmod_table_differences$dependOn(c("dependent", "variables", "ci", "interactions"))
  
  ### add citation
  glinmod_table_differences$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table_differences$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  glinmod_table_differences$addColumnInfo(name = "comparison",    title = "Comparison",       type = "string", combine = TRUE)	
  glinmod_table_differences$addColumnInfo(name = "diff",      title = "Difference",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    glinmod_table_differences$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    glinmod_table_differences$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  glinmod_table_differences$addColumnInfo(name = "cohensd",      title = "Cohen's d",    type = "number", format = "dp:2", combine = TRUE)	
  
  ### add message about what type of interval was used
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
  glinmod_table_differences$addFootnote(message)  
  glinmod_table_differences$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["glinmod_table_differences"]] <- glinmod_table_differences
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  ### fill the table with those results
  .fill_glinmod_table_differences(glinmod_table_differences, glinmod_results)
  
  return()
}

.create_glinmod_table_slopes <- function(jaspResults, options, ready) {
  glinmod_table_slopes <- createJaspTable(title = "Regression Slopes and Intercept")
  
  ### which options are required
  glinmod_table_slopes$dependOn(c("dependent", "variables", "ci", "interactions"))
  
  ### add citation
  glinmod_table_slopes$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table_slopes$addColumnInfo(name = "var",      title = "Variables",   type = "string", combine = TRUE)
  glinmod_table_slopes$addColumnInfo(name = "val",    title = "Value",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    glinmod_table_slopes$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    glinmod_table_slopes$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  glinmod_table_slopes$addColumnInfo(name = "std",    title = "Standardized Slope (β)",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    glinmod_table_slopes$addColumnInfo(name = "slwr",      title = "Lower β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    glinmod_table_slopes$addColumnInfo(name = "supr",      title = "Upper β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  ### add message about what type of interval was used
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
  
  message = paste0(message, "\n All estimates are conditional estimates.")
  glinmod_table_slopes$addFootnote(message)  
  glinmod_table_slopes$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["glinmod_table_slopes"]] <- glinmod_table_slopes
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  ### fill the table with those results
  .fill_glinmod_table_slopes(glinmod_table_slopes, glinmod_results)
  
  return()
}

.create_glinmod_table_modcomp <- function(jaspResults, options, ready) {
  glinmod_table_modcomp <- createJaspTable(title = "Model Comparisons (Estimating the Effect of Removing Terms)")
  
  ### which options are required
  glinmod_table_modcomp$dependOn(c("dependent", "variables", "ci", "interactions"))
  
  ### add citation
  glinmod_table_modcomp$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table_modcomp$addColumnInfo(name = "terms",      title = "Term",   type = "string", combine = TRUE)
  glinmod_table_modcomp$addColumnInfo(name = "rsq",    title = "Semi-partial R Squared",       type = "number", format = "dp:2", combine = TRUE)	
  glinmod_table_modcomp$addColumnInfo(name = "bayes",      title = "Semi-partial Bayes Factor",       type = "number", format = "dp:2", combine = TRUE)
  
  
  message = paste0("message \n Note: Semi-partials indicate the effect of removing that particular term from the model. Higher Bayes Factors indicate important terms. Lower Bayes Factors suggest they can be removed from the model")
  glinmod_table_modcomp$addFootnote(message)  
  glinmod_table_modcomp$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["glinmod_table_modcomp"]] <- glinmod_table_modcomp
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  ### fill the table with those results
  .fill_glinmod_table_modcomp(glinmod_table_modcomp, glinmod_results)
  
  return()
}



.fill_glinmod_table_means = function(glinmod_table_means, glinmod_results){
  

  factors = glinmod_results$factor.summary
  
  ### output results
  tabdat = list(
    var = as.character(factors$variables),
    levels = factors$levels,
    est = factors$estimate,
    lwr = factors$lower,
    upr = factors$upper
  )
  glinmod_table_means$setData(tabdat)
  
  
  return()
}

.fill_glinmod_table_differences = function(glinmod_table_differences, glinmod_results){
  
  
  diff = glinmod_results$difference.matrix
  
  ### output results
  tabdat = list(
    var = as.character(diff$variables),
    comparison = diff$comparison,
    diff = diff$difference,
    lwr = diff$lower,
    upr = diff$upper,
    cohensd = diff$cohens.d
  )
  
  glinmod_table_differences$setData(tabdat)
  
  
  return()
}

.fill_glinmod_table_slopes = function(glinmod_table_slopes, glinmod_results){
  
  slopes = glinmod_results$numbers.summary
  
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
  
  glinmod_table_slopes$setData(tabdat)
  
  
  return()
}

.fill_glinmod_table_modcomp = function(glinmod_table_modcomp, glinmod_results){
  
  mc = glinmod_results$model.comparison
  
  
  ### reformat : to be a times
  term.labels = as.character(mc$all.terms)
  term.labels = gsub(":", "×", term.labels)
  ### output results
  tabdat = list(
    terms = term.labels,
    rsq = mc$rsq,
    bayes = mc$bayes.factor
  )
  
  glinmod_table_modcomp$setData(tabdat)
  
  
  return()
}


.check_glinmod_error = function(dataset, options){
  
  # check length of variables
  if ((options$dependent == "" & length(options$paneledVars)>0) | (options$dependent == "" & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
  if (options$dependent != "" & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")
  
}

.read_glinmod_data = function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    dataset = .readDataSetToEnd(columns=(c(options$dependent, options$variables))) 
    ## variable names in the dataset are encoded. de-encodify them
    names(dataset) = JASP:::.unv(names(dataset))
    return(dataset)
}
