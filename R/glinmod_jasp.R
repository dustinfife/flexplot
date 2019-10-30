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
  }
  
  ### check if there's a jasp table already. if not, create it
  if (is.null(jaspResults[["glinmod_table"]])){
    .create_glinmod_table(jaspResults, dataset, options, ready)
  }  
}

.create_glinmod_table <- function(jaspResults, dataset, options, ready) {
  glinmod_table <- createJaspTable(title = "Means of Categorical Variables")
  
  ### which options are required
  glinmod_table$dependOn(c("dependent", "variables"))
  
  ### add citation
  glinmod_table$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  glinmod_table$addColumnInfo(name = "levels",    title = "Level",       type = "string", combine = TRUE)	
  glinmod_table$addColumnInfo(name = "est",      title = "Estimate",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    glinmod_table$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    glinmod_table$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }

  ### add message about what type of interval was used
  message <- switch(options$estimationmethod,
                    "Bootstrapped Intervals"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Credible Interval"  = paste0("Confidence intervals computed using 95% ", options$estimationmethod),
                    "Confidence Interval"  = paste0("Confidence intervals computed 95% ", options$estimationmethod)
  )
  glinmod_table$addFootnote(message)  
  glinmod_table$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["glinmod_table"]] <- glinmod_table
  
  ### return the table
  if (!ready) {
    return()
  } else {
    .fill_glinmod_table(glinmod_table, dataset, options)
    return()
  }
  
}

# for testing: 
# dataset = exercise_data; require(flexplot); data(exercise_data)
# options = list(dependent = "weight.loss", variables=c("gender"))
# f = weight.loss~gender
.fill_glinmod_table = function(glinmod_table, dataset, options){
  
  #save(glinmod_table, dataset, options,)
  ### make a formula from input
  predictors = paste0(
      unlist(
          lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
            ), 
      collapse=" + ")
  f = paste0(options$dependent, " ~ ", predictors, collapse = "")
  f = as.formula(f)
  names(dataset) = JASP:::.unv(names(dataset))
  save(options, dataset, f, file="/Users/fife/Dropbox/jaspresults.Rdat")
  #save(dataset, options, file="/Users/fife/Dropbox/jaspresults.Rdat")
  ### store results
  model = lm(f, dataset)
  
  
  est = estimates(model)
  factors = est$factor.summary
  differences = est$difference.matrix
  numeric = est$numbers.summary
  
  ### output results
  tabdat = list(
    var = as.character(factors$variables),
    levels = factors$levels,
    est = factors$estimate,
    lwr = factors$lower,
    upr = factors$upper
  )
  glinmod_table$setData(tabdat)
  
  
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
    return(.readDataSetToEnd(columns=(c(options$dependent, options$variables))))
}

# 
# .flexPlotRes <- function(jaspResults, formula, dataset, options, ready) {
#   
#   #require(ggplot2)
#   
#   #### set up parameters
#   flex_Plot <- createJaspPlot(title = "Flexplot",  width = 900, height = 900)
#   flex_Plot$dependOn(c("confidence", "dependent", "variables", "paneledVars", "ghostLines"))
#   flex_Plot$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")
#   
#   #### pre-populate the jasp object
#   jaspResults[["flex_Plot"]] <- flex_Plot
#   
#   if (!ready){
#     return()
#   }
#   
#   #### prepare the data for flexplot
#   k = data.frame(matrix(nrow=nrow(dataset), ncol=length(options$variables) + length(options$dependent) + length(options$paneledVars)))
#   names(k) = c(options$dependent, options$variables, options$paneledVars)
#   variables <- unlist(options$variables)
#   panels <- unlist(options$paneledVars)
#   if (length(panels)>0){
#     vars = c(variables, panels)
#   } else {
#     vars = variables
#   }
#   k[,1] = dataset[[.v(options$dependent)]]
#   if (length(vars)>0){		#### this statement is necessary to allow histograms
#     for (i in 2:(length(vars)+1)){
#       k[,i] = dataset[[.v(vars[i-1])]]
#     }
#   }
#   
#   if (length(options$variables)==0){
#     formula = as.formula(paste0(options$dependent, "~1"))		
#   } else if (length(options$paneledVars)>0){
#     formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+"), " | ", paste0(panels, collapse="+")))
#   } else {
#     formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+")))
#   }
#   tst = data.frame(x=1:10, y=1:10)
#   require(ggplot2)
#   
#   #### do a ghost line
#   if	(length(options$ghostLines)[1]>0){
#     ghost.length = length(unlist(options$ghostLines))
#     num.vars = seq(from=2, to=ghost.length, by=2)
#     ghost.vars = unlist(options$ghostLines)[num.vars]
#     f = function(name){
#       mean.var = round(mean(dataset[[.v(name)]], na.rm=T))
#       return(mean.var)
#     }
#     ghost.lines = lapply(ghost.vars, FUN=f)
#     names(ghost.lines) = ghost.vars
#     #ghost.lines = split(ghost.lines, ghost.vars)
#     plot = flexplot(formula, data=k, method=options$type, se=options$confidence, ghost.line="gray", ghost.reference=ghost.lines) + xlab(names(ghost.lines))		
#     #plot = flexplot(formula, data=k, method=options$type, se=options$confidence, ghost.line="gray", ghost.reference=ghost.lines) + xlab(paste0(ghost.lines)) + ylab(as.character(formula))
#     # #plot = flexplot(formula, data=k, method=options$type, se=options$confidence) + xlab(paste0(ghost.lines, collapse=","))
#   } else {
#     plot = flexplot(formula, data=k, method=options$type, se=options$confidence) + xlab(paste0(unlist(options$ghostLines)))
#   }
#   
#   #plot = ggplot(data=tst, aes(x,y)) + ggtitle(options$dependent) +
#   #xlab(paste0(length(unlist(options$ghostLines)))) + ylab("Teeth length")
#   
#   # #### create flexplot object   
#   # require(ggplot2)
#   #plot = flexplot(formula, data=k, method=options$type, se=options$confidence) 
#   #flex_Plot$plotObject <- JASPgraphs::themeJasp(plot) + ggplot2::theme(strip.text = ggplot2::element_text(size = 14), panel.margin=unit(.15, "lines"), strip.background = element_rect(color = "gray", fill="white", size = 1))
#   flex_Plot$plotObject <- plot
#   return()   
# }   
# 
# 
# .flexplotFill <- function(flex_Plot, formula, dataset, options){
#   #f <- as.formula(paste0(options$dependent, "~", paste0(options$variables, collapse="+")))
#   # k <- data.frame(A=1:10, B=1:10)
#   #plot <- ggplot2::ggplot(data=dataset, ggplot2::aes(x=A, y=B)) + geom_point()
#   
#   #plot = ggplot2::ggplot(data=dataset, ggplot2::aes_string(y=options$dependent, x=options$dependent)) + geom_point()
#   
#   
#   
#   return()
# }
# 
# 
# #### create a table
# .printedResults = function(jaspResults, dataset, options, ready){
#   
#   if (!is.null(jaspResults[["resultsTable"]])) return()
#   
#   # Create Table
#   resultsTable <- createJaspTable(title="Flexplot Table")
#   resultsTable$dependOn(c("variables", "dependent", "confidence", "type"))
#   resultsTable$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")
#   
#   resultsTable$showSpecifiedColumnsOnly <- TRUE
#   
#   ### add columns to table
#   resultsTable$addColumnInfo(name = "var",   title = "Variable",   type = "string", combine = TRUE)
#   resultsTable$addColumnInfo(name = "mean",   title = "Mean",   type = "string", combine = TRUE)	
#   resultsTable$addColumnInfo(name = "median",   title = "Median",   type = "string", combine = TRUE)	
#   resultsTable$addColumnInfo(name = "typ",   title = "Type",   type = "string", combine = TRUE)	
#   
#   #### tell jasp how many rows we expect
#   if (ready)
#     resultsTable$setExpectedSize(length(options$variables))
#   
#   for (variable in options$variables){
#     #row <- c(variable, options$dependent, options$confidence, options$type)
#     resultsTable$addRows(list(
#       "var" = variable,
#       "mean" = mean(dataset[[.v(variable)]]),
#       "median" = median(dataset[[.v(variable)]]), 
#       "typ" = mode(dataset[[.v(variable)]])))
#   }
#   #f <- paste0(options$dependent, "~", paste0(options$variables, collapse="+"))
#   #message <- options$variables
#   resultsTable$addFootnote(message="hello", symbol="<em>Note.</em>")	
#   
#   jaspResults[["resultsTable"]] <- resultsTable
# }
# 
# 
# 
# 
# 
# 
# 
# 
# #### read in data
# .flexReadData <- function(dataset, options) {
#   if (!is.null(dataset))
#     return(dataset)
#   else
#     return(.readDataSetToEnd(columns=c(options$dependent, options$variables)))
# }
# 
# 
# 
# 
# 
# # # 
# 
# # .fillTableMain <- function
# 
# 
# 
# # .flexPlotRes <- function(jaspResults, dataset, options, ready) {
# # flex_Plot <- createJaspPlot(title = "Flexplot",  width = 160, height = 320)
# # flex_Plot$dependOn(c("variables", "dependent"))
# 
# # flex_Plot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
# 
# # flex_Plot$addFootnote(message="Some descriptive text we want to show!", symbol="<em>Note.</em>")
# 
# # jaspResults[["flex_Plot"]] <- flex_Plot
# 
# # if (!ready)
# # return()
# 
# # .flexFillPlotDescriptives(flex_Plot, dataset, options)
# 
# # return()
# 
# # } 
# 
# # .flexFillPlotDescriptives <- function(flex_Plot, dataset, options){
# 
# # #### make a formula
# # #form = make.formula(options$dependent, options$variables)
# # variables = unlist(options$variables)
# # depend = unlist(options$dependent)
# # form = as.formula(paste0(depend, "~", paste0(variables, collapse="+")))
# # plot <- try(flexplot(form, data=dataset))
# 
# # if (inherits(plot, "try-error") {
# # errorMessage <- as.character(plot)
# # flex_Plot$setError(errorMessage)
# # return()
# # }
# 
# # flex_Plot$plotObject <- plot
# 
# # return()
# 
# # }
# 
# # }
# # # 
# 
# # #### prepare the data for flexplot
# # k = matrix(nrow=nrow(dataset), ncol=length(options$variables) + length(options$dependent)+ length(options$paneledVars))
# # variables <- unlist(options$variables)
# # if (length(options$paneledVars)>0){
# # pvars = unlist(options$paneledVars)		
# # all.vars = c(variables, pvars)
# # formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+"), "|", paste0(pvars, collapse="+")))		
# # }	else {
# # all.vars = variables
# # formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+")))
# # }	
# # k[,1] = dataset[[.v(options$dependent)]]
# # for (i in 2:(length(all.vars)+1)){
# # k[,i] = dataset[[.v(all.vars[i-1])]]
# # }
# # k = data.frame(k)
# # names(k) = c(options$dependent, variables)
# # #formula = 
# 
# # #### create flexplot object   
# # require(ggplot2)
# # plot = flexplot(formula, data=k, method=options$type, se=options$confidence) 
# # flex_Plot$plotObject <- plot
# 
# 
