#' Flexplot in JASP
#'
#' This function was developed for use in JASP. It takes a dataset as input with user 
#' options and returns a flexplot graphic
#' 
#' @param jaspResults A JASP  object
#' @param dataset dataset supplied by JASP
#' @param options a list of options to pass to JASP
#'
#' @return a flexplot graphic. 
#' @export
flexplot_jasp2 = function(jaspResults, dataset, options) {
  
  ### check if they've entered anything	  
  
  ready <- (options$dependent != "") 
  
  ### read in the dataset 
  if (ready) {
    ### read in the dataset
    if (is.null(dataset)){ 
      dataset = (.readDataSetToEnd(columns=c(options$dependent, options$variables, options$paneledVars)))
      #save(dataset, ready, options, file="~/Users/Documents/jaspdata.rdat")
    } else {
      return(dataset) 
    }
  }
  
  ### create plots
  .flexPlotRes(jaspResults, formula, dataset, options, ready)
}


.flexPlotRes <- function(jaspResults, formula, dataset, options, ready) {
  #### set up parameters
  flex_Plot <- createJaspPlot(title = "Flexplot",  width = 600, height = 450)
  flex_Plot$dependOn(c("confidence", "dependent", "variables", "paneledVars", "ghostLines", "bw"))
  flex_Plot$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")
  
  #### pre-populate the jasp object
  jaspResults[["flex_Plot"]] <- flex_Plot
  
  if (!ready){
    return()
  }
  
  #### prepare the data for flexplot
  k = data.frame(matrix(nrow=nrow(dataset), ncol=length(options$variables) + length(options$dependent) + length(options$paneledVars)))
  names(k) = c(options$dependent, options$variables, options$paneledVars)
  variables <- unlist(options$variables)
  panels <- unlist(options$paneledVars)
  if (length(panels)>0){
    vars = c(variables, panels)
  } else {
    vars = variables
  }
  k[,1] = dataset[[encodeColNames(options$dependent)]]
  if (length(vars)>0){		#### this statement is necessary to allow histograms
    for (i in 2:(length(vars)+1)){
      k[,i] = dataset[[encodeColNames(vars[i-1])]]
    }
  }
  dv = encodeColNames(unlist(options$dependent));
  v = encodeColNames(unlist(options$variables)); 
  p = encodeColNames(unlist(options$paneledVars))
  #save(jaspResults, k, dataset, v,p, dv,options, file="~/Documents/JaspResults.Rdata")
  
  if (length(options$variables)==0){
    formula = as.formula(paste0(dv, "~1"))
  } else if (length(options$paneledVars)>0){
    formula = as.formula(paste0(dv, 
                                "~", 
                                paste0(v, collapse="+"), 
                                " | ", 
                                paste0(p, collapse="+")
                                ))
  } else {
    formula = as.formula(paste0(dv, "~", paste0(v, collapse="+")))
  }
  #tst = data.frame(x=1:10, y=1:10)
  
  #### do a ghost line
  if	(options$ghost){
    ghost=rgb(195/255,0,0,.3) 
  } else {
    ghost = NULL
  }
  
  whiskers = list("Quartiles" = "quartiles",
                  "Standard errors" = "sterr",
                  "Standard deviations" = "stdev")
  
  linetype = tolower(options$type)
  
  #save(k, formula, file="/Users/fife/Documents/jaspbroke.rdata")
  jitter = c(options$jitx,options$jity)
  if (linetype == "regression") linetype = "lm"
  
  plot = flexplot(formula, data=dataset, method=linetype, se=options$confidence, alpha=options$alpha,
                            ghost.line=ghost,
                            spread=whiskers[[options$intervals]],
                            jitter = jitter)
  plot <- theme_it(plot, options$theme)
  if (options$bw) {
    plot = plot + scale_colour_grey(start = 0, end = .9)
  }
  colsnstuff = ifelse(length(options$variables)>1, options$variables[2], "")
  plot = .fancifyMyLabels(plot, options)
  
  # #### create flexplot object   
  flex_Plot$plotObject <- plot
  return()   
}

#### create function that figures out how to label things
.fancifyMyLabels = function(plot, options){
  
  # univariate plots
  if (length(options$variables) == 0) {
    x = options$dependent
    y = "Count"
    # bivariate (or more)
  } else {
    y = options$dependent
    x = options$variables[1]
  }
  
  # if second slot is occupied
  if (length(options$variables)>1) {
    col = options$variables[2]
    lines = options$variables[2]
    shape = options$variables[2]
  } else {
    col = ""; lines = ""; shape=""
  }
  
  ## if there's panels
  if (length(options$paneledVars) == 1) {
    names(plot$facet$params$cols) = options$paneledVars[1]
  } else if (length(options$paneledVars) == 2) {
    names(plot$facet$params$cols) = options$paneledVars[1]
    names(plot$facet$params$rows) = options$paneledVars[2]
  }
  
  plot = plot + labs(x=x, y=y, col=col, linetype=lines, shape=shape)
  
  
  
}

#### create a table
.printedResults = function(jaspResults, dataset, options, ready){
  
  if (!is.null(jaspResults[["resultsTable"]])) return()
  
  # Create Table
  resultsTable <- createJaspTable(title="Flexplot Table")
  resultsTable$dependOn(c("variables", "dependent", "confidence", "type"))
  resultsTable$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")
  
  resultsTable$showSpecifiedColumnsOnly <- TRUE
  
  ### add columns to table
  resultsTable$addColumnInfo(name = "var",   title = "Variable",   type = "string", combine = TRUE)
  resultsTable$addColumnInfo(name = "mean",   title = "Mean",   type = "string", combine = TRUE)	
  resultsTable$addColumnInfo(name = "median",   title = "Median",   type = "string", combine = TRUE)	
  resultsTable$addColumnInfo(name = "typ",   title = "Type",   type = "string", combine = TRUE)	
  
  #### tell jasp how many rows we expect
  if (ready)
    resultsTable$setExpectedSize(length(options$variables))
  
  for (variable in options$variables){
    #row <- c(variable, options$dependent, options$confidence, options$type)
    resultsTable$addRows(list(
      "var" = variable,
      "mean" = mean(dataset[[encodeColNames(variable)]]),
      "median" = median(dataset[[encodeColNames(variable)]]), 
      "typ" = mode(dataset[[encodeColNames(variable)]])))
  }
  #f <- paste0(options$dependent, "~", paste0(options$variables, collapse="+"))
  #message <- options$variables
  resultsTable$addFootnote(message="hello", symbol="<em>Note.</em>")	
  
  jaspResults[["resultsTable"]] <- resultsTable
}

#### read in data
.flexReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns=c(options$dependent, options$variables)))
}




themeJasp = function(graph,
                     xAxis = TRUE,
                     yAxis = TRUE,
                     sides = "bl",
                     axis.title.cex = getGraphOption("axis.title.cex"),
                     bty = getGraphOption("bty"),
                     fontsize = getGraphOption("fontsize"),
                     family = getGraphOption("family"),
                     horizontal = FALSE,
                     legend.position = "right",
                     legend.justification = "top",
                     axisTickLength = getGraphOption("axisTickLength"),
                     axisTickWidth = getGraphOption("axisTickWidth")) {
  
  # if (!xAxis || !yAxis) {
  #   warning("Arguments xAxis and yAxis of themeJasp will be deprecated. Please use the argument \"sides\" instead.")
  #   
  #   if (horizontal) {
  #     if (!xAxis)
  #       #sides <- stringr::str_remove(sides, "l")
  #     if (!yAxis)
  #       #sides <- stringr::str_remove(sides, "b")
  #   } else {
  #     if (!xAxis)
  #       #sides <- stringr::str_remove(sides, "b")
  #     if (!yAxis)
  #       #sides <- stringr::str_remove(sides, "l")
  #   }
  #   if (sides == "")
  #     bty <- NULL
  # }
  
  
  if (is.list(bty) && bty[["type"]] == "n")
    graph <- graph + geom_rangeframe(sides = sides)
  
  if (horizontal)
    graph <- graph + coord_flip()
  
  graph <- graph +
                  themeJaspRaw(legend.position = legend.position,
                                axis.title.cex = axis.title.cex, family = family,
                                fontsize = fontsize, legend.justification = legend.justification,
                                axisTickLength = axisTickLength, axisTickWidth = axisTickWidth) +
            theme(panel.spacing = unit(.2, "cm"))
                                
  
  return(graph)
  
}
