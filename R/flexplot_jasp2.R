flexplot_jasp2<- function(jaspResults, dataset, options) {

        #jaspResults$title <- "Flexplot"

	### check if they've entered anything	  
	ready <- (length(options$dependent) > 0) #&& length(options$variables)>0)

		#### check for errors
	customChecksFlexPlot <- list(
		function(){
			if ((length(options$dependent)==0 & length(options$paneledVars)>0) | (length(options$dependent)==0 & length(options$variables)>0)) {
					return(paste0("You must specify a dependent variable to view a graphic"))
				}
			},
			function(){
				if(length(options$dependent)!=0 & length(options$paneledVars)>0){
					return(paste0("You must have at least one independent variable to do paneling"))
				}
			})

		error <- .hasErrors(dataset=dataset, perform=perform, type=c("infinity", "variance"), custom= customChecksFlexPlot, exitAnalysisIfErrors=TRUE)
	  
	### read in the dataset 
	if (ready) {
		### read in the dataset
		if (is.null(dataset)){ 
    		dataset = (.readDataSetToEnd(columns=c(options$dependent, options$variables, options$paneledVars)))
		} else {
			return(dataset) 
	    }


		### create a table output
		#.printedResults(jaspResults, dataset, options, ready)

		### change name of smoothed line
		if (options$type=="regression"){options$type=="lm"}

		### create plots
		#if (is.null(jaspResults[["flex_Plot"]])){
		.flexPlotRes(jaspResults, formula, dataset, options, ready)
		
		return()	  
		#}  
	} else {
		return()
	} 
}


			
			
.flexCheckErrors <- function(dataset, options){
	
	# check length of variables
	if ((length(options$dependent)==0 & length(options$paneledVars)>0) | (length(options$dependent)==0 & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
	if (length(options$dependent)!=0 & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")

}


.flexPlotRes <- function(jaspResults, formula, dataset, options, ready) {
	
	require(ggplot2)
	
	#### set up parameters
	flex_Plot <- createJaspPlot(title = "Flexplot",  width = 900, height = 900)
	flex_Plot$dependOn(c("confidence", "dependent", "variables", "paneledVars", "ghostLines"))
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
		k[,1] = dataset[[.v(options$dependent)]]
		if (length(vars)>0){		#### this statement is necessary to allow histograms
			for (i in 2:(length(vars)+1)){
				k[,i] = dataset[[.v(vars[i-1])]]
			}
		}

		if (length(options$variables)==0){
			formula = as.formula(paste0(options$dependent, "~1"))		
		} else if (length(options$paneledVars)>0){
			formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+"), " | ", paste0(panels, collapse="+")))
		} else {
			formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+")))
		}
		tst = data.frame(x=1:10, y=1:10)
		require(ggplot2)
		
		#### do a ghost line
		if	(length(options$ghostLines)[1]>0){
			ghost.length = length(unlist(options$ghostLines))
			num.vars = seq(from=2, to=ghost.length, by=2)
			ghost.vars = unlist(options$ghostLines)[num.vars]
			f = function(name){
				mean.var = round(mean(dataset[[.v(name)]], na.rm=T))
				return(mean.var)
			}
			ghost.lines = lapply(ghost.vars, FUN=f)
			names(ghost.lines) = ghost.vars
			#ghost.lines = split(ghost.lines, ghost.vars)
			plot = flexplot(formula, data=k, method=options$type, se=options$confidence, ghost.line="gray", ghost.reference=ghost.lines) + xlab(names(ghost.lines))		
			#plot = flexplot(formula, data=k, method=options$type, se=options$confidence, ghost.line="gray", ghost.reference=ghost.lines) + xlab(paste0(ghost.lines)) + ylab(as.character(formula))
			# #plot = flexplot(formula, data=k, method=options$type, se=options$confidence) + xlab(paste0(ghost.lines, collapse=","))
		} else {
			plot = flexplot(formula, data=k, method=options$type, se=options$confidence) + xlab(paste0(unlist(options$ghostLines)))
		}

		#plot = ggplot(data=tst, aes(x,y)) + ggtitle(options$dependent) +
 		 #xlab(paste0(length(unlist(options$ghostLines)))) + ylab("Teeth length")

		# #### create flexplot object   
		# require(ggplot2)
		#plot = flexplot(formula, data=k, method=options$type, se=options$confidence) 
		#flex_Plot$plotObject <- JASPgraphs::themeJasp(plot) + ggplot2::theme(strip.text = ggplot2::element_text(size = 14), panel.margin=unit(.15, "lines"), strip.background = element_rect(color = "gray", fill="white", size = 1))
		flex_Plot$plotObject <- plot
		return()   
}   

  
.flexplotFill <- function(flex_Plot, formula, dataset, options){
  #f <- as.formula(paste0(options$dependent, "~", paste0(options$variables, collapse="+")))
  # k <- data.frame(A=1:10, B=1:10)
  #plot <- ggplot2::ggplot(data=dataset, ggplot2::aes(x=A, y=B)) + geom_point()
  
  #plot = ggplot2::ggplot(data=dataset, ggplot2::aes_string(y=options$dependent, x=options$dependent)) + geom_point()
 
  
 
  return()
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
						"mean" = mean(dataset[[.v(variable)]]),
						"median" = median(dataset[[.v(variable)]]), 
						"typ" = mode(dataset[[.v(variable)]])))
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





# # 

# .fillTableMain <- function


	
	# .flexPlotRes <- function(jaspResults, dataset, options, ready) {
	  # flex_Plot <- createJaspPlot(title = "Flexplot",  width = 160, height = 320)
          # flex_Plot$dependOn(c("variables", "dependent"))
	
		# flex_Plot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
	  
	  # flex_Plot$addFootnote(message="Some descriptive text we want to show!", symbol="<em>Note.</em>")
	  
	  # jaspResults[["flex_Plot"]] <- flex_Plot
	  
	  # if (!ready)
	    # return()
	    
	    # .flexFillPlotDescriptives(flex_Plot, dataset, options)
	
		# return()
	
	 # } 
	 
	 # .flexFillPlotDescriptives <- function(flex_Plot, dataset, options){
	 
	 # #### make a formula
	 # #form = make.formula(options$dependent, options$variables)
	 # variables = unlist(options$variables)
	 # depend = unlist(options$dependent)
	 # form = as.formula(paste0(depend, "~", paste0(variables, collapse="+")))
	  # plot <- try(flexplot(form, data=dataset))
	  
	  # if (inherits(plot, "try-error") {
	    # errorMessage <- as.character(plot)
	    # flex_Plot$setError(errorMessage)
	    # return()
	  # }
	  
	  # flex_Plot$plotObject <- plot
	 
	  # return()
	
	# }

# }
# # 

	# #### prepare the data for flexplot
	# k = matrix(nrow=nrow(dataset), ncol=length(options$variables) + length(options$dependent)+ length(options$paneledVars))
	# variables <- unlist(options$variables)
	# if (length(options$paneledVars)>0){
		# pvars = unlist(options$paneledVars)		
		# all.vars = c(variables, pvars)
		# formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+"), "|", paste0(pvars, collapse="+")))		
	# }	else {
		# all.vars = variables
		# formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+")))
	# }	
	# k[,1] = dataset[[.v(options$dependent)]]
	# for (i in 2:(length(all.vars)+1)){
		# k[,i] = dataset[[.v(all.vars[i-1])]]
	# }
	# k = data.frame(k)
	# names(k) = c(options$dependent, variables)
	# #formula = 

	# #### create flexplot object   
	# require(ggplot2)
	# plot = flexplot(formula, data=k, method=options$type, se=options$confidence) 
	# flex_Plot$plotObject <- plot



