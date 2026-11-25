jamovi_flexplot_reset = function(results) {
  for (item in results$items) {
    item$setVisible(FALSE)
    jamovi_result_clear(item)
    if (inherits(item,"Image")) item$.setPath(NULL)
  }
  jamovi_plot_clear_states(results,"plot")
  jamovi_diagnostic(results$waiting,"Select an outcome variable to create a plot.")
}

# Analysis$asProtoBuf collects the bibliography with getRefs(recurse=TRUE),
# whereas ResultsElement$asProtoBuf calls getRefs() for on-result callouts.
# Keep the declared references in the bibliography without repeating markers
# below every ordinary plot or waiting message. Use the public R6 extension API
# here so compilation of the generated header preserves this behavior.
jamovi_flexplot_bibliography = function() {
  flexplotaResults$set("public","getRefs",function(recurse=FALSE) {
    if (isTRUE(recurse)) super$getRefs(recurse=TRUE) else character()
  },overwrite=TRUE)
}

flexplotaClass = if (requireNamespace('jmvcore')) R6::R6Class(
    "flexplotaClass",
    inherit = flexplotaBase,
    public = list(initialize=function(...) {
      jamovi_flexplot_bibliography()
      super$initialize(...)
    }),
    private = list(
  .init = function() jamovi_flexplot_reset(self$results),
  
  .run = function() {
    jamovi_flexplot_reset(self$results)
    if (length(self$options$out)>0){
      self$results$waiting$setVisible(FALSE)
      output = tryCatch({
      formula = jamovi_formula(self$options$out, self$options$preds, self$options$given)
      data = self$data
      if (!any(stats::complete.cases(data[,all.vars(formula),drop=FALSE])))
        stop("No complete observations are available for the selected plot variables.",call.=FALSE)
      list(formula=formula, data=data)
      },error=identity)
      if (inherits(output,"error")) {
        jamovi_diagnostic(self$results$explanation,conditionMessage(output),error=TRUE)
        return(invisible(NULL))
      }
      image = self$results$plot
      image$setState(output)
      image$setVisible(TRUE)
    }
	}, 
			
	.plot = function(image, ...){

	  # return no plot					
	  if (is.null(image$state)) return(FALSE)		  
	  
	  # return the jamovi plot
    tryCatch({
    p = jamovi_plots(image$state$formula, image$state$data, self$options)

		#### modify geoms (if they choose to)
    geoms = sapply(p$layers, function(x) class(x$geom)[1])
    if (self$options$plmethod != "Jittered-density plot" & "GeomErrorbar" %in% geoms){
      		
 				#### delete old summary
			p$layers[[2]] = NULL
			p$layers[[2]] = NULL
			p$layers[[2]] = NULL
			
			##### figure out the correct geom
			if (self$options$plmethod == "Boxplot"){
				g = geom_boxplot()
			} else if (self$options$plmethod=="Violin plot"){
				g = geom_violin()
			}
			
			#### add new layer
			p$layers = c(g, p$layers)
			
    }

		print(p)
		TRUE
    },error=function(error) {
      image$setState(NULL)
      jamovi_glm_plot_error(image,error)
    })
}
		
				
))
