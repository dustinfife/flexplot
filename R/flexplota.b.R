flexplotaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flexplotaClass",
    inherit = flexplotaBase,
    private = list(
  
  .run = function() {
    if (length(self$options$out)>0){
      formula = jamovi_formula(self$options$out, self$options$preds, self$options$given)
      output = list(formula=formula, data=self$data)
      image <- self$results$plot
      image$setState(output)
    }
	}, 
			
	.plot = function(image, ...){

	  # return no plot					
	  if (is.null(image$state)) return(FALSE)		  
	  
	  # return the jamovi plot
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
}
		
				
))