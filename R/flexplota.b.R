
# This file is a generated template, your changes will not be overwritten

flexplotaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flexplotaClass",
    inherit = flexplotaBase,
    private = list(
        .run = function() {

			if (length(self$options$out)>0 & length(self$options$preds)>0){
		        	#### write formula for flexplot
		            formula <- jmvcore::constructFormula(self$options$out, self$options$preds)
		            formula <- as.formula(formula)
	
					output = list(formula=formula, data=self$data)
					image <- self$results$plot
					image$setState(output)
			} else if (length(self$options$out)>0 & length(self$options$preds)==0){
		        	#### write formula for flexplot
		            formula <- paste0(self$options$out, "~1")
		            formula <- as.formula(formula)
	
					output = list(formula=formula, data=self$data)
					image <- self$results$plot
					image$setState(output)
			}
			return(formula)
		}
		
		, 
			
		.plot = function(image, ...){
            if (is.null(image$state))
                return(FALSE)
            se.type = subsetString(self$options$center," + ", position=2)   			
			formula = image$state$formula
			data = image$state$data
            plot = flexplot(formula, data=data, se=self$options$se, method=self$options$line, spread=se.type, alpha = self$options$alpha)
			print(plot)
			TRUE
		}		
))