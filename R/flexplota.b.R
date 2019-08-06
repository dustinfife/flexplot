
# This file is a generated template, your changes will not be overwritten

flexplotaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flexplotaClass",
    inherit = flexplotaBase,
    private = list(
        .run = function() {
        	
						
			
			#### if they have both predictor and outcome
			if (length(self$options$out)>0 & length(self$options$preds)>0){
				
		        	#### write formula for flexplot
		            formula <- jmvcore::constructFormula(self$options$out, self$options$preds)
		        
		            if (length(self$options$given)>0){
			            formula <- paste0(self$options$out, "~", paste0(self$options$preds, collapse="+"), "|", paste0(self$options$given, collapse="+"))		            	
		            }
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
       		
       		#### change case so line type can be read in
			if (self$options$line=="Loess"){line="loess"}
			if (self$options$line=="Regression"){line ="lm"}
			if (self$options$line=="Logistic"){line ="logistic"}
						
            if (is.null(image$state))
                return(FALSE)
            se.type = subsetString(self$options$center," + ", position=2)   			
			formula = image$state$formula
			data = image$state$data
			
				### if they choose to residualize it
			if (length(self$options$given)>0 & self$options$resid==TRUE) {
            	plot = added.plot(formula, data=data, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha)	
            	### if they choose to do a ghost line	
            } else if (length(self$options$given)>0 & self$options$ghost==TRUE){
	            plot = flexplot(formula, data=data, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha, ghost.line="darkgray") + 
	            		theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
	            		theme_bw(base_size = 16)
            } else {	
            	plot = flexplot(formula, data=data, se=self$options$se, spread=se.type,method=line,  alpha = self$options$alpha) + 
	            		theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
	            		theme_bw(base_size = 16)
            }
			print(plot)
			TRUE
		}		
))