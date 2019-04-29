# This file is a generated template, your changes will not be overwritten

modelcompClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "modelcompClass",
    inherit = modelcompBase,
    private = list(
        .run = function() {

			if (length(self$options$dep)>0 & length(self$options$pred)>0 & length(self$options$pred2)>0){
				        	#### write formula for flexplot
				            formula1 <- jmvcore::constructFormula(self$options$dep, self$options$pred)
				            formula2 <- jmvcore::constructFormula(self$options$dep, self$options$pred2)				            
				            
				            ### fit the models
				            model1 = lm(formula1, data=self$data)
				            model2 = lm(formula2, data=self$data)		
				            
				            ### compare the models
				            modcomp = model.comparison(model1, model2)		
				            print(modcomp)            

					#return(formula)
				}}			

        )
)
