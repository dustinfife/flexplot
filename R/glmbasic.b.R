
# This file is a generated template, your changes will not be overwritten

jamovi_glm_model_plot = function(model, se, ...) {
  # visualize.lm uses a descriptive added-variable plot for larger models.
  if (length(all.vars(stats::formula(model))[-1]) > 4L) {
    return(visualize(model, plot="model", se=se, ...))
  }
  plot = visualize(model, plot="model", report.se=se, ...)
  if (!se) return(plot)

  for (layer in plot$layers) {
    data = layer$data
    if (!is.data.frame(data) ||
        !all(c("prediction", "prediction.lwr", "prediction.upr") %in% names(data))) next
    numeric_x = is.numeric(rlang::eval_tidy(layer$mapping$x, data=data))
    if (numeric_x && !inherits(layer$geom, "GeomLine")) next
    if (!numeric_x && !inherits(layer$geom, "GeomPoint")) next
    mapping = layer$mapping
    mapping$ymin = ggplot2::aes(ymin=.data[["prediction.lwr"]])$ymin
    mapping$ymax = ggplot2::aes(ymax=.data[["prediction.upr"]])$ymax
    if (numeric_x) {
      mapping$fill = mapping$colour
      mapping$colour = mapping$linetype = NULL
      plot = plot + ggplot2::geom_ribbon(data=data, mapping=mapping, alpha=.2,
                                         inherit.aes=FALSE, show.legend=FALSE)
    } else {
      plot = plot + ggplot2::geom_errorbar(data=data, mapping=mapping, width=.1,
                      position=layer$position, inherit.aes=FALSE, show.legend=FALSE)
    }
  }
  plot
}

glmbasicClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "glmbasicClass",
    inherit = glmbasicBase,
    private = list(
      
        .run = function() {
        	if (length(self$options$out)>0 & length(self$options$preds)>0){
        	  
        	  #### write formula for glinmod
            data = self$data
            if (self$options$compare) {
              comparison = tryCatch(jamovi_glm_comparison(self$options, data), error=identity)
              if (inherits(comparison, "error")) {
                self$results$comparison$setError(conditionMessage(comparison))
                self$results$comparisonplot$setError(conditionMessage(comparison))
                return(invisible(NULL))
              }
              self$results$comparisonplot$setState(comparison)
              jamovi_glm_comparison_table(self$results$comparison, comparison)
              model = comparison$model1
              formula = stats::formula(model)
              data = comparison$data
            } else {
              formula = tryCatch(jamovi_glm_formula(self$options$out, self$options$preds,
                                                   self$options$modelTerms), error=identity)
              if (inherits(formula, "error")) {
                self$results$glmcat$setError(conditionMessage(formula))
                return(invisible(NULL))
              }
              model = if (is.null(self$options$modelTerms)) lm(formula, data=data) else
                tryCatch(lm(formula, data=data), error=identity)
              if (!is.null(self$options$modelTerms) &&
                  (inherits(model, "error") || model$rank < length(stats::coef(model)))) {
                self$results$glmcat$setError("The primary model is not estimable. Check its terms and complete observations.")
                return(invisible(NULL))
              }
            }
            results = if (!self$options$compare && is.null(self$options$modelTerms))
              estimates.lm(model, mc=FALSE) else tryCatch(estimates.lm(model, mc=FALSE), error=identity)
            
          	#### s*ve formula/dataset to a file (to be used for plotting)
          	if (length(self$options$preds)>0){
            output = list(formula=formula, data=data)
  			    	image <- self$results$plot
  			    	image$setState(output)
  				
  			    	image2 <- self$results$assumpplot
  			    	image2$setState(output)			
  		  	  }

            if (inherits(results, "error")) {
              self$results$glmcat$setError(paste("Primary-model summaries are unavailable:",
                                                conditionMessage(results)))
              return(invisible(NULL))
            }

            if (is.data.frame(results)) {
              self$results$glmcat$addRow(rowKey=1, values=list(var="Intercept",
                levels="", means=results$Mean, lower=results$Lower, upper=results$Upper))
              return(invisible(NULL))
            }

            has_factors = is.data.frame(results$factor.summary) && nrow(results$factor.summary) > 0L
            has_numbers = is.data.frame(results$numbers.summary) && nrow(results$numbers.summary) > 0L

  			#### prepare r square output
  			rsq_out = list(rsq = results$r.squared, semi.p = results$semi.p, correlation=results$correlation)
  			private$.rsq(rsq_out, preds = self$options$preds)	
  									
  
  			#### prepopulate table
        table = self$results$glmcat			
              
              #### format factor summary to look nice
  			f = function(x){ x[is.na(x)] = "-"; x}
      
  			#### if there are factors, report those results....           
			if (has_factors){
  				
  			#### prepare difference scores output
  			diff.out = list(diff = results$difference.matrix)
  			private$.diff(results$difference.matrix)						   
  				
  				#### prepoulate first row with label
  				table$addRow(rowKey=1, values=list(
  					var = "Factors (Estimates reported are means)",
  					levels="",
  					means="",
  					lower="",
  					upper=""
  				))
  					

    		#### make output for categorical predictors

        results$factor.summary[,3:ncol(results$factor.summary)] = apply(results$factor.summary[,3:ncol(results$factor.summary)], 2, round, digits=2)
        results$factor.summary[,3:ncol(results$factor.summary)] = apply(results$factor.summary[,3:ncol(results$factor.summary)], 2, f)
	                        
	                        ### loop through all rows in summary
	       for (i in 3:(nrow(results$factor.summary)+2)){
				  	table$addRow(rowKey = i, values=list(
					    var=paste0("", as.character(results$factor.summary$variables[i-2])),
					    levels=results$factor.summary$levels[i-2],
						means = results$factor.summary$estimate[i-2],
						lower = results$factor.summary$lower[i-2],
						upper = results$factor.summary$upper[i-2]			
					))
				}  
				
			} 

			m = if (has_factors) nrow(results$factor.summary) + 2L else 0L
			
			####  if there are numbers, report those results.... 
			if (has_numbers){
							
				#### prepoulate first row with label
				table$addRow(rowKey=m+1, values=list(
					var = "Numeric Variables (Estimates reported are slopes/intercepts)",
					levels="",
					means="",
					lower="",
					upper=""
				))
							
				rows.tot = m + 2L

				rows.all = seq(from=rows.tot, to=(rows.tot + nrow(results$numbers.summary) - 1))
				#table2 = self$results$glmnum				
	            results$numbers.summary[,3:ncol(results$numbers.summary)] = apply(results$numbers.summary[,3:ncol(results$numbers.summary)], 2, round, digits=2)
	            results$numbers.summary[,3:ncol(results$numbers.summary)] = apply(results$numbers.summary[,3:ncol(results$numbers.summary)], 2, f)			
				i = 1
	     		for (j in rows.all){
	            
	            	if (i ==1){
	            		levs = "Intercept"
	            	} else {
	            		levs = paste0("Slope: ", as.character(results$numbers.summary$variables[i]))
	            	}
					table$addRow(rowKey = j, values=list(
					    var=levs,
					    levels = "", 
						means = results$numbers.summary$estimate[i],
						lower = results$numbers.summary$lower[i],
						upper = results$numbers.summary$upper[i]				
					))
				i = i+1
				} 
			}
						  	        
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        }},
        .comparisonplot = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot = tryCatch(jamovi_glm_comparison_plot(image$state), error=identity)
            if (inherits(plot, "error")) {
              image$setError(conditionMessage(plot))
              return(FALSE)
            }
            print(plot)
            TRUE
        },
		.plot = function(image, ...){
			if (self$options$line=="Loess"){line="loess"}
			if (self$options$line=="Regression"){line ="lm"}
			if (self$options$line=="Logistic"){line ="logistic"}
						
            if (is.null(image$state))
                return(FALSE)
            se.type = unlist(strsplit(self$options$center," + ", fixed=T))[2]			
			formula = image$state$formula
			data = image$state$data
            mod = lm(formula, data=data)
            theme_set(theme_bw(base_size = 16))
            plot = jamovi_glm_model_plot(mod, se=self$options$se, method=line, spread=se.type)
			print(plot)
			TRUE
			},

		.assumpplot = function(image, ...){
            if (is.null(image$state))
                return(FALSE)
                
			formula = image$state$formula
			data = image$state$data
			# plot = plot(data$motivation, data$income, main=as.character(formula))
			# print(plot)
			# TRUE			
            mod = lm(formula, data=data)
            theme_set(theme_bw(base_size = 16))            
            plot = visualize(mod, plot="residuals") + 
            	theme(panel.background = element_rect(fill = "transparent",colour = NA), 
					plot.background = element_rect(fill = "white",colour = NA),             	
            		line = element_blank(), 
            		text = element_blank(), 
            		panel.border = element_blank())
            print(plot)
            TRUE
            },		
            					
			
			.diff = function(l){
				
				table <- self$results$diff

				for (i in 1:(nrow(l))){
						row = list('variables' = l$variables[i], 
								'comparison' = l$comparison[i], 
								'diff' = round(l$difference[i], digits=2), 
								'lower' = round(l$lower[i], digits=2), 
								'upper' = round(l$upper[i],  digits=2), 																					
								"cohensd" = round(l$cohens.d[i], digits=2))
						if (is.na(row$variables)){
							row$variables = ""
						}		
					table$addRow(rowKey=i, values=row)
				}

			},		
			
			.rsq = function(l, preds){
				
				if (length(preds)>0){
				table <- self$results$rsq
				
				for (i in 1:(length(l$semi.p)+1)){
					if (i == 1){
						row = list('var' = 'model', 'Estimate' = l$rsq[1])
					} else {
						row = list('var' = names(l$semi.p)[i-1], 'Estimate' = l$semi.p[i-1])
					}
					table$addRow(rowKey=i, values=row)
				}
				
				if (!is.na(l$correlation)){
					table$addRow(rowKey=i+1, values=list('var' = "Correlation coefficient", 'Estimate' = l$correlation))
				}
			}}
		
			


       # head(exercise_data)
        
        )
)
