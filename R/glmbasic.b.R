
# This file is a generated template, your changes will not be overwritten

glmbasicClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "glmbasicClass",
    inherit = glmbasicBase,
    private = list(
        .run = function() {
        	
        	if (length(self$options$out)>0 & length(self$options$preds)>0){
        		
        	# if (length(self$options$preds)==0){
        		# formula = paste0(self$options$out, "~1")
        	# } else {
        	#### write formula for glinmod
	            formula <- jmvcore::constructFormula(self$options$out, self$options$preds)
            	formula <- as.formula(formula)
#			}        
        	#### output results
            results <- glinmod::estimates(lm(formula, self$data))
            
         	

			#glinmod::estimates(lm(weight.loss~gender, data=exercise_data))
        	#### save formula/dataset to a file (to be used for plotting)
        	if (length(self$options$preds)>0){
				output = list(formula=formula, data=self$data)
				image <- self$results$plot
				image$setState(output)
				
				image2 <- self$results$assumpplot
				image2$setState(output)			
			}

			#### prepare r square output
			rsq_out = list(rsq = results$r.squared, semi.p = results$semi.p, correlation=results$correlation)
			private$.rsq(rsq_out, preds = self$options$preds)	
			

									

			#### prepopulate table
            table = self$results$glmcat			
            
            #### format factor summary to look nice
			f = function(x){ x[is.na(x)] = "-"; x}
			

           
			#### if there are factors, report those results....           
			if (!is.na(results$factor.summary)){        
				
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

			m = nrow(table)
			
			####  if there are numbers, report those results.... 
			if (!is.na(results$numbers.summary)){
							
				#### prepoulate first row with label
				table$addRow(rowKey=m+1, values=list(
					var = "Numeric Variables (Estimates reported are slopes/intercepts)",
					levels="",
					means="",
					lower="",
					upper=""
				))
							
				rows.tot = ifelse(is.na(results$factor.summary)[1], 2, nrow(results$factor.summary)+2)

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
		.plot = function(image, ...){
            if (is.null(image$state))
                return(FALSE)
            se.type = subsetString(self$options$center," + ", position=2)   			
			formula = image$state$formula
			data = image$state$data
            mod = lm(formula, data=data)
            plot = visualize(mod, plot="bivariate", se=self$options$se, method=self$options$line, spread=se.type)	+ theme(plot.background = element_rect(fill = "white", colour = NA)) 
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
            plot = visualize(mod, plot="residuals") + theme(plot.background = element_rect(fill = "white", colour = NA))
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
				
				#if (length(l$correlation)>0){
					table$addRow(rowKey=i+1, values=list('var' = "Correlation Coefficient", 'Estimate' = l$correlation))
				#}
			}}
		
			


       # head(exercise_data)
        
        )
)
