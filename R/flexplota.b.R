
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
            		
            	#### first create initial plot
            	p = flexplot(formula, data=data, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha, data_output=T) 
            	plot = p$plot
            	
            	#### now add the ghost line (have to do it separately because of environment conflicts)
            	#### first get ghost.reference
            	k = p$data
            	ghost.reference = list()
            	for (i in 1:length(self$options$given)){
            		condition = k[1,self$options$given[i]]
            		k = k[k[,self$options$given[i]]==condition,]
            	}
				
			
				### identify which variables are in the given category
				ghost.given = self$options$given
				
				if (is.numeric(data[,self$options$preds[1]])){
					g0 = ggplot(data=k, aes_string(x=self$options$preds[1], y=self$options$out))+geom_smooth(method=line, se=self$options$se)
				} else {
					g0 = ggplot(data=k, aes_string(x=self$options$preds[1], y=self$options$out))+p$summary1 + p$summary2 + p$sum.line
				}
				d_smooth = ggplot_build(g0)$data[[1]]; 
				save(d_smooth, k, data, file="/Users/amber/Dropbox/jamovitest.rda")				
				### rename columns
				names(d_smooth)[names(d_smooth)=="x"] = as.character(self$options$preds[1]); names(d_smooth)[names(d_smooth)=="y"] = self$options$out; 
	

	
				## add line to existing plot   
	            plot = plot + theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
	        	    	theme_bw(base_size = 16) +
						geom_line(data=d_smooth, aes_string(x=self$options$preds[1], y= self$options$out), color="gray")

            } else {	
            	plot = flexplot(formula, data=data, se=self$options$se, spread=se.type,method=line,  alpha = self$options$alpha) + 
	            		theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
	            		theme_bw(base_size = 16)
            }
			print(plot)
			TRUE
		}		
))