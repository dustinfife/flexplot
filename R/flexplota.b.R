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
		
			#### run if they do univariate stuff		
			} else if (length(self$options$out)>0 & length(self$options$preds)==0 & length(self$options$given)==0){
		        	#### write formula for flexplot
		            formula <- paste0(self$options$out, "~1")
		            formula <- as.formula(formula)
	
					output = list(formula=formula, data=self$data)
					image <- self$results$plot
					image$setState(output)
			} #else if (length(self$options$out)>0 & length(self$options$preds)>1 & length(self$options$given)>0 & self$options$ghost==TRUE){
				#	jmvcore::reject("Sorry! You can't do ghost lines when you have two variables in the 'Predictor variables' box. Try putting one in the 'Paneled variables' box.")
			#}
			return(formula)
		}
		, 
			
		.plot = function(image, ...){
       		
       		#### change case so line type can be read in
			if (self$options$line=="Loess"){line="loess"}
			if (self$options$line=="Regression"){line ="lm"}
			if (self$options$line=="Logistic"){line ="logistic"}
			if (self$options$line=="Polynomial"){line ="polynomial"}
			if (self$options$line=="Cubic"){line ="cubic"}	
			if (self$options$line=="Robust"){line ="rlm"}				
			if (self$options$sample==100){samp = Inf} else { samp = self$options$sample*.01*nrow(image$state$data)}					
			#### record related = T if the conditions are met
			related = FALSE
			if (self$options$diff==TRUE & length(self$options$preds)==1){
				if (length(unique(image$state$data[,self$options$preds]))==2 & 
						table(image$state$data[,self$options$preds])[1]==table(image$state$data[,self$options$preds])[2]){
							related = TRUE
				}	
			}
								
            if (is.null(image$state))
                return(FALSE)
            se.type = unlist(strsplit(self$options$center," + ", fixed=T))[2]			
			formula = image$state$formula
			data = image$state$data
			save(self, data, formula, file="/Users/fife/Dropbox/checkme.Rdat")		

				### ADDED VARIABLE PLOT
				### if they choose to residualize it
			if ((length(self$options$given) + length(self$options$preds))>0 & self$options$resid==TRUE) {
            	p = added.plot(formula, data=data, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha*.01, sample = samp, jitter=c(self$options$jittx, self$options$jitty), bins=self$options$bins, suppress_smooth=self$options$suppr, related=related)	


	    		### THIRD EYE
			} else if ((length(self$options$preds) + length(self$options$given))>1 & self$options$thirdeye==T){
				perms = ifelse((length(self$options$preds) + length(self$options$given)>2), 1:4, c(1,3))
				p = third.eye(formula, data=data, fixed.positions=NULL, which.perms=1:perms, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha*.01, ghost.line="gray", sample = samp, jitter=c(self$options$jittx, self$options$jitty),suppress_smooth=self$options$suppr, bins=self$options$bins) 

				#### GHOST LINES
            } else if (length(self$options$given)>0 & self$options$ghost==TRUE){
	            p = flexplot(formula, data=data, se=self$options$se,spread=se.type, method=line, alpha = self$options$alpha*.01, ghost.line="gray", sample = samp, jitter=c(self$options$jittx, self$options$jitty), bins=self$options$bins,suppress_smooth=self$options$suppr, related=related) 
    formula = weight.loss~gender


            } else {        	

	            	p = flexplot(formula, data=data, se=self$options$se, spread=se.type, method=line,  alpha = self$options$alpha*.01, sample = samp, jitter=c(self$options$jittx, self$options$jitty),suppress_smooth=self$options$suppr, bins=self$options$bins, related=related) #+ 
	            	#	theme_bw(base_size = 16) +
	            	#	theme(plot.background = element_rect(fill = "transparent",colour = NA), panel.background = element_rect(fill = "transparent",colour = NA))
	        }		
            #plot = added.plot(conscientiousness~communication + gender, data=relationship_satisfaction)
            #### find out where (if) geom_summary is
            #save(self, data, p, se.type, file="/Users/fife/Dropbox/checkme.Rdat")		
            #plot = flexplot(weight.loss~therapy.type, data=exercise_data, related=T)
            geoms = sapply(p$layers, function(x) class(x$geom)[1])
            if (self$options$plmethod != "Jittered-density plot" & "GeomErrorbar" %in% geoms & self$options$thirdeye==F){

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
            ### choose plot type
            #if (!is.numeric(data[,self$options$preds[1]]) & )
			print(p)
			TRUE
		}
		
				
))

#fifer2::clear()
#load("/Users/fife/Dropbox/checkme.Rdat")
#ls()
#se.type
#head(data)
#self$options$center
#formula = weight.loss~therapy.type

#p$layers