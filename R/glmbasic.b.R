
# This file is a generated template, your changes will not be overwritten

jamovi_glm_model_plot = function(model, se, formula=NULL, data=model$model, ...) {
  # A square-only fitted frame omits raw x; prepare its layout from input data.
  if (is.null(formula) && length(setdiff(all.vars(stats::formula(model)),names(model$model)))) {
    formula = jamovi_model_layout(list(model),data)
  }
  # visualize.lm uses a descriptive added-variable plot for larger models.
  if (is.null(formula) && length(all.vars(stats::formula(model))[-1]) > 4L) {
    return(visualize(model, plot="model", se=se, ...))
  }
  plot = if (is.null(formula)) visualize(model, plot="model", report.se=se, ...) else
    compare_fits(formula, data=data, model1=model, report.se=se, ...)
  jamovi_model_intervals(plot, se=se)
}

# Coefficients are model-matrix quantities, independent of predictor summaries.
jamovi_lm_display = function(model, data, options) {
  layout = NULL
  if (length(c(options$plotX,options$plotPanels,options$plotGroup)))
    layout = jamovi_model_layout(list(model),data,options$plotX,options$plotPanels,options$plotGroup)
  jamovi_glm_model_plot(model,se=options$se,formula=layout,data=data)
}

jamovi_lm_label_info = function(model, options, comparison=FALSE, formula=NULL) {
  explicit = length(c(options$plotX,options$plotPanels,options$plotGroup))>0L
  added = !comparison && !explicit && length(all.vars(stats::formula(model))[-1])>4L
  list(context=list(output=if (comparison) "comparison" else "model",outcome=all.vars(stats::formula(model))[1],
    meaning=if (added) "added-variable" else "response"),
    layout=if (comparison) list(x=flexplot_axis_given(formula)$axis[1]) else list(x=options$plotX))
}

jamovi_lm_coefficients = function(table, model, includeP=FALSE) {
  table$deleteRows()
  jamovi_result_reset_status(table)
  coefficients = stats::coef(model)
  inference = summary(model)$coefficients
  index = match(names(coefficients), rownames(inference))
  standard_error = sqrt(diag(stats::vcov(model)))[names(coefficients)]
  df = stats::df.residual(model)
  critical = if (df > 0) stats::qt(.975,df) else NA_real_
  values = data.frame(var=names(coefficients), means=unname(coefficients),
    se=unname(standard_error), lower=unname(coefficients-critical*standard_error),
    upper=unname(coefficients+critical*standard_error),
    t=unname(inference[index,3]), p=unname(inference[index,4]),
    status=ifelse(is.na(coefficients),"Aliased (not estimable)",""))
  table$getColumn("p")$setVisible(includeP)
  table$getColumn("status")$setVisible(anyNA(coefficients))
  table$setNote("aliased",if (anyNA(coefficients))
    "Aliased coefficients are not uniquely estimable; their inference cells are blank." else NULL)
  table$setNote("intervals",if (df > 0)
    paste0("95% t confidence intervals; residual df = ",df,".") else
    "Residual degrees of freedom are insufficient for coefficient inference.")
  jamovi_glm_table_rows(table,values)
}

glmbasicClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "glmbasicClass",
    inherit = glmbasicBase,
    private = list(
      
        .run = function() {
          jamovi_plot_sync_labels(self$options)
          jamovi_plot_sync_labels(self$options,rows="comparisonPlotLabels",retained="comparisonPlotLabelStore")
          self$results$comparison$deleteRows()
          # Replace rows on every model change, including failed specifications.
          for (name in c("glmcat","factorMeans","diff","rsq")) {
            table = self$results[[name]]
            jamovi_result_clear(table)
          }
          self$results$factorMeans$setVisible(FALSE)
          self$results$diff$setVisible(FALSE)
          if (!length(self$options$out) || !length(self$options$preds)) return(invisible(NULL))
          data = self$data
          if (self$options$compare) {
            comparison = tryCatch(jamovi_glm_comparison(self$options,data),error=identity)
            if (inherits(comparison,"error")) {
              self$results$comparison$setError(conditionMessage(comparison))
              self$results$comparisonplot$setError(conditionMessage(comparison))
              return(invisible(NULL))
            }
            self$results$comparisonplot$setState(comparison)
            jamovi_glm_comparison_table(self$results$comparison,comparison)
            model = comparison$model1
            formula = stats::formula(model)
            data = comparison$data
          } else {
            formula = tryCatch(jamovi_lm_formula(self$options,data),error=identity)
            if (inherits(formula,"error")) {
              self$results$glmcat$setError(conditionMessage(formula))
              return(invisible(NULL))
            }
            model = tryCatch(stats::lm(formula,data=data),error=identity)
            if (inherits(model,"error")) {
              self$results$glmcat$setError(paste("The primary model could not be fitted:",conditionMessage(model)))
              return(invisible(NULL))
            }
          }
          jamovi_lm_coefficients(self$results$glmcat,model,self$options$includeP)
          output = list(formula=formula,data=data)
          self$results$plot$setState(output)
          self$results$assumpplot$setState(output)
          if (self$options$graphic) {
            previous_theme = theme_set(theme_bw(base_size=16))
            plot = suppressWarnings(tryCatch(jamovi_lm_display(model,data,self$options),
              error=function(error) NULL,finally=theme_set(previous_theme)))
            if (!is.null(plot)) {
              info = jamovi_lm_label_info(model,self$options)
              jamovi_plot_sync_labels(self$options,plot,info$layout,info$context)
            }
          }
          if (self$options$compare) {
            plot = suppressWarnings(tryCatch(jamovi_glm_comparison_plot(comparison),error=function(error) NULL))
            if (!is.null(plot)) {
              info = jamovi_lm_label_info(model,self$options,TRUE,comparison$formula)
              jamovi_plot_sync_labels(self$options,plot,info$layout,info$context,
                rows="comparisonPlotLabels",retained="comparisonPlotLabelStore")
            }
          }
          # A failed supplementary summary must not remove valid coefficients.
          results = tryCatch(estimates.lm(model,mc=FALSE),error=identity)
          if (inherits(results,"error")) {
            self$results$glmcat$setNote("summaries",paste("Supplementary summaries are unavailable:",conditionMessage(results)))
            return(invisible(NULL))
          }
          if (is.data.frame(results)) return(invisible(NULL))
          private$.rsq(list(rsq=results$r.squared,semi.p=results$semi.p,
            correlation=results$correlation),preds=self$options$preds)
          if (!is.data.frame(results$factor.summary) || !nrow(results$factor.summary)) return(invisible(NULL))
          values = results$factor.summary
          jamovi_glm_table_rows(self$results$factorMeans,data.frame(
            var=as.character(values$variables),levels=values$levels,
            means=round(values$estimate,2),lower=round(values$lower,2),upper=round(values$upper,2)))
          self$results$factorMeans$setNote("reference",
            "Conditional predictions: other numeric predictors are held at their means and categorical predictors at their modes. These are not model coefficients.")
          self$results$factorMeans$setVisible(self$options$estimates)
          private$.diff(results$difference.matrix)
        },
        .comparisonplot = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot = tryCatch(jamovi_glm_comparison_plot(image$state), error=identity)
            if (inherits(plot, "error")) {
              image$setError(conditionMessage(plot))
              return(FALSE)
            }
            info = jamovi_lm_label_info(image$state$model1,self$options,TRUE,image$state$formula)
            print(jamovi_plot_apply_labels(plot,self$options$comparisonPlotLabels,info$context,info$layout))
            TRUE
        },
        .plot = function(image, ...){
            if (is.null(image$state)) return(FALSE)
            # Legacy line/center options remain readable but do not select a
            # second smoother or observed-group summary for a fitted model.
			formula = image$state$formula
			data = image$state$data
            mod = lm(formula, data=data)
            theme_set(theme_bw(base_size = 16))
            plot = tryCatch(jamovi_lm_display(mod,data,self$options),error=identity)
            if (inherits(plot, "error")) {
              image$setError(conditionMessage(plot))
              return(FALSE)
            }
            info = jamovi_lm_label_info(mod,self$options)
			print(jamovi_plot_apply_labels(plot,self$options$plotLabels,info$context,info$layout))
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
				
				table = self$results$diff
        if (!is.data.frame(l) || !nrow(l)) return(invisible(NULL))
        table$setVisible(self$options$estimates)
        adjustments = attr(l, "adjustments")
        table$setNote("intervals", paste0("95% confidence intervals: ",
          paste(paste(names(adjustments), adjustments, sep=": "), collapse="; "),
          ". Adjusted intervals cover the estimable pairwise family within each factor. Other numeric predictors are held at their means and categorical predictors at their modes."))


				for (i in seq_len(nrow(l))){
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
        table$deleteRows()
        table$setNote("hierarchy", attr(l$semi.p,"note"))
        if (any(endsWith(names(l$semi.p)," (combined polynomial)") |
                endsWith(names(l$semi.p)," (incremental curvature)"))) {
          table$setNote("polynomial", "Curvature is incremental to the linear term; combined polynomial rows include linear and curvature contributions.")
        } else table$setNote("polynomial", NULL)
				
				for (i in 1:(length(l$semi.p)+1)){
					if (i == 1){
						row = list('var' = 'model', 'Estimate' = l$rsq[1])
					} else {
						row = list('var' = names(l$semi.p)[i-1], 'Estimate' = l$semi.p[i-1])
					}
        if (is.na(row$Estimate)) row$Estimate = NULL
					table$addRow(rowKey=i, values=row)
				}
				
				if (!is.na(l$correlation)){
					table$addRow(rowKey=i+1, values=list('var' = "Correlation coefficient", 'Estimate' = l$correlation))
				}
			}}
		
			


       # head(exercise_data)
        
        )
)
