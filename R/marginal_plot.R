#' Create "margin plots"
#'
#' This function takes as an argument a regular flexplot graphic that has panels, then plots the "marginal" relationships for the panels. 
#' This makes it easier to see the average effect across the rows (or columns). 
#' @param p A flexplot graphic that has paneled variables. 
#' @param columns Logical. Should column panels be plotted?
#' @param rows Logical. Should row panels be plotted? 
#' @param grand_mean Logical. Stating TRUE will generate a simple bivariate plot between X and Y
#'
#' @return A graphic with marginal plots
#' @export
#'
#' @examples
#' p = flexplot(weight.loss~motivation | gender + rewards, data=exercise_data)
#' margin_plot(p)
#' margin_plot(p, rows=FALSE)
marginal_plot = function(p, columns=TRUE, rows=TRUE, grand_mean=TRUE) {

  # return paneled variables
  paneled_variables = return_panel_vars(p$formula)
  
  # extract the dataset
  data = p$data
  
  # make new formulae
  variables = all.vars(p$formula)
  dv = variables[1]
  x = variables[2]
  if (columns) formula_cols = make_paneled_formula(dv, x, panel=variables[1])
  if (rows & length(variables)>1) row_cols = make_paneled_formula(dv, x, panel=variables)
  
  # convert paneled variables to "_binned" version
  paneled_variables_binned = replace_text_with_binned(data, paneled_variables)
  
  
  
  # make the plots
  if (length(paneled_variables)>1 & rows) {
    row_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
      facet_grid(as.formula(paste0(paneled_variables_binned[2], "~."))) +
      common_layers_margin_plot()
  } else {
    row_plot = plot_spacer()  
  }
  
  if (columns) {
    column_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
      facet_grid(as.formula(paste0("~",paneled_variables_binned[1]))) +
      common_layers_margin_plot() 
  } else {
    column_plot = plot_spacer()
  }
  
  if (grand_mean) {
    grand_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
        common_layers_margin_plot() 
  } else {
    grand_plot = plot_spacer()
  }
require(patchwork)
  (column_plot + grand_plot + plot_layout(widths = c(4,1))) /
    (p + row_plot + plot_layout(widths = c(4,1))) + plot_layout(heights=c(1,4))
    
}




make_paneled_formula = function(dv, x, col=NULL, panel) {
  coltext = ifelse(is.null(col), "", paste0("+", col, collapse = ""))
  formula(paste0(dv, "~", x, "", coltext, "|", paste0(panel, collapse="+")))
}

common_layers_margin_plot = function() {
  layers = list(geom_smooth(method="lm", formula = y~x, se=F),
                
                theme_bw(),
                theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                      axis.text.y = element_blank(), axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.ticks.y = element_blank(),
                      plot.background = element_rect(fill = 'white', linetype = 'solid', colour = 'white')))
  return(layers)
}

replace_text_with_binned = function(data, paneled_variables) {

  binned_names = paste0(paneled_variables, "_binned")
  which_are_binned = binned_names %in% names(data)
  return_names = paneled_variables
  return_names[which_are_binned] = binned_names[which_are_binned]
  return_names
}

return_panel_vars = function(formula) {
  
  # convert formula to text
  split_string = strsplit(paste(formula)[3], "|", fixed=T)[[1]]
  
  # make sure they have panels
  if (length(split_string)<2) stop("You can't do a margin plot without having a paneled flexplot.")
  
  # extract first variable
  split_panels = trimws(strsplit(split_string[2], "+", fixed=T)[[1]])
  return(split_panels)
}


