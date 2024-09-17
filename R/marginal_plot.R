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
#' @import patchwork
#'
#' @examples
#' p = flexplot(weight.loss~motivation | gender + rewards, data=exercise_data)
#' marginal_plot(p)
#' marginal_plot(p, rows=FALSE)
marginal_plot = function(p, columns=TRUE, rows=TRUE, grand_mean=TRUE) {
#browser()
  # return paneled variables
  paneled_variables = return_panel_vars(p$formula)
  
  # extract the dataset
  data = p$data
  #ggplot_build(p)$data[[2]]
  
  
  # make new formulae
  variables = all.vars(p$formula)
  dv = variables[1]
  x = variables[2]
  if (columns) formula_cols = make_paneled_formula(dv, x, panel=variables[1])
  if (rows & length(variables)>1) row_cols = make_paneled_formula(dv, x, panel=variables)
  
  # convert paneled variables to "_binned" version
  paneled_variables_binned = replace_text_with_binned(data, paneled_variables)
  
  # get the smoothing method
  plot_method = extract_plot_method(p)
  
  # make the plots
  if (length(paneled_variables)>1 & rows) {
    row_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
      coord_cartesian(ylim=c(min(data[,dv]), max(data[,dv]))) + 
      geom_smooth(method=plot_method$method, formula = plot_method$formula, se=plot_method$se) + 
      facet_grid(as.formula(paste0(paneled_variables_binned[2], "~."))) +
      common_layers_margin_plot()
  } else {
    row_plot = plot_spacer()  
  }
  
  if (columns) {
    column_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
      coord_cartesian(ylim=c(min(data[,dv]), max(data[,dv]))) +       
      geom_smooth(method=plot_method$method, formula = plot_method$formula, se=plot_method$se) +       
      facet_grid(as.formula(paste0("~",paneled_variables_binned[1]))) +
      common_layers_margin_plot() 
  } else {
    column_plot = plot_spacer()
  }
  
  if (grand_mean) {
    grand_plot = ggplot(data=data, aes_string(x=x, y=dv)) +
        coord_cartesian(ylim=c(min(data[,dv]), max(data[,dv]))) +       
        geom_smooth(method=plot_method$method, formula = plot_method$formula, se=plot_method$se) +       
        common_layers_margin_plot() 
  } else {
    grand_plot = plot_spacer()
  }

  (column_plot + grand_plot + plot_layout(widths = c(4,1))) /
    (p + row_plot + plot_layout(widths = c(4,1))) + plot_layout(heights=c(1,4))
    
}




make_paneled_formula = function(dv, x, col=NULL, panel) {
  coltext = ifelse(is.null(col), "", paste0("+", col, collapse = ""))
  formula(paste0(dv, "~", x, "", coltext, "|", paste0(panel, collapse="+")))
}

common_layers_margin_plot = function() {
  layers = list(
                
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

#plot = flexplot(weight.loss~health | motivation + muscle.gain, data=exercise_data, method="lm")
#extract_plot_method(plot)
# thanks to https://stackoverflow.com/questions/40854225/how-to-identify-the-function-used-by-geom-smooth
#' @importFrom mgcv gam
extract_plot_method = function(plot) {
  layer.data <- plot$layers[[2]]$layer_data(plot$data)
  layout <- create_layout_flex(plot$facet, plot$coordinates)
  data <- layout$setup(list(layer.data), plot$data, plot$plot_env)
  data[[1]] <- plot$layers[[2]]$compute_aesthetics(data[[1]], plot)
  scales <- plot$scales
  data[[1]] <- scales_transform_df_flex(scales = scales, df = data[[1]])
  layout$train_position(data, scales$get_scales("x"), scales$get_scales("y"))
  data <- layout$map_position(data)[[1]]
  
  # set up stat params (e.g. replace "auto" with actual method / formula)
  stat.params <- suppressMessages(
    plot$layers[[2]]$stat$setup_params(data = data, 
                                       params = plot$layers[[2]]$stat_params)
  )
  
  # reverse the last step in setup_params; we don't need the actual function
  # for mgcv::gam, just the name
  if(identical(stat.params$method, mgcv::gam)) stat.params$method <- "gam"
  
  return(stat.params)
}


create_layout_flex = function (facet = FacetNull, coord = CoordCartesian) 
{
  ggplot2::ggproto(NULL, Layout, facet = facet, coord = coord)
}
empty_flex = function (df) 
{
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 
}

scales_transform_df_flex = function (scales, df) 
{
  if (empty_flex(df) || length(scales$scales) == 0) 
    return(df)
  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)), 
                        recursive = FALSE)
  new_data_frame_flex(c(transformed, df[setdiff(names(df), names(transformed))]))
}
new_data_frame_flex = function (x = list(), n = NULL) 
{
  if (length(x) != 0 && is.null(names(x))) {
    rlang::abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 
      0
    else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) 
      next
    if (lengths[i] != 1) {
      rlang::abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}
