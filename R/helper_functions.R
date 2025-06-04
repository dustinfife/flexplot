return_factors_names = function(model) {
  
  terms = remove_interaction_terms(model)
  d = extract_data_from_fitted_object(model)
  d %>% select(-where(is.numeric)) %>%  names
}

identify_method = function(data, outcome, axis, method=NULL) {
  # histograms/barcharts
  if (axis[1] == "1") return("loess")
  if (!is.null(method)) return(method) 
  # association plot
  if (check.non.number(data[,axis[1]])) return("loess")
  # logistic
  if (length(unique(data[,outcome]))==2) return("logistic")
   
  return("loess")
}

#' Create a 1080p plot for YouTube
#'
#' @param p the ggplot object
#' @param name the name of the file (excluding the ".jpg")
#' @param path the path to the folder
#'
#' @export
youtube_plot = function(p, name="plot", path=""){
  require(extrafont)
  loadfonts(quiet=TRUE)
  p = p + theme(text=element_text(size=18, family="Moon Flower Bold"))
  file_path = ifelse(path=="", paste0(name, ".jpg"), paste0(path, "/", name, ".jpg"))
  ggsave(file_path, plot=p, height = 1080/150, width=1920/150, dpi=150, units = "in")
}